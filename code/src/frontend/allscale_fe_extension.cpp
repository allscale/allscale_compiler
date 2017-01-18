
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <limits>
#include <regex>

#include <boost/algorithm/string.hpp>

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/tu/ir_translation_unit_io.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/iterator_utils.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/allscale_utils.h"

namespace iu = insieme::utils;

namespace allscale {
namespace compiler {
namespace frontend {

	boost::optional<std::string> AllscaleExtension::isPrerequisiteMissing(insieme::frontend::ConversionSetup& setup) const {
		if(!setup.hasExtension<insieme::frontend::extensions::InterceptorExtension>()) {
			return std::string("AllscaleExtension requires the InterceptorExtension to be loaded");
		}
		//TODO: ensure that we are running before the interceptor extension
		return {};
	}

	namespace {

		core::ExpressionPtr removeUndesiredRefCasts(const core::ExpressionPtr& input) {
			auto& refExt = input->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefCast(input) || refExt.isCallOfRefKindCast(input)) {
				return core::analysis::getArgument(input, 0);
			}
			return input;
		}

		core::ExpressionPtr derefOrDematerialize(const core::ExpressionPtr& argExprIn) {
			core::IRBuilder builder(argExprIn->getNodeManager());

			auto argExpr = removeUndesiredRefCasts(argExprIn);

			if(auto call = argExpr.isa<core::CallExprPtr>()) {
				if(core::lang::isPlainReference(call->getType())) {
					auto rawCallType = core::analysis::getReferencedType(call->getType());
					return builder.callExpr(rawCallType, call->getFunctionExpr(), call->getArgumentDeclarations());
				}
			}
			auto exprType = argExpr->getType();
			if(core::analysis::isRefType(exprType)) {
				return builder.deref(argExpr);
			}
			return argExpr;
		}
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TYPES

	namespace {
		/// Name of placeholder generic type generated for dependent types for temporary translation
		static const char* ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER = "__AllScale__Dependent_AutoType";

		/// Mapping specification from C++ to IR types used during type translation
		const static std::map<std::string, std::string> typeMap = {
			// callables
			{ "allscale::api::core::fun_def", "recfun<TEMPLATE_T_1,TEMPLATE_T_0>" },
			{ "allscale::api::core::rec_defs", "('TEMPLATE_T_0...)" },
			{ "allscale::api::core::detail::prec_operation", "recfun<TEMPLATE_T_0,TEMPLATE_T_1>" },
			{ "allscale::api::core::detail::callable", "TUPLE_TYPE_0<TUPLE_TYPE_0<('TEMPLATE_T_0...)>>" },
			// completed tasks
			{ "allscale::api::core::detail::completed_task", "completed_task<TEMPLATE_T_0>" },
			// treetures
			{ "allscale::api::core::impl::.*::treeture", "treeture<TEMPLATE_T_0,t>" },
			{ "allscale::api::core::impl::.*::.*unreleased_treeture", "treeture<TEMPLATE_T_0,f>" },
		};

		/// Extract type argument #id from a C++ template instantiation declared by recordDecl
		core::TypePtr extractTemplateTypeArgument(const clang::RecordDecl* recordDecl, int id, insieme::frontend::conversion::Converter& converter) {
			if(auto specializedDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(recordDecl)) {
				int i = 0;
				for(auto a : specializedDecl->getTemplateArgs().asArray()) {
					if(a.getKind() == clang::TemplateArgument::Type) {
						if(i == id) {
							return converter.convertType(a.getAsType());
						}
						i++;
					}
				}
			}
			return {};
		}
		/// Extract type arguments at #id from a C++ template type argument pack in a recordDecl
		core::TypeList extractTemplateTypeArgumentPack(const clang::RecordDecl* recordDecl, int id, insieme::frontend::conversion::Converter& converter) {
			core::TypeList ret;
			if(auto specializedDecl = llvm::dyn_cast<clang::ClassTemplateSpecializationDecl>(recordDecl)) {
				int i = 0;
				for(auto a : specializedDecl->getTemplateArgs().asArray()) {
					if(a.getKind() == clang::TemplateArgument::Pack) {
						if(i == id) {
							for(auto inner : a.getPackAsArray()) {
								if(inner.getKind() == clang::TemplateArgument::Type) {
									ret.push_back(converter.convertType(inner.getAsType()));
								}
							}
							break;
						}
						i++;
					}
				}
			}
			return ret;
		}
	}

	namespace detail {

		/// Implementation of these type mapping specifications
		class TypeMapper {
			std::map<std::string, core::TypePtr> typeIrMap;

			using CodeExtractor = std::function<core::TypePtr(const clang::RecordDecl* recordDecl, const core::TypePtr& irType)>;
			std::map<core::TypePtr, CodeExtractor> placeholderReplacer;

			const unsigned MAX_MAPPED_TEMPLATE_ARGS = 8;
			const char* TEMPLATE_ARG_PATTERN = "TEMPLATE_T_%u";

		public:
			TypeMapper(insieme::frontend::conversion::Converter& converter) {

				auto& allscaleExt = converter.getNodeManager().getLangExtension<lang::AllscaleModule>();

				// generate type to ir map from string-based type map
				for(auto stringMapping : typeMap) {
					auto ir = converter.getIRBuilder().parseType(stringMapping.second, allscaleExt.getSymbols());
					typeIrMap[stringMapping.first] = ir;
				}

				// generate the template placeholder replacers to be applied on mapped IR
				for(unsigned i = 0; i < MAX_MAPPED_TEMPLATE_ARGS; ++i) {
					std::string name = ::format(TEMPLATE_ARG_PATTERN, i);
					// single argument
					auto type = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[type] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return extractTemplateTypeArgument(recordDecl, i, converter);
					};
					// variadic argument
					name = ::format("('%s...)", name);
					auto variadicType = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[variadicType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return converter.getIRBuilder().tupleType(extractTemplateTypeArgumentPack(recordDecl, i, converter));
					};
					// type extraction from tuple types
					name = ::format("TUPLE_TYPE_%d", i);
					auto tupleTypeExtractorType = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[tupleTypeExtractorType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return irType.as<core::GenericTypePtr>()->getTypeParameter(0).as<core::TupleTypePtr>()->getElement(i);
					};
				}
			}

			core::TypePtr apply(const clang::Type* type) {
				core::TypePtr ret {};
				// we are only interested in Record Types for now
				if(auto recordType = llvm::dyn_cast<clang::RecordType>(type)) {
					auto recordDecl = recordType->getDecl();
					auto name = recordDecl->getQualifiedNameAsString();

					// replace if we have a map entry matching this name
					for(const auto& mapping : typeIrMap) {
						std::regex pattern(mapping.first);
						if(std::regex_match(name, pattern)) {
							ret = mapping.second;
							core::IRBuilder builder(ret->getNodeManager());
							// replace all placeholders in generated IR type
							ret = core::transform::transformBottomUpGen(ret, [&](const core::TypePtr& typeIn) -> core::TypePtr {
								auto typeForMatching = typeIn;
								if(auto genTy = typeIn.isa<core::GenericTypePtr>()) {
									typeForMatching = builder.genericType(genTy->getName()->getValue());
								}
								if(::containsKey(placeholderReplacer, typeForMatching)) {
									return placeholderReplacer[typeForMatching](recordDecl, typeIn);
								}
								return typeIn;
							});
							break;
						}
					}
				}
				return ret;
			}
		};
		void TypeMapperDeleter::operator()(TypeMapper* tm) {
			delete tm;
		}
	}

	detail::TypeMapper& AllscaleExtension::getTypeMapper(insieme::frontend::conversion::Converter& converter) {
		if(!typeMapper) {
			typeMapper.reset(new detail::TypeMapper(converter));
		}
		return *typeMapper;
	}

	insieme::core::TypePtr AllscaleExtension::Visit(const clang::QualType& typeIn, insieme::frontend::conversion::Converter& converter) {
		const clang::Type* type = typeIn->getUnqualifiedDesugaredType();

		// Apply the type mapping specification table to record types
		auto mappedType = getTypeMapper(converter).apply(type);
		if(mappedType) {
			std::cout << "Mapped type to : " << *mappedType << std::endl;
			return mappedType;
		}

		// if the passed type is an AutoType and is dependent, we can't really translate it correctly.
		// we create a dummy replacement type to move forward in the translation and assert this replacement doesn't survive in the final IR
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			if(autoType->isDependentType()) { return converter.getIRBuilder().genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER); }
		}

		return{};
	}

	insieme::core::TypePtr AllscaleExtension::PostVisit(const clang::QualType& typeIn, const insieme::core::TypePtr& irType,
		insieme::frontend::conversion::Converter& converter) {

		return irType;
	}


	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// EXPRESSIONS

	namespace {

		using CallMapper = std::function<core::ExpressionPtr(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter)>;

		/// Utility for the specification of simple call mappings (C++ to IR)
		class SimpleCallMapper {
		  private:
			const string targetIRString;
			bool derefThisArg;

			core::ExpressionPtr buildCallWithDefaultParamConversion(const core::ExpressionPtr& callee, const clang::CallExpr* call,
			                                                        insieme::frontend::conversion::Converter& converter) {
				core::ExpressionList args;
				// if it was a member call, add the implicit this argument
				if(auto memCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(call)) {
					auto thisArg = converter.convertExpr(memCall->getImplicitObjectArgument());
					if(derefThisArg) thisArg = derefOrDematerialize(thisArg);
					args.push_back(thisArg);
				}
				// add normal arguments
				for(const auto& arg : call->arguments()) {
					args.push_back(converter.convertExpr(arg));
				}
				return converter.getIRBuilder().callExpr(callee, args);
			}

		  public:
			SimpleCallMapper(const string& targetIRString, bool derefThisArg = false) : targetIRString(targetIRString), derefThisArg(derefThisArg) {}

			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
				auto& allscaleExt = converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
				auto targetFun = converter.getIRBuilder().parseExpr(targetIRString, allscaleExt.getSymbols());

				return buildCallWithDefaultParamConversion(targetFun, call, converter);
			}
		};


		/// Mapping specification from C++ to IR used during call expression translation
		const static std::map<std::string, CallMapper> callMap = {
			{ "allscale::api::core::done", SimpleCallMapper("task_done") },
			{ "allscale::api::core::.*::completed_task<.*>::operator treeture", SimpleCallMapper("task_to_treeture") },
			{ "allscale::api::core::.*::completed_task<.*>::operator unreleased_treeture", SimpleCallMapper("task_to_unreleased_treeture") },
			{ "allscale::api::core::impl::.*treeture.*::wait", SimpleCallMapper("treeture_wait", true) },
			{ "allscale::api::core::impl::.*reference.*::wait", SimpleCallMapper("treeture_wait", true) },
		};

	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		expr->dumpColor();

		if(auto construct = llvm::dyn_cast<clang::CXXConstructExpr>(expr)) {
			auto retType = converter.convertType(expr->getType());
			if(lang::isTreeture(retType)) {
				return converter.convertExpr(construct->getArg(0));
			}
		}

		// we handle certain calls specially, which we differentiate by their callee's name
		if(auto call = llvm::dyn_cast<clang::CallExpr>(expr)) {
			auto decl = call->getCalleeDecl();
			if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
				auto name = funDecl->getQualifiedNameAsString();
				std::cout << "N: " << name << std::endl;

				for(const auto& mapping : callMap) {
					std::regex pattern(mapping.first);
					if(std::regex_match(name, pattern)) {
						std::cout << "Matched " << mapping.first << std::endl;
						return mapping.second(call, converter);
					}
				}

				if(name == "allscale::api::core::prec") {
				}
				if(name == "allscale::api::core::fun") {
				}
				if(name == "allscale::api::core::done") {
					//assert_eq(call->getNumArgs(), 1);
					//return lang::buildTreetureDone(converter.convertExpr(call->getArg(0)));
				}
				if(name == "allscale::api::core::combine") {
				}
			}
		}

		return nullptr;
	}

	insieme::core::ExpressionPtr AllscaleExtension::Visit(const clang::CastExpr* castExpr,
	                                                      insieme::core::ExpressionPtr& irExpr, insieme::core::TypePtr& irTargetType,
	                                                      insieme::frontend::conversion::Converter& converter) {

		std::cout << "!!\n";
		if(castExpr->getCastKind() == clang::CK_UncheckedDerivedToBase) {
			std::cout << "!! Casting CK_UncheckedDerivedToBase " << dumpColor(irExpr->getType());
			auto irSourceType = irExpr->getType();
			if(core::analysis::isRefType(irExpr)) irSourceType = core::analysis::getReferencedType(irSourceType);
			if(lang::isTreeture(irSourceType)) {
				std::cout << "!! Casting treeture\n";
				return irExpr;
			}
		}

		return nullptr;
	}


	insieme::core::ExpressionPtr AllscaleExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
		                                                      insieme::frontend::conversion::Converter& converter) {

		return irExpr;
	}

	std::pair<core::VariablePtr, core::ExpressionPtr> AllscaleExtension::PostVisit(const clang::VarDecl* varDecl, const core::VariablePtr& var,
		                                                                           const core::ExpressionPtr& varInit,
		                                                                           insieme::frontend::conversion::Converter& converter) {

		return {var, varInit};
	}

	insieme::core::tu::IRTranslationUnit AllscaleExtension::IRVisit(insieme::core::tu::IRTranslationUnit& tu) {

		return tu;
	}

	insieme::core::ProgramPtr AllscaleExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		core::IRBuilder builder(prog->getNodeManager());

		// temporarily dump the generated IR in a readable format
		dumpReadable(prog);

		// make sure that we don't have the dummy dependent type replacement type in the program anywhere anymore
		assert_eq(core::analysis::countInstances(prog, builder.genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER), false), 0);

		return prog;
	}

}
}
}

