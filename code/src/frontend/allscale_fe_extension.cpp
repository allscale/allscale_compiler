
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
#include "insieme/core/lang/list.h"
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

		core::FunctionTypePtr extractLambdaOperationType(const clang::Expr* clangExpr, insieme::frontend::conversion::Converter& converter, bool deref) {
			if(auto mat = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(clangExpr)) clangExpr = mat->GetTemporaryExpr();
			if(auto lambda = llvm::dyn_cast<clang::LambdaExpr>(clangExpr)) {
				auto ret = converter.convertType(lambda->getCallOperator()->getType()).as<core::FunctionTypePtr>();
				if(deref) {
					auto dereffedParamTypes = ::transform(ret->getParameterTypeList(), [](const core::TypePtr& t) {
						return core::analysis::isRefType(t) ? core::analysis::getReferencedType(t) : t;
					});
					ret = converter.getIRBuilder().functionType(dereffedParamTypes, ret->getReturnType(), ret->getKind());
				}
				return ret;
			}
			return {};
		}

		core::ExpressionPtr buildDependencyList(insieme::frontend::conversion::Converter& converter) {
			auto& allscaleExt = converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
			return converter.getIRBuilder().callExpr(allscaleExt.getDependencyAfter());
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
			{ "allscale::api::core::detail::callable<.*>::(Sequential|Parallel)Callable", "TUPLE_TYPE_0<('ENCLOSING_TEMPLATE_T_0...)>" },
			// completed tasks
			{ "allscale::api::core::detail::completed_task", "treeture<TEMPLATE_T_0,f>" },
			// treetures
			{ "allscale::api::core::impl::.*::treeture", "treeture<TEMPLATE_T_0,t>" },
			{ "allscale::api::core::impl::.*::.*unreleased_treeture", "treeture<TEMPLATE_T_0,f>" },
			// dependencies
			{ "allscale::api::core::dependencies", "dependencies" },
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
		/// Extract type argument #id from a C++ template instantiation declared by the type enclosing recordDecl
		core::TypeList extractEnclosingTemplateTypeArgumentPack(const clang::RecordDecl* recordDecl, int id, insieme::frontend::conversion::Converter& converter) {
			if(auto cxxRecordDecl = llvm::dyn_cast<clang::CXXRecordDecl>(recordDecl)) {
				auto enclosingRecordDecl = llvm::dyn_cast<clang::CXXRecordDecl>(cxxRecordDecl->getDeclContext());
				assert_true(enclosingRecordDecl) << "Enclosing context is not a CXXRecordDecl";
				return extractTemplateTypeArgumentPack(enclosingRecordDecl, id, converter);
			}
			return {};
		}
	}

	namespace detail {

		/// Implementation of these type mapping specifications
		class TypeMapper {
			std::map<std::string, core::TypePtr> typeIrMap;

			using CodeExtractor = std::function<core::TypePtr(const clang::RecordDecl* recordDecl, const core::TypePtr& irType)>;
			std::map<core::TypePtr, CodeExtractor> placeholderReplacer;

			const unsigned MAX_MAPPED_TEMPLATE_ARGS = 8;

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
					std::string name = ::format("TEMPLATE_T_%u", i);
					// single argument
					auto singleType = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[singleType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return extractTemplateTypeArgument(recordDecl, i, converter);
					};

					// variadic argument
					name = ::format("('%s...)", name);
					auto variadicType = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[variadicType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return converter.getIRBuilder().tupleType(extractTemplateTypeArgumentPack(recordDecl, i, converter));
					};

					// type extraction from tuple types
					name = ::format("TUPLE_TYPE_%u", i);
					auto tupleTypeExtractorType = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[tupleTypeExtractorType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return irType.as<core::GenericTypePtr>()->getTypeParameter(0).as<core::TupleTypePtr>()->getElement(i);
					};

					// variadic argument from enclosing type
					name = ::format("('ENCLOSING_TEMPLATE_T_%u...)", i);
					auto enclosingSingleType = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[enclosingSingleType] = [&converter, i](const clang::RecordDecl* recordDecl, const core::TypePtr& irType) {
						return converter.getIRBuilder().tupleType(extractEnclosingTemplateTypeArgumentPack(recordDecl, i, converter));
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

		// if the passed type is an AutoType or BuiltinType and is dependent, we can't really translate it correctly.
		// we create a dummy replacement type to move forward in the translation and assert this replacement doesn't survive in the final IR
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			if(autoType->isDependentType()) { return converter.getIRBuilder().genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER); }
		}
		if(auto builtinType = llvm::dyn_cast<clang::BuiltinType>(type)) {
			std::cout << "Builtin: ";
			builtinType->dump();
			std::cout << std::endl;
			if(builtinType->isDependentType()) {
				std::cout << "Dependent Builtin" << std::endl;
				return converter.getIRBuilder().genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER);
			}
		}

		return{};
	}

	insieme::core::TypePtr AllscaleExtension::PostVisit(const clang::QualType& typeIn, const insieme::core::TypePtr& irType,
		insieme::frontend::conversion::Converter& converter) {

		return irType;
	}


	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// EXPRESSIONS

	namespace {

		/**
		 * Removes all duplicate call operators in the passed type
		 */
		core::TagTypePtr removeDuplicateCallOperators(const core::TagTypePtr& tagType) {
			return core::transform::transformBottomUpGen(tagType, [](const core::MemberFunctionsPtr& memFuns) {
				core::MemberFunctionList newMemFuns;
				bool alreadyPresent = false;
				for(const auto& memFun : memFuns->getMembers()) {
					if(memFun->getNameAsString() == insieme::utils::getMangledOperatorCallName()) {
						if(alreadyPresent) {
							continue;
						}
						alreadyPresent = true;
					}
					newMemFuns.push_back(memFun);
				}
				return core::IRBuilder(memFuns->getNodeManager()).memberFunctions(newMemFuns);
			});
		}

		/**
		 * Generates a new call operator as a replacement for the given old one.
		 * The new operator will have the correct function type and a matching body which uses the passed recFun argument(s) correctly
		 */
		core::LambdaExprPtr fixCallOperator(const core::LambdaExprPtr& oldOperator, const core::TupleTypePtr& recFunTupleType) {

			auto& mgr = oldOperator->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
			const auto& oldFunType = oldOperator->getFunctionType();
			core::TypeList funTypeParamTypes(oldFunType->getParameterTypeList());
			core::VariableList params(oldOperator->getParameterList()->getParameters());

			// drop generated params and create mappings
			core::VariableList removedParams;
			for(unsigned i = 0; i < funTypeParamTypes.size() - 2; ++i) {
				funTypeParamTypes.pop_back();
				removedParams.insert(removedParams.begin(), params.back());
				params.pop_back();
			}
			// add recfun tuple type and param
			funTypeParamTypes.push_back(recFunTupleType);
			auto recfunTupleParam = builder.variable(core::transform::materialize(recFunTupleType));
			params.push_back(recfunTupleParam);

			// transform body and replace accesses to the dropped params with the desired accesses to the passed recfun tuple
			auto body = core::transform::transformBottomUpGen(oldOperator->getBody(), [&](const core::CallExprPtr& call) {
				if(refExt.isCallOfRefDeref(call)) {
					auto inner = call->getArgument(0);
					// here we need to replace the call
					// the index of our tuple access relates to the index of the dropped variable in our removedParams list
					auto index = std::find(removedParams.begin(), removedParams.end(), inner) - removedParams.begin();
					if((unsigned) index < removedParams.size()) {
						return builder.accessComponent(builder.deref(recfunTupleParam), index);
					}
				}
				return call;
			});

			auto functionType = builder.functionType(funTypeParamTypes, oldFunType->getReturnType(), core::FK_MEMBER_FUNCTION);
			auto ret = builder.lambdaExpr(functionType, params, body, oldOperator->getReference()->getNameAsString());
			return ret;
		}

		/**
		 * Replaces the call operator operatorLit inside the given tagType with newOperator
		 */
		core::TagTypePtr replaceCallOperator(const core::TagTypePtr& tagType, const core::LiteralPtr& operatorLit, const core::LambdaExprPtr& newOperator) {
			return core::transform::transformBottomUpGen(tagType, [&](const core::MemberFunctionsPtr& memFuns) {
				core::IRBuilder builder(memFuns->getNodeManager());
				core::MemberFunctionList newMemFuns;
				for(const auto& memFun : memFuns->getMembers()) {
					core::ExpressionPtr impl = memFun->getImplementation();
					if(impl == operatorLit) {
						impl = builder.literal(newOperator->getType(), operatorLit->getValue());
					}
					newMemFuns.push_back(builder.memberFunction(memFun->isVirtual(), memFun->getNameAsString(), impl));
				}
				return builder.memberFunctions(newMemFuns);
			});
		}

		/**
		 * Skips the given node if it is of type ClangTypeToSkip, and converts it's shild node instead, which is determined by calling the passed childExtractor
		 */
		template<typename ClangTypeToSkip>
		core::ExpressionPtr skipClangExpr(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter,
																			std::function<const clang::Expr*(const ClangTypeToSkip*)> childExtractor) {
			if(auto clangTypedExpr = llvm::dyn_cast<ClangTypeToSkip>(expr)) {
				auto retType = converter.convertType(expr->getType());
				if(lang::isTreeture(retType) || lang::isRecFun(retType)) {
					return converter.convertExpr(childExtractor(clangTypedExpr));
				}
			}
			return {};
		}


		using CallMapper = std::function<core::ExpressionPtr(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter)>;

		/// Utility for the specification of noop call mappings (C++ to IR)
		class NoopCallMapper {
		  public:
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
				if(auto memCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(call)) {
					auto thisArg = converter.convertExpr(memCall->getImplicitObjectArgument());
					return thisArg;
				}
				return converter.convertExpr(call->getArg(0));
			}
		};

		/// Utility for the specification of simple call mappings (C++ to IR)
		class SimpleCallMapper {
		  protected:
			virtual core::ExpressionPtr convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) {
				return converter.convertExpr(clangArg);
			}
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args, insieme::frontend::conversion::Converter& converter) {
				return args;
			}
			virtual core::ExpressionPtr generateCallee(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
				auto& allscaleExt = converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
				return converter.getIRBuilder().parseExpr(targetIRString, allscaleExt.getSymbols());
			}

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
					args.push_back(convertArgument(arg, converter));
				}
				return converter.getIRBuilder().callExpr(callee, postprocessArgumentList(args, converter));
			}

		  public:
			SimpleCallMapper(const string& targetIRString, bool derefThisArg = false) : targetIRString(targetIRString), derefThisArg(derefThisArg) {}

			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
				auto callee = generateCallee(call, converter);
				return buildCallWithDefaultParamConversion(callee, call, converter);
			}
		};

		/// Utility for the specification of treeture/task aggregation (C++ to IR)
		/// same as SimpleCallMapper, but skips std::move and converts completed_task to treeture as required
		/// also postprocesses argument list in order to generate empty dependencies list if none is available
		class AggregationCallMapper : public SimpleCallMapper {
			bool requiresDependencies = false;
		  protected:
			virtual core::ExpressionPtr convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) override {
				auto ret = converter.convertExpr(clangArg);
				if(auto clangCall = llvm::dyn_cast<clang::CallExpr>(clangArg)) {
					if(auto namedDecl = llvm::dyn_cast_or_null<clang::NamedDecl>(clangCall->getCalleeDecl())) {
						if(namedDecl->getQualifiedNameAsString() == "std::move") {
							ret = converter.convertExpr(clangCall->getArg(0));
						}
					}
				}
				ret = derefOrDematerialize(ret);
				if(auto lambdaType = extractLambdaOperationType(clangArg, converter, true)) {
					ret = lang::buildCppLambdaToLambda(ret, lambdaType);
				}
				return ret;
			}
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args,
				                                                 insieme::frontend::conversion::Converter& converter) override {
				if(requiresDependencies && (args.size() == 0 || !lang::isDependencies(args[0]))) {
					core::ExpressionList ret;
					ret.push_back(buildDependencyList(converter));
					std::copy(args.cbegin(), args.cend(), std::back_inserter(ret));
					return ret;
				}
				return args;
			}

		  public:
			AggregationCallMapper(const string& targetIRString, bool requiresDependencies = false)
				: SimpleCallMapper(targetIRString, true), requiresDependencies(requiresDependencies) {}
		};

		/// Utility to map the call operator call of recfun objects
		class RecFunCallMapper : public SimpleCallMapper {
		  protected:
			virtual core::ExpressionPtr generateCallee(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) override {
				auto opCall = llvm::dyn_cast<clang::CXXOperatorCallExpr>(call);
				assert_true(opCall);
				auto recfunArg = converter.convertExpr(opCall->getArg(0));
				assert_true(recfunArg);
				return lang::buildRecfunToFun(derefOrDematerialize(recfunArg));
			}
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args,
				insieme::frontend::conversion::Converter& converter) override {
				assert_ge(args.size(), 1);
				return core::ExpressionList(args.cbegin() + 1, args.cend());
			}

		  public:
			RecFunCallMapper() : SimpleCallMapper("") { }
		};

		/// Utility to map the call to fun
		class FunConstructionMapper {
		  public:
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
				assert_eq(call->getNumArgs(), 3) << "handleCoreFunCall expects 3 arguments";

				auto& builder = converter.getIRBuilder();
				auto& tMap = converter.getIRTranslationUnit().getTypes();

				// asserts if the structType (i.e. the lambda we are translating) created by the passed expression doesn't have a call operator -
				// i.e. the prec is never called
				auto checkForCallOperator = [&](const core::ExpressionPtr& expr) {
					auto genType = insieme::core::analysis::getReferencedType(expr->getType()).as<insieme::core::GenericTypePtr>();
					auto structType = tMap.at(genType)->getStruct();
					if(!utils::hasCallOperator(structType)) {
						assert_fail() << "Conversion of prec construct around lambda at \""
								<< insieme::frontend::utils::getLocationAsString(call->getLocStart(), converter.getSourceManager(), false)
								<< "\" failed, because the result is never actually called.";
					}
				};

				auto funType = lang::RecFunType(converter.convertType(call->getType()));

				// handle cutoff
				core::ExpressionPtr cutoffBind = nullptr;
				// simply convert the lambda
				{
					// first we translate the lambda
					auto cutoffIr = converter.convertExpr(call->getArg(0));
					// we check for the presence of a call operator
					checkForCallOperator(cutoffIr);

					// finally we create the closure type as well as the CppLambdaToClosure call
					auto cutoffClosureType = builder.functionType(funType.getParamType(), builder.getLangBasic().getBool(), insieme::core::FK_CLOSURE);
					cutoffBind = lang::buildCppLambdaToClosure(cutoffIr, cutoffClosureType);
				}

				// handle base case
				core::ExpressionPtr baseBind = nullptr;
				// simply convert the lambda
				{
					// first we translate the lambda
					auto baseIr = converter.convertExpr(call->getArg(1));
					// we check for the presence of a call operator
					checkForCallOperator(baseIr);

					// finally we create the closure type as well as the CppLambdaToClosure call
					auto baseClosureType = builder.functionType(funType.getParamType(), funType.getReturnType(), insieme::core::FK_CLOSURE);
					baseBind = lang::buildCppLambdaToClosure(baseIr, baseClosureType);
				}

				// handle step case
				core::ExpressionPtr stepBind = nullptr;
				// here we have to do a bit more work. We convert the lambda and afterwards have to modify it a bit
				{
					// first we translate the lambda
					auto stepIr = converter.convertExpr(call->getArg(2));
					// we check for the presence of a call operator
					checkForCallOperator(stepIr);

					// we create the closure type as well as the CppLambdaToClosure call
					auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr) funType));
					core::GenericTypePtr stepReturnType = lang::TreetureType(funType.getReturnType(), false);
					auto stepClosureType = builder.functionType(toVector<core::TypePtr>(funType.getParamType(), callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);
					stepBind = lang::buildCppLambdaToClosure(stepIr, stepClosureType);

					// we extract the generated struct tag type
					auto genType = insieme::core::analysis::getReferencedType(stepIr->getType()).as<insieme::core::GenericTypePtr>();
					auto tagType = tMap.at(genType);

					// and remove the duplicate call operators
					auto newTagType = removeDuplicateCallOperators(tagType);

					// now we fix the call operator and replace it with a correctly translated one
					auto oldOperatorLit = utils::extractCallOperator(newTagType->getStruct())->getImplementation().as<core::LiteralPtr>();

					// only fix the operator if it doesn't have the correct tuple type already
					if(!oldOperatorLit->getType().as<core::FunctionTypePtr>()->getParameterType(2).isa<core::TupleTypePtr>()) {
						auto newOperator = fixCallOperator(converter.getIRTranslationUnit()[oldOperatorLit], callableTupleType);
						newTagType = replaceCallOperator(newTagType, oldOperatorLit, newOperator);

						// finally we communicate the changes to the IR-TU
						converter.getIRTranslationUnit().removeFunction(oldOperatorLit);
						converter.getIRTranslationUnit().addFunction(builder.literal(newOperator->getType(), oldOperatorLit->getValue()), newOperator);
					}
					converter.getIRTranslationUnit().replaceType(genType, newTagType);

					// finally, we check for the presence of a call operator (the fixed one) again to make sure our conversion didn't lose it
					checkForCallOperator(stepIr);
				}

				// now that we have all three ingredients we can finally build the RecFun
				return lang::buildBuildRecFun(cutoffBind, toVector(baseBind), toVector(stepBind));
			}
		};

		/// Utility to map the call to prec
		class PrecMapper {
		  public:
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
				assert_eq(call->getNumArgs(), 1) << "prec call only supports 1 argument";
				return lang::buildPrec(toVector(derefOrDematerialize(converter.convertExpr(call->getArg(0)))));
			}
		};

		/// Mapping specification from C++ to IR used during call expression translation
		const static std::map<std::string, CallMapper> callMap = {
			// completed_tasks
			{"allscale::api::core::done", SimpleCallMapper("treeture_done")},
			{"allscale::api::core::.*::completed_task<.*>::operator treeture", SimpleCallMapper("treeture_run")},
			{"allscale::api::core::.*::completed_task<.*>::operator unreleased_treeture", NoopCallMapper()},
			// treeture
			{"allscale::api::core::impl::.*treeture.*::wait", SimpleCallMapper("treeture_wait", true)},
			{"allscale::api::core::impl::.*treeture.*::get", SimpleCallMapper("treeture_get", true)},
			{"allscale::api::core::impl::.*treeture.*::getLeft", SimpleCallMapper("treeture_left", true)},
			{"allscale::api::core::impl::.*treeture.*::getRight", SimpleCallMapper("treeture_right", true)},
			{"allscale::api::core::impl::.*treeture.*::getTaskReference", NoopCallMapper()},
			// task_reference
			{"allscale::api::core::impl::.*reference.*::wait", SimpleCallMapper("treeture_wait", true)},
			{"allscale::api::core::impl::.*reference::getLeft", SimpleCallMapper("treeture_left", true)},
			{"allscale::api::core::impl::.*reference::getRight", SimpleCallMapper("treeture_right", true)},
			// treeture aggregation
			{"allscale::api::core::combine", AggregationCallMapper("treeture_combine", true)},
			{"allscale::api::core::sequential", AggregationCallMapper("treeture_sequential", true)},
			{"allscale::api::core::parallel", AggregationCallMapper("treeture_parallel", true)},
			// dependencies
			{"allscale::api::core::after", AggregationCallMapper("dependency_after")},
			{"allscale::api::core::dependencies::add", AggregationCallMapper("dependency_add")},
			// recfun operations
			{R"(allscale::api::core::.*prec_operation<.*>::operator\(\))", RecFunCallMapper()},
			{R"(allscale::api::core::detail::callable<.*>::(Sequential|Parallel)Callable::operator\(\))", RecFunCallMapper()},
			// fun
			{"allscale::api::core::fun", FunConstructionMapper()},
			// prec
			{"allscale::api::core::prec", PrecMapper()},
		};
	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		expr->dumpColor();

		// we don't need special handling for CXXConstructExpr, MaterializeTemporaryExpr, ExprWithCleanups and VisitCXXBindTemporaryExpr on our AllScale types
		// these nodes are skipped and we only handle their respective child
		if(auto s = skipClangExpr<clang::CXXConstructExpr>(expr, converter,         [](const auto sE) { return sE->getArg(0); }))          { return s; }
		if(auto s = skipClangExpr<clang::MaterializeTemporaryExpr>(expr, converter, [](const auto sE) { return sE->GetTemporaryExpr(); })) { return s; }
		if(auto s = skipClangExpr<clang::ExprWithCleanups>(expr, converter,         [](const auto sE) { return sE->getSubExpr(); }))       { return s; }
		if(auto s = skipClangExpr<clang::CXXBindTemporaryExpr>(expr, converter,     [](const auto sE) { return sE->getSubExpr(); }))       { return s; }

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

