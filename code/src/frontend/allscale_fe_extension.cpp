
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <limits>

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

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TYPES

	namespace {
		/// Name of placeholder generic type generated for dependent types for temporary translation
		static const char* ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER = "__AllScale__Dependent_AutoType";

		/// Mapping specification from C++ to IR types used during type translation
		const static std::map<std::string, std::string> typeMap = {
			{ "allscale::api::core::detail::prec_operation", "recfun<TEMPLATE_T_0,TEMPLATE_T_1>" },
			{ "allscale::api::core::fun_def", "recfun<TEMPLATE_T_1,TEMPLATE_T_0>" },
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
	}

	namespace detail {

		/// Implementation of these type mapping specifications
		class TypeMapper {
			std::map<std::string, core::TypePtr> typeIrMap;

			using CodeExtractor = std::function<core::TypePtr(const clang::RecordDecl* recordDecl)>;
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
					auto type = converter.getIRBuilder().parseType(name, allscaleExt.getSymbols());
					placeholderReplacer[type] = [&converter, i](const clang::RecordDecl* recordDecl) {
						return extractTemplateTypeArgument(recordDecl, i, converter);
					};
				}
			}

			core::TypePtr apply(const clang::Type* type) {
				core::TypePtr ret{};
				// we are only interested in Record Types for now
				if(auto recordType = llvm::dyn_cast<clang::RecordType>(type)) {
					auto recordDecl = recordType->getDecl();
					auto name = recordDecl->getQualifiedNameAsString();
					// replace if we have a map entry for this name
					if(::containsKey(typeIrMap, name)) {
						ret = typeIrMap[name];
						// replace all placeholders in generted IR type
						ret = core::transform::transformBottomUpGen(ret, [&](const core::GenericTypePtr& genTypeIn) -> core::TypePtr {
							if(::containsKey(placeholderReplacer, genTypeIn)) {
								return placeholderReplacer[genTypeIn](recordDecl);
							}
							return genTypeIn;
						});
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

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		// we handle certain calls specially, which we differentiate by their callee's name
		if(auto call = llvm::dyn_cast<clang::CallExpr>(expr)) {
			auto decl = call->getCalleeDecl();
			if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
				auto name = funDecl->getQualifiedNameAsString();

				if(name == "allscale::api::core::prec") {
				}
				if(name == "allscale::api::core::fun") {
				}
				if(name == "allscale::api::core::done") {
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

