
#include "allscale/compiler/frontend/allscale_fe_extension_types.h"

#include <regex>

#include "insieme/core/transform/node_replacer.h"
#include "insieme/frontend/converter.h"

#include "allscale/compiler/lang/allscale_ir.h"


namespace allscale {
namespace compiler {
namespace frontend {
namespace detail {

	/// Mapping specification from C++ to IR types used during type translation
	const static std::map<std::string, std::string> typeMap = {
		// callables
		{ "allscale::api::core::fun_def", "recfun<TEMPLATE_T_1,TEMPLATE_T_0>" },
		{ "allscale::api::core::rec_defs", "('TEMPLATE_T_0...)" },
		{ "allscale::api::core::detail::prec_operation", "precfun<TEMPLATE_T_0,TEMPLATE_T_1>" },
		{ "allscale::api::core::detail::callable", "TUPLE_TYPE_0<TUPLE_TYPE_0<('TEMPLATE_T_0...)>>" },
		{ "allscale::api::core::detail::callable<.*>::(Sequential|Parallel)Callable", "TUPLE_TYPE_0<('ENCLOSING_TEMPLATE_T_0...)>" },
		// completed tasks
		{ "allscale::api::core::detail::completed_task", "treeture<TEMPLATE_T_0,f>" },
		// treetures
		{ "allscale::api::core::impl::.*::treeture", "treeture<TEMPLATE_T_0,t>" },
		{ "allscale::api::core::impl::.*::.*unreleased_treeture", "treeture<TEMPLATE_T_0,f>" },
		// dependencies
		{ "allscale::api::core::.*dependencies", "dependencies" },
	};


	/// Maximum number of template arguments we can map
	const unsigned MAX_MAPPED_TEMPLATE_ARGS = 8;

	namespace {
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

	void TypeMapper::initializeIfNeeded(insieme::frontend::conversion::Converter& converter) {
		// skip initialization if already done
		if(!typeIrMap.empty()) {
			return;
		}

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

	core::TypePtr TypeMapper::apply(const clang::Type* type) {
		// ensure the initialization has already been done
		if(typeIrMap.empty()) {
			assert_fail() << "TypeMapper not properly initialized";
		}

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

} // end namespace detail
} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
