
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <boost/algorithm/string.hpp>

#include "insieme/annotations/c/include.h"
#include "insieme/core/lang/list.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/utils/iterator_utils.h"

#include "allscale/compiler/config.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/frontend/allscale_fe_extension_exprs.h"

namespace iu = insieme::utils;

namespace allscale {
namespace compiler {
namespace frontend {

	namespace core = insieme::core;

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// MAPPING OVERLOADS

	/// Mapping specification from C++ to IR types used during type translation
	std::map<std::string, std::string> AllscaleExtension::getTypeMappings() {
		return {
			// callables
			{ "allscale::api::core::fun_variants", "list<TUPLE_TYPE_0<('TEMPLATE_T_0...)>>" },
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
			// task_refs
			{ "allscale::api::core::impl::.*::task_reference", "task_ref" },
			// dependencies
			{ "allscale::api::core::.*dependencies", "dependencies" },
		};
	}

	std::vector<insieme::frontend::extensions::detail::FilterMapper> AllscaleExtension::getExprMappings() {
		return detail::exprMappings;
	}


	namespace {
		/// Name of placeholder generic type generated for dependent types for temporary translation
		static const char* ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER = "__AllScale__Dependent_AutoType";

		static bool debug = false;


		// returns whether we have to perform implicit materialization of an AllScale type for the given decl
		bool declNeedsImplicitMaterialization(const core::DeclarationPtr& decl) {
			const auto& declType = decl->getType();
			return (core::lang::isCppReference(declType) || core::lang::isCppRValueReference(declType))
					&& lang::isAllscaleType(core::analysis::getReferencedType(declType))
					&& !core::analysis::isRefType(decl->getInitialization()->getType());
		}
	}

	boost::optional<std::string> AllscaleExtension::isPrerequisiteMissing(insieme::frontend::ConversionSetup& setup) const {
		if(!setup.hasExtension<insieme::frontend::extensions::InterceptorExtension>()) {
			return std::string("AllscaleExtension requires the InterceptorExtension to be loaded");
		}
		//TODO: ensure that we are running before the interceptor extension
		return {};
	}


	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TYPES

	core::TypePtr AllscaleExtension::Visit(const clang::QualType& typeIn, insieme::frontend::conversion::Converter& converter) {
		// let the superclass handle the type mappings
		if(auto ret = MappingFrontendExtension::Visit(typeIn, converter)) {
			return ret;
		}

		// if the passed type is an AutoType or BuiltinType and is dependent, we can't really translate it correctly.
		// we create a dummy replacement type to move forward in the translation and assert this replacement doesn't survive in the final IR
		const clang::Type* type = typeIn->getUnqualifiedDesugaredType();
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			if(autoType->isDependentType()) { return converter.getIRBuilder().genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER); }
		}
		if(auto builtinType = llvm::dyn_cast<clang::BuiltinType>(type)) {
			if(debug) {
				std::cout << "Builtin: ";
				builtinType->dump();
				std::cout << std::endl;
			}
			if(builtinType->isDependentType()) {
				if(debug) std::cout << "Dependent Builtin" << std::endl;
				return converter.getIRBuilder().genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER);
			}
		}

		return{};
	}


	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// EXPRESSIONS

	namespace {
		/**
		 * Skips the given node if it is of type ClangTypeToSkip, and converts it's child node instead, which is determined by calling the passed childExtractor
		 */
		template<typename ClangTypeToSkip>
		core::ExpressionPtr skipClangExpr(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter,
																			std::function<const clang::Expr*(const ClangTypeToSkip*)> childExtractor) {
			if(auto clangTypedExpr = llvm::dyn_cast<ClangTypeToSkip>(expr)) {
				auto retType = converter.convertType(expr->getType());
				if(lang::isAllscaleType(retType)) {
					return converter.convertExpr(childExtractor(clangTypedExpr));
				}
			}
			return {};
		}
	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		if(debug) expr->dumpColor();

		// we don't need special handling for MaterializeTemporaryExpr, ExprWithCleanups and VisitCXXBindTemporaryExpr on our AllScale types
		// these nodes are skipped and we only handle their respective child
		if(auto s = skipClangExpr<clang::MaterializeTemporaryExpr>(expr, converter, [](const auto sE) { return sE->GetTemporaryExpr(); })) { return s; }
		if(auto s = skipClangExpr<clang::ExprWithCleanups>(expr, converter,         [](const auto sE) { return sE->getSubExpr(); }))       { return s; }
		if(auto s = skipClangExpr<clang::CXXBindTemporaryExpr>(expr, converter,     [](const auto sE) { return sE->getSubExpr(); }))       { return s; }

		// the actual mapping is done via the mapping functionality in the parent class
		return MappingFrontendExtension::Visit(expr, converter);
	}

	core::ExpressionPtr AllscaleExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExprIn,
	                                                 insieme::frontend::conversion::Converter& converter) {
		auto irExpr = irExprIn;

		// we apply the data item processing step
		irExpr = applyDataItemProcessing(expr, irExpr, converter);

		return irExpr;
	}

	insieme::core::ExpressionPtr AllscaleExtension::Visit(const clang::CastExpr* castExpr,
	                                                      insieme::core::ExpressionPtr& irExpr, insieme::core::TypePtr& irTargetType,
	                                                      insieme::frontend::conversion::Converter& converter) {

		auto& allscaleExt = irExpr->getNodeManager().getLangExtension<lang::AllscaleModule>();

		if(debug) std::cout << "!!\n";
		if(castExpr->getCastKind() == clang::CK_UncheckedDerivedToBase) {
			if(debug) std::cout << "!! Casting CK_UncheckedDerivedToBase " << dumpColor(irExpr->getType());
			auto irSourceType = irExpr->getType();
			if(core::analysis::isRefType(irExpr)) irSourceType = core::analysis::getReferencedType(irSourceType);
			if(lang::isAllscaleType(irSourceType)) {
				if(debug) std::cout << "!! Casting Allscale type\n";
				return irExpr;
			}
		}

		// treeture_get has different semantics
		if(castExpr->getCastKind() == clang::CK_LValueToRValue) {
			if(allscaleExt.isCallOfTreetureGet(irExpr)) {
				return irExpr;
			}
		}

		return nullptr;
	}

	insieme::core::ExpressionPtr AllscaleExtension::Visit(const clang::CXXCtorInitializer* ctorInit, const clang::Expr* initExpr,
	                                                      insieme::core::ExpressionPtr& irInitializedMemLoc,
	                                                      insieme::frontend::conversion::Converter& converter) {
		// ctor init exprs on AllScale types have special semantics. here we actually generate an init expression in order for the backend to create ctor init exprs again
		assert_true(core::lang::isReference(irInitializedMemLoc));
		if(lang::isAllscaleType(core::analysis::getReferencedType(irInitializedMemLoc))) {
			core::IRBuilder builder(irInitializedMemLoc->getNodeManager());
			return builder.initExpr(irInitializedMemLoc, converter.convertExpr(initExpr));
		}
		return nullptr;
	}


	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// FINAL POSTPROCESSING

	insieme::core::tu::IRTranslationUnit AllscaleExtension::IRVisit(insieme::core::tu::IRTranslationUnit& tu) {
		core::IRBuilder builder(tu.getNodeManager());

		/*
		 * We fix all user-defined store and load functions for serializable types which have template parameters.
		 * These functions get a wrong name from the frontend ("IMP_store_returns_void" and "IMP_load_returns_CLASSNAME"), which prevents the
		 * auto-serialization facilities from identifying them correctly and thus new ones might be generated if possible or that step will fail.
		 * Eventually, in the backend these types might then not be serializable or do something else upon serialization.
		 * We thus rename them to the names they would have gotten if the type hadn't been templated in the first place.
		 */
		const auto desiredStoreName = builder.stringValue("IMP_store");
		const auto desiredLoadName = builder.stringValue("IMP_load");
		const auto archiveReaderType = builder.refType(builder.genericType("IMP_allscale_colon__colon_utils_colon__colon_ArchiveReader"), false, false, core::lang::ReferenceType::Kind::CppReference);
		const auto archiveWriterType = builder.refType(builder.genericType("IMP_allscale_colon__colon_utils_colon__colon_ArchiveWriter"), false, false, core::lang::ReferenceType::Kind::CppReference);

		for(const auto& typeMapping : tu.getTypes()) {
			// we iterate over all types in the irTU
			auto type = typeMapping.second;

			// check all member functions
			for(const auto& memberFun : type->getRecord()->getMemberFunctions()) {
				const auto& name = memberFun->getName();
				// if the name starts with "IMP_store_", has 2 arguments and accepts an ArchiveWriter, we rename it
				if(boost::starts_with(name->getValue(), "IMP_store_")) {
					const auto paramTypes = memberFun->getImplementation()->getType().as<core::FunctionTypePtr>()->getParameterTypeList();
					if(paramTypes.size() == 2 && paramTypes[1] == archiveWriterType) {
						type = core::transform::replaceAllGen(tu.getNodeManager(), type, name, desiredStoreName, core::transform::globalReplacement);
						break;
					}
				}
			}
			// check all static member functions
			for(const auto& staticMemberFun : type->getRecord()->getStaticMemberFunctions()) {
				const auto& name = staticMemberFun->getName();
				// if the name starts with "IMP_load_", has 1 argument and accepts an ArchiveReader, we rename it
				if(boost::starts_with(name->getValue(), "IMP_load_")) {
					const auto paramTypes = staticMemberFun->getImplementation()->getType().as<core::FunctionTypePtr>()->getParameterTypeList();
					if(paramTypes.size() == 1 && paramTypes[0] == archiveReaderType) {
						type = core::transform::replaceAllGen(tu.getNodeManager(), type, name, desiredLoadName, core::transform::globalReplacement);
						break;
					}
				}
			}

			// replace the type in the TU. This doesn't change anything if we didn't modify it
			tu.replaceType(typeMapping.first, type);
		}
		return tu;
	}

	insieme::core::ProgramPtr AllscaleExtension::IRVisit(insieme::core::ProgramPtr& progIn) {
		core::IRBuilder builder(progIn->getNodeManager());
		const auto& refExt = progIn->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		auto prog = progIn;

		// do some actual post-processing here
		prog = core::transform::transformBottomUpGen(prog, [&](const core::DeclarationPtr& decl) {
			const auto& declType = decl->getType();
			// clean up ref_temp_init around list types, which might be added by the Insieme frontend
			if(core::lang::isPlainReference(declType)) {
				if(core::lang::isList(core::analysis::getReferencedType(declType))) {
					if(refExt.isCallOfRefTempInit(decl->getInitialization())) {
						return builder.declaration(declType, core::analysis::getArgument(decl->getInitialization(), 0));
					}
				}
			}

			// add implicit materializations for AllScale types if required
			if(declNeedsImplicitMaterialization(decl)) {
				const auto& declType = decl->getType();
				return builder.declaration(declType, core::lang::buildRefCast(builder.refTemp(decl->getInitialization()), declType));
			}

			return decl;
		}, core::transform::globalReplacement);

		// XXX: Replace all occurrences of the dummy dependent type replacement type in function return types with unit
		//
		// Rationale: We stumbled upon several instances of innocent looking lambdas like:
		//   [](X& x) { x.foo(); }
		// which caused the dummy replacement type to end up in the final IR. This can be fixed manually by specifying the return type of the lambda explicitly like:
		//   [](X& x) -> void { x.foo(); }
		// Normally, this should not be necessary, since in this case the result type can only be void, but it seems like that if a lambda is defined
		// inside a templated function, clang always tells us that the return type is an auto type and dependent.
		//
		// WARNING: We know that replacing the return type with unit might be wrong.
		// It fixed some problems for us but might lead to semantic errors which could be hard to diagnose. For this reason, we print a warning,
		// whenever we are applying this transformation, pointing the user to this comment here which gives some background and enables him to
		// verify the correctness. Getting rid of this warning will probably imply finding the offending lambda and specifying the return type
		// explicitly (has always been void for us to date).
		const auto dependentTypePlaceholder = builder.genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER);
		prog = core::transform::transformBottomUpGen(prog, [&](const core::FunctionTypePtr& funType) {
			// if we have a function type with the dummy dependent type replacement type as result type
			if(funType->getReturnType() == dependentTypePlaceholder) {
				// we print a warning and replace the return type with unit
				std::cout << "\n\nWARNING: We replaced an ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER node with the unit type in a function return type." << std::endl;
				std::cout << "         This might not be the correct type and should be investigated. For further details, have a look at the comment block" << std::endl;
				std::cout << "         above the code generating this warning message in file " << __FILE__ << " above line " << __LINE__ << "\n" << std::endl;
				return core::transform::replaceNode(funType.getNodeManager(), core::FunctionTypeAddress(funType)->getReturnType(),
				                                    builder.getLangBasic().getUnit()).as<core::FunctionTypePtr>();
			}
			return funType;
		}, core::transform::globalReplacement);

		// temporarily dump the generated IR in a readable format
		if(debug) dumpReadable(prog);

		// make sure that we don't have the dummy dependent type replacement type in the program anywhere anymore
		assert_eq(core::analysis::countInstances(prog, dependentTypePlaceholder, false), 0);

		// also make sure that the output doesn't contain any node which has any header from the core attached. if that happens we should have intercepted it
		assert_decl({
			core::visitDepthFirstOnce(prog, [](const core::NodePtr& node) {
				if(insieme::annotations::c::hasIncludeAttached(node)) {
					auto include = insieme::annotations::c::getAttachedInclude(node);
					const auto& coreIncludePath = getAllscaleAPIInterceptionIncludePath();
					if(include == coreIncludePath + "/data.h"
							|| include == coreIncludePath + "/prec.h"
							|| include == coreIncludePath + "/treeture.h"
							|| boost::starts_with(include, coreIncludePath + "/impl")) {
						assert_fail() << "Found attached include of core API to \"" << include << "\" on node: " << dumpReadable(node)
								<< "\n\nInvestigate whether:\n"
								<< "  - we forgot to map something, which should have been mapped in the AllScale frontend\n"
								<< "  - we forgot to intercept something which should have been intercepted\n"
								<< "  - we should include some more headers in pfor.h and the like. See API commit message d006bb9";
					}
				}
			});
		});

		return prog;
	}



	AllscaleExtension::AllscaleExtension() : insieme::frontend::extensions::MappingFrontendExtension() {
		includeDirs.push_back(getAllscaleAPICoreIncludeDir());
		includeDirs.push_back(getAllscaleAPIUtilsIncludeDir());

		// add a macro identifying the compiler to the API
		macros.insert({"ALLSCALECC", "epsilon"});
	}
}
}
}

