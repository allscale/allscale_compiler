
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <boost/algorithm/string.hpp>

#include "insieme/annotations/c/include.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/utils/iterator_utils.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/config.h"

namespace iu = insieme::utils;

namespace allscale {
namespace compiler {
namespace frontend {

	namespace core = insieme::core;

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

		static bool debug = false;
	}

	detail::TypeMapper& AllscaleExtension::getTypeMapper(insieme::frontend::conversion::Converter& converter) {
		typeMapper.initializeIfNeeded(converter);
		return typeMapper;
	}

	insieme::core::TypePtr AllscaleExtension::Visit(const clang::QualType& typeIn, insieme::frontend::conversion::Converter& converter) {
		const clang::Type* type = typeIn->getUnqualifiedDesugaredType();

		// Apply the type mapping specification table to record types
		auto mappedType = getTypeMapper(converter).apply(type);
		if(mappedType) {
			if(debug) std::cout << "Mapped type to : " << *mappedType << std::endl;
			return mappedType;
		}

		// if the passed type is an AutoType or BuiltinType and is dependent, we can't really translate it correctly.
		// we create a dummy replacement type to move forward in the translation and assert this replacement doesn't survive in the final IR
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
		 * Skips the given node if it is of type ClangTypeToSkip, and converts it's shild node instead, which is determined by calling the passed childExtractor
		 */
		template<typename ClangTypeToSkip>
		core::ExpressionPtr skipClangExpr(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter,
																			std::function<const clang::Expr*(const ClangTypeToSkip*)> childExtractor) {
			if(auto clangTypedExpr = llvm::dyn_cast<ClangTypeToSkip>(expr)) {
				auto retType = converter.convertType(expr->getType());
				if(lang::isTreeture(retType) || lang::isRecFun(retType) || lang::isDependencies(retType)) {
					return converter.convertExpr(childExtractor(clangTypedExpr));
				}
			}
			return {};
		}
	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		if(debug) expr->dumpColor();

		// we don't need special handling for CXXConstructExpr, MaterializeTemporaryExpr, ExprWithCleanups and VisitCXXBindTemporaryExpr on our AllScale types
		// these nodes are skipped and we only handle their respective child
		if(auto s = skipClangExpr<clang::MaterializeTemporaryExpr>(expr, converter, [](const auto sE) { return sE->GetTemporaryExpr(); })) { return s; }
		if(auto s = skipClangExpr<clang::ExprWithCleanups>(expr, converter,         [](const auto sE) { return sE->getSubExpr(); }))       { return s; }
		if(auto s = skipClangExpr<clang::CXXBindTemporaryExpr>(expr, converter,     [](const auto sE) { return sE->getSubExpr(); }))       { return s; }

		// the actual mapping is done externally
		return detail::mapExpr(expr, converter);
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
			if(lang::isTreeture(irSourceType)) {
				if(debug) std::cout << "!! Casting treeture\n";
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


	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// FINAL POSTPROCESSING

	insieme::core::ProgramPtr AllscaleExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		core::IRBuilder builder(prog->getNodeManager());

		// temporarily dump the generated IR in a readable format
		if(debug) dumpReadable(prog);

		// make sure that we don't have the dummy dependent type replacement type in the program anywhere anymore
		assert_eq(core::analysis::countInstances(prog, builder.genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER), false), 0);

		// also make sure that the output doesn't contain any node which has any header from the core attached. if that happens we should have intercepted it
		assert_decl({
			core::visitDepthFirstOnce(prog, [](const core::NodePtr& node) {
				if(insieme::annotations::c::hasIncludeAttached(node)) {
					auto include = insieme::annotations::c::getAttachedInclude(node);
					assert_false(boost::starts_with(include, getAllscaleAPIInterceptionIncludePath()))
							<< "Found attached include of core API to \"" << include << "\"";
				}
			});
		});

		return prog;
	}

}
}
}

