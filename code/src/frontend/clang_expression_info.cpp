
#include "allscale/compiler/frontend/clang_expression_info.h"

#include "insieme/utils/assert.h"

namespace allscale {
namespace compiler {
namespace frontend {

	ClangExpressionInfo ClangExpressionInfo::getClangExpressionInfo(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		// check preconditions
		auto callExpr = llvm::dyn_cast<clang::CallExpr>(expr);
		auto memberCallExpr = llvm::dyn_cast<clang::CXXMemberCallExpr>(expr);
		auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(expr);
		assert_true(callExpr || constructExpr) << "Passed expr must either be a CallExpr or CXXConstructExpr";

		// fill fields
		auto numArgs = callExpr ? callExpr->getNumArgs() : constructExpr->getNumArgs();
		std::vector<const clang::Expr*> args;
		for(auto arg : (callExpr ? callExpr->arguments() : constructExpr->arguments())) {
			args.push_back(arg);
		}
		auto clangType = expr->getType();
		auto implicitObjectArgument = memberCallExpr ? memberCallExpr->getImplicitObjectArgument() : nullptr;
		auto isMemberCall = llvm::isa<clang::CXXMemberCallExpr>(expr);
		auto isOperatorCall = llvm::isa<clang::CXXOperatorCallExpr>(expr);
		auto isConstructorCall = llvm::isa<clang::CXXConstructExpr>(expr);
		auto locStart = expr->getLocStart();

		return {numArgs, args, clangType, implicitObjectArgument, isMemberCall, isOperatorCall, isConstructorCall, locStart, converter};
	}

}
}
}
