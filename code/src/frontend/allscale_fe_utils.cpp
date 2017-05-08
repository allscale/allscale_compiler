
#include "allscale/compiler/frontend/allscale_fe_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/lang/allscale_ir.h"

namespace allscale {
namespace compiler {
namespace frontend {

	namespace core = insieme::core;

	core::LambdaExprPtr fixStepLambdaReturns(const core::LambdaExprPtr& lambdaExpr) {
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& basic = builder.getLangBasic();

		auto funType = lambdaExpr->getFunctionType();
		auto returnType = funType->getReturnType();
		auto body = lambdaExpr->getBody();

		auto buildReturn = [&builder](const core::ExpressionPtr& arg) {
			auto returnArg = lang::buildTreetureDone(arg);
			return builder.returnStmt(returnArg, core::transform::materialize(returnArg->getType()));
		};

		// if the last statement in the body of a lambda returning unit isn't a return, we need to add one
		if(basic.isUnit(returnType)) {
			core::StatementList bodyStmts(body->getStatements());
			if(bodyStmts.empty() || !bodyStmts.back().isa<core::ReturnStmtPtr>()) {
				bodyStmts.push_back(buildReturn(basic.getUnitConstant()));
				body = builder.compoundStmt(bodyStmts);
			}
		}

		// the return type needs to be a treeture
		if(!lang::isTreeture(returnType)) {
			returnType = (core::GenericTypePtr) lang::TreetureType(returnType, false);
		}

		// and every return expr; has to be changed to return treeture_done(expr);
		body = core::transform::transformBottomUpGen(body, [&](const core::ReturnStmtPtr& retStmt) {
			const auto& returnExpr = retStmt->getReturnExpr();
			if(!lang::isTreeture(returnExpr)) {
				return buildReturn(returnExpr);
			}
			return retStmt;
		});

		auto functionType = builder.functionType(funType->getParameterTypes(), returnType, funType->getKind());
		return builder.lambdaExpr(functionType, lambdaExpr->getParameterList()->getParameters(), body, lambdaExpr->getReference()->getNameAsString());
	}

}
}
}
