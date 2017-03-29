
#include "allscale/compiler/frontend/allscale_fe_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/lang/allscale_ir.h"

namespace allscale {
namespace compiler {
namespace frontend {

	namespace core = insieme::core;

	core::LambdaExprPtr fixTreetureUnitLambda(const core::LambdaExprPtr& lambdaExpr) {
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& basic = builder.getLangBasic();

		auto funType = lambdaExpr->getFunctionType();
		auto returnType = funType->getReturnType();
		auto body = lambdaExpr->getBody();

		// we need some special treatment for step cases with unit return type
		if(basic.isUnit(returnType)) {
			auto returnArg = lang::buildTreetureDone(basic.getUnitConstant());
			auto unitReturn = builder.returnStmt(returnArg, core::transform::materialize(returnArg->getType()));
			// the return type needs to be different
			returnType = (core::GenericTypePtr) lang::TreetureType(basic.getUnit(), false);
			// and every return unit; has to be changed to return treeture_done(unit);
			body = core::transform::transformBottomUpGen(body, [&](const core::ReturnStmtPtr& retStmt) {
				if(retStmt->getReturnExpr() == basic.getUnitConstant()) {
					return unitReturn;
				}
				return retStmt;
			});
			// finally, if the last statement in the body isn't a return, we need to add one
			core::StatementList bodyStmts(body->getStatements());
			if(bodyStmts.empty() || !bodyStmts.back().isa<core::ReturnStmtPtr>()) {
				bodyStmts.push_back(unitReturn);
				body = builder.compoundStmt(bodyStmts);
			}
		}

		auto functionType = builder.functionType(funType->getParameterTypes(), returnType, funType->getKind());
		return builder.lambdaExpr(functionType, lambdaExpr->getParameterList()->getParameters(), body, lambdaExpr->getReference()->getNameAsString());
	}

}
}
}
