
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"

namespace allscale {
namespace compiler {
namespace lang {

	core::ExpressionPtr buildLambdaToClosure(const core::ExpressionPtr& lambdaExpr, const core::FunctionTypePtr& closureType) {
		assert_eq(closureType.getKind(), core::FK_CLOSURE) << "Trying to build a closure of non-closure type.";
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getLambdaToClosure(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

}
}
}
