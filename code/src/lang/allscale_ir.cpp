
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/boolean_marker.h"

namespace allscale {
namespace compiler {
namespace lang {

	/////////////////////////////// RecFun

	RecFunType::RecFunType(const core::TypePtr& param, const core::TypePtr& ret) : param(param), ret(ret) { }

	RecFunType::operator core::GenericTypePtr() const {
		core::IRBuilder builder(param->getNodeManager());
		return builder.genericType("recfun", toVector(param, ret));
	}

	/////////////////////////////// Treeture

	TreetureType::TreetureType(const core::TypePtr& valueType, bool released) : valueType(valueType) {
		auto& mgr = valueType->getNodeManager();
		const auto& boolExt = mgr.getLangExtension<core::lang::BooleanMarkerExtension>();
		this->released = boolExt.getMarkerType(released);
	}

	TreetureType::operator core::GenericTypePtr() const {
		core::IRBuilder builder(valueType->getNodeManager());
		return builder.genericType("treeture", toVector(valueType, released));
	}

	/////////////////////////////// Builders

	core::ExpressionPtr buildLambdaToClosure(const core::ExpressionPtr& lambdaExpr, const core::FunctionTypePtr& closureType) {
		assert_eq(closureType.getKind(), core::FK_CLOSURE) << "Trying to build a closure of non-closure type.";
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getLambdaToClosure(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

}
}
}
