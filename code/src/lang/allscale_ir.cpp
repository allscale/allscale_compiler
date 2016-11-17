
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/boolean_marker.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

namespace allscale {
namespace compiler {
namespace lang {

	/////////////////////////////// RecFun

	RecFunType::RecFunType(const core::TypePtr& param, const core::TypePtr& ret) : param(param), ret(ret) { }

	RecFunType::RecFunType(const core::NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<core::GenericTypePtr>();
		if (auto expr = node.isa<core::ExpressionPtr>()) type = expr->getType().isa<core::GenericTypePtr>();

		// check given node type
		assert_true(isRecFunType(type)) << "Given node " << *node << " is not a RecFun type!";

		*this = RecFunType(type->getTypeParameter(0), type->getTypeParameter(1));
	}

	RecFunType::operator core::GenericTypePtr() const {
		core::IRBuilder builder(param->getNodeManager());
		return builder.genericType("recfun", toVector(param, ret));
	}

	bool RecFunType::isRecFunType(const core::NodePtr& node) {
		// a quick check
		auto type = node.isa<core::GenericTypePtr>();
		if(!type) return false;

		// check properties
		return type->getTypeParameter().size() == 2 && type->getParents().empty() && type->getName()->getValue() == "recfun";
	}

	/////////////////////////////// Treeture

	TreetureType::TreetureType(const core::TypePtr& valueType, bool released) : valueType(valueType) {
		auto& mgr = valueType->getNodeManager();
		const auto& boolExt = mgr.getLangExtension<core::lang::BooleanMarkerExtension>();
		this->released = boolExt.getMarkerType(released);
	}

	TreetureType::TreetureType(const core::NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<core::GenericTypePtr>();
		if (auto expr = node.isa<core::ExpressionPtr>()) type = expr->getType().isa<core::GenericTypePtr>();

		// check given node type
		assert_true(isTreetureType(type)) << "Given node " << *node << " is not a Treeture type!";

		*this = TreetureType(type->getTypeParameter(0), type->getTypeParameter(1));
	}

	bool TreetureType::getReleased() {
		auto& mgr = valueType->getNodeManager();
		const auto& boolExt = mgr.getLangExtension<core::lang::BooleanMarkerExtension>();
		return boolExt.isTrueMarker(released);
	}

	TreetureType::operator core::GenericTypePtr() const {
		core::IRBuilder builder(valueType->getNodeManager());
		return builder.genericType("treeture", toVector(valueType, released));
	}

	bool TreetureType::isTreetureType(const core::NodePtr& node) {
		// a quick check
		auto type = node.isa<core::GenericTypePtr>();
		if(!type) return false;

		// check properties
		return type->getTypeParameter().size() == 2 && type->getParents().empty() && type->getName()->getValue() == "treeture";
	}

	/////////////////////////////// Builders

	core::ExpressionPtr buildBuildRecFun(const core::ExpressionPtr& cutoffBind,
	                                     const core::ExpressionList& baseBinds,
	                                     const core::ExpressionList& stepBinds) {
		assert_false(baseBinds.empty()) << "baseBinds must not be empty";
		assert_false(stepBinds.empty()) << "stepBinds must not be empty";
		auto& mgr = cutoffBind->getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& firstBaseType = baseBinds.front()->getType().as<core::FunctionTypePtr>();
		core::GenericTypePtr returnType = RecFunType(firstBaseType.getParameterType(0), firstBaseType.getReturnType());
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getBuildRecfun(), cutoffBind,
		                        core::encoder::toIR<core::ExpressionList, core::encoder::DirectExprListConverter>(mgr, baseBinds),
		                        core::encoder::toIR<core::ExpressionList, core::encoder::DirectExprListConverter>(mgr, stepBinds));
	}

	core::ExpressionPtr buildLambdaToClosure(const core::ExpressionPtr& lambdaExpr, const core::FunctionTypePtr& closureType) {
		assert_eq(closureType.getKind(), core::FK_CLOSURE) << "Trying to build a closure of non-closure type.";
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getLambdaToClosure(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

}
}
}
