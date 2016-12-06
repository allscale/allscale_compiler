
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/boolean_marker.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/container_utils.h"

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
		assert_true(isRecFun(type)) << "Given node " << *node << " is not a RecFun type!\nType: " << *type;

		*this = RecFunType(type->getTypeParameter(0), type->getTypeParameter(1));
	}

	core::GenericTypePtr RecFunType::toIRType() const {
		core::IRBuilder builder(param->getNodeManager());
		return builder.genericType("recfun", toVector(param, ret));
	}

	RecFunType::operator core::GenericTypePtr() const {
		return toIRType();
	}

	bool isRecFun(const core::NodePtr& node) {
		// a quick check
		auto type = node.isa<core::GenericTypePtr>();
		if(auto expr = node.isa<core::ExpressionPtr>()) type = expr->getType().isa<core::GenericTypePtr>();
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

	TreetureType::TreetureType(const core::TypePtr& valueType, const core::TypePtr& released)
		: valueType(valueType), released(released) {}

	TreetureType::TreetureType(const core::NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<core::GenericTypePtr>();
		if (auto expr = node.isa<core::ExpressionPtr>()) type = expr->getType().isa<core::GenericTypePtr>();

		// check given node type
		assert_true(isTreeture(type)) << "Given node " << *node << " is not a Treeture type!\nType: " << *type << std::endl;

		*this = TreetureType(type->getTypeParameter(0), type->getTypeParameter(1));
	}

	bool TreetureType::isReleased() const {
		auto& mgr = valueType->getNodeManager();
		const auto& boolExt = mgr.getLangExtension<core::lang::BooleanMarkerExtension>();
		return boolExt.isTrueMarker(released);
	}

	core::GenericTypePtr TreetureType::toIRType() const {
		core::IRBuilder builder(valueType->getNodeManager());
		return builder.genericType("treeture", toVector(valueType, released));
	}

	TreetureType::operator core::GenericTypePtr() const {
		return toIRType();
	}

	bool isTreeture(const core::NodePtr& node) {
		// a quick check
		auto type = node.isa<core::GenericTypePtr>();
		if(auto expr = node.isa<core::ExpressionPtr>()) type = expr->getType().isa<core::GenericTypePtr>();
		if(!type) return false;

		// check properties
		if(type->getTypeParameter().size() != 2 || !type->getParents().empty() || type->getName()->getValue() != "treeture") { return false; }

		const auto& boolExt = node.getNodeManager().getLangExtension<core::lang::BooleanMarkerExtension>();
		auto released = type->getTypeParameter(1);
		bool isValidReleased = core::analysis::isGeneric(released) || boolExt.isTrueMarker(released) || boolExt.isFalseMarker(released);

		return isValidReleased;
	}


	/////////////////////////////// PrecOperation utility


	PrecFunction::PrecFunction(const core::ExpressionPtr& baseCaseTest, const core::ExpressionList& baseCases, const core::ExpressionList& stepCases)
		: baseCaseTest(baseCaseTest), baseCases(baseCases), stepCases(stepCases) {

		// check that actual values are present
		assert_true(baseCaseTest);
		assert_false(baseCases.empty());
		assert_false(stepCases.empty());

		assert_true(all(baseCases, id<core::ExpressionPtr>()));
		assert_true(all(stepCases, id<core::ExpressionPtr>()));

		// check that all cases have the same type
		assert_true(all(baseCases, [&](const core::ExpressionPtr& cur) { return *getBaseCaseType() == *cur->getType(); }));
		assert_true(all(stepCases, [&](const core::ExpressionPtr& cur) { return *getStepCaseType() == *cur->getType(); }));

		// check that they are all functions
		assert_true(baseCaseTest->getType().isa<core::FunctionTypePtr>());
		assert_true(baseCases[0]->getType().isa<core::FunctionTypePtr>());
		assert_true(stepCases[0]->getType().isa<core::FunctionTypePtr>());

		// check the write arity of those functions
		assert_eq(1, getBaseCaseTestType()->getParameterTypes().size());
		assert_eq(1, getBaseCaseType()->getParameterTypes().size());
		assert_eq(2, getStepCaseType()->getParameterTypes().size());

		// check that they all have the same first parameter type
		assert_eq(*getParameterType(), *getBaseCaseTestType()->getParameterTypes()[0]);
		assert_eq(*getParameterType(), *getBaseCaseType()->getParameterTypes()[0]);
		assert_eq(*getParameterType(), *getStepCaseType()->getParameterTypes()[0]);

		// check that cases have all the same type
		assert_true(all(baseCases,[&](const core::ExpressionPtr& cur) { return *baseCases[0]->getType() == *cur->getType(); }));
		assert_true(all(stepCases,[&](const core::ExpressionPtr& cur) { return *stepCases[0]->getType() == *cur->getType(); }));

		// check the return type of those
		assert_eq("bool", toString(*getBaseCaseTestType()->getReturnType()));
		assert_eq(getTreetureType().getValueType(), getBaseCaseType()->getReturnType());
		assert_eq(getTreetureType().toIRType(), getStepCaseType()->getReturnType());

		// check the recursive parameter of the step cases
		assert_true(getStepCaseType()->getParameterType(1).isa<core::TupleTypePtr>());
		assert_true(all(getRecursiveFunctionParameterTypes(),[&](const core::TypePtr& type){
			auto genType = type.isa<core::GenericTypePtr>();
			return genType && genType->getFamilyName() == "recfun" && genType->getTypeParameter()->size() == 2;
		}));

	}


	// -- getters and setters --

	void PrecFunction::setBaseCaseTest(const core::ExpressionPtr& test) {
		assert_eq(*getBaseCaseTestType(), *test->getType());
		baseCaseTest = test;
	}

	void PrecFunction::setBaseCases(const core::ExpressionList& cases) {
		assert_false(cases.empty());
		assert_true(all(cases, [&](const core::ExpressionPtr& cur) { return *getBaseCaseType() == *cur->getType(); }));
		baseCases = cases;
	}

	void PrecFunction::addBaseCase(const core::ExpressionPtr& baseCase) {
		assert_eq(*getBaseCaseType(),*baseCase->getType());
		baseCases.push_back(baseCase);
	}

	void PrecFunction::setStepCases(const core::ExpressionList& cases) {
		assert_false(cases.empty());
		assert_true(all(cases, [&](const core::ExpressionPtr& cur) { return *getStepCaseType() == *cur->getType(); }));
		stepCases = cases;
	}

	void PrecFunction::addStepCase(const core::ExpressionPtr& stepCase) {
		assert_eq(*getStepCaseType(),*stepCase->getType());
		stepCases.push_back(stepCase);
	}


	// -- more observers --

	core::FunctionTypePtr PrecFunction::getBaseCaseTestType() const {
		return baseCaseTest->getType().as<core::FunctionTypePtr>();
	}

	core::FunctionTypePtr PrecFunction::getBaseCaseType() const {
		assert_false(baseCases.empty());
		return baseCases.front()->getType().as<core::FunctionTypePtr>();
	}

	core::FunctionTypePtr PrecFunction::getStepCaseType() const {
		assert_false(stepCases.empty());
		return stepCases.front()->getType().as<core::FunctionTypePtr>();
	}

	core::TypePtr PrecFunction::getParameterType() const {
		return getBaseCaseTestType()->getParameterTypes()[0];
	}

	core::TypePtr PrecFunction::getResultType() const {
		return getBaseCaseType()->getReturnType();
	}

	TreetureType PrecFunction::getTreetureType() const {
		return TreetureType(getResultType(), false);
	}

	core::TypePtr PrecFunction::getRecursiveFunctionType() const {
		auto& mgr = baseCaseTest->getNodeManager();
		return core::GenericType::get(mgr, "recfun", { getParameterType(), getResultType() });
	}

	core::TypeList PrecFunction::getRecursiveFunctionParameterTypes() const {
		return getStepCaseType()->getParameterType(1).as<core::TupleTypePtr>()->getElementTypes();
	}


	// -- encoder interface --

	core::TypePtr PrecFunction::getEncodedType(core::NodeManager&) {
		assert_fail() << "This object is encoded as a generic type, and thus has no general type!";
		return core::TypePtr();
	}

	bool PrecFunction::isEncoding(const core::ExpressionPtr& expr) {
		auto& mgr = expr->getNodeManager();
		auto& ext = mgr.getLangExtension<AllscaleModule>();

		// check that the given expression is a build_recfun call
		if (!core::analysis::isCallOf(expr,ext.getBuildRecfun())) return false;

		// check that the arguments are list encodings
		auto recFunCall = expr.as<core::CallExprPtr>();
		if (!core::encoder::isEncodingOf<core::ExpressionList,core::encoder::DirectExprListConverter>(recFunCall->getArgument(1))) return false;
		if (!core::encoder::isEncodingOf<core::ExpressionList,core::encoder::DirectExprListConverter>(recFunCall->getArgument(2))) return false;

		// ok, test passed
		return true;
	}

	core::ExpressionPtr PrecFunction::toIR(core::NodeManager&) const {
		// build up the rec operator
		return buildBuildRecFun(baseCaseTest, baseCases, stepCases);
	}

	PrecFunction PrecFunction::fromIR(const core::ExpressionPtr& expr) {
		assert_pred1(isEncoding,expr);

		// decompose the rec fun call
		auto recFunCall = expr.as<core::CallExprPtr>();

		// extract the parameters
		auto baseCaseTest = recFunCall->getArgument(0);
		auto baseCases = core::encoder::toValue<core::ExpressionList,core::encoder::DirectExprListConverter>(recFunCall->getArgument(1));
		auto stepCases = core::encoder::toValue<core::ExpressionList,core::encoder::DirectExprListConverter>(recFunCall->getArgument(2));

		// build up result
		return PrecFunction(baseCaseTest, baseCases, stepCases);
	}



	// - PrecOperation -

	PrecOperation::PrecOperation(const std::vector<PrecFunction>& functions) : functions(functions) {

		// there must be at least one function
		assert_false(functions.empty());

		// check that functions are compatible
		assert_decl(
			{
				// assemble the recursive function parameter type
				std::vector<core::TypePtr> types;
				for(const auto& cur : functions) {
					types.push_back(cur.getRecursiveFunctionType());
				}

				// make sure all functions have the proper recursive function type parameters
				for(const auto& cur : functions) {
					assert_eq(types, cur.getRecursiveFunctionParameterTypes());
				}
			}
		);

	}

	bool PrecOperation::isPrecOperation(const core::NodePtr& node) {
		auto expr = node.isa<core::ExpressionPtr>();
		return expr && isEncoding(expr);
	}

	// -- more observers --

	core::TypePtr PrecOperation::getParameterType() const {
		return getFunction().getParameterType();
	}

	core::TypePtr PrecOperation::getResultType() const {
		return getFunction().getResultType();
	}

	TreetureType PrecOperation::getTreetureType() const {
		return getFunction().getTreetureType();
	}


	// -- encoder interface --

	core::TypePtr PrecOperation::getEncodedType(core::NodeManager&) {
		assert_fail() << "This object is encoded as a generic type, and thus has no general type!";
		return core::TypePtr();
	}

	bool PrecOperation::isEncoding(const core::ExpressionPtr& expr) {
		auto& mgr = expr->getNodeManager();
		auto& ext = mgr.getLangExtension<AllscaleModule>();

		// check that it is a prec call
		if (!core::analysis::isCallOf(expr, ext.getPrec())) return false;

		// check that the argument is a tuple
		auto arg = expr.as<core::CallExprPtr>()->getArgument(0);
		if (!arg.isa<core::TupleExprPtr>()) return false;

		// check that all elements in the tuple are recursive functions
		for(const auto& cur : arg.as<core::TupleExprPtr>()->getExpressions()) {
			if (!PrecFunction::isEncoding(cur)) return false;
		}

		// ok, in this way we accept
		return true;
	}

	core::ExpressionPtr PrecOperation::toIR(core::NodeManager& mgr) const {

		// convert the recursive functions
		core::ExpressionList funs;
		for(const auto& cur : functions) {
			funs.push_back(cur.toIR(mgr));
		}

		// build up the prec call
		return buildPrec(funs);
	}

	PrecOperation PrecOperation::fromIR(const core::ExpressionPtr& expr) {
		assert_pred1(isEncoding,expr);

		// decompose the prec call
		auto encodedFuns = expr.as<core::CallExprPtr>()->getArgument(0).as<core::TupleExprPtr>()->getExpressions();

		// convert the encoded functions
		std::vector<PrecFunction> funs;
		for(const auto& cur : encodedFuns) {
			funs.push_back(PrecFunction::fromIR(cur));
		}

		// build up result
		return PrecOperation(funs);
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

	core::ExpressionPtr buildPrec(const core::ExpressionList& recFuns) {
		assert_false(recFuns.empty()) << "recFuns must not be empty";
		auto& firstRecFun = recFuns.front();
		auto& mgr = firstRecFun->getNodeManager();
		core::IRBuilder builder(mgr);
		auto firstRecfunType = RecFunType(firstRecFun);
		core::GenericTypePtr treetureType = TreetureType(firstRecfunType.getReturnType(), false);
		auto returnType = builder.functionType(firstRecfunType.getParamType(), treetureType, core::FK_CLOSURE);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getPrec(), builder.tupleExpr(recFuns));
	}

	core::ExpressionPtr buildTreetureDone(const core::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		core::IRBuilder builder(mgr);
		core::GenericTypePtr returnType = TreetureType(param->getType(), false);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getTreetureDone(), param);
	}

	core::ExpressionPtr buildTreetureRun(const core::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		core::IRBuilder builder(mgr);
		auto treetureType = TreetureType(param);
		core::GenericTypePtr returnType = TreetureType(treetureType.getValueType(), true);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getTreetureRun(), param);
	}

	core::ExpressionPtr buildTreetureCombine(const core::ExpressionPtr& a, const core::ExpressionPtr& b,
	                                         const core::ExpressionPtr& combinerLambda, const core::ExpressionPtr& parallel) {
		assert_true(a) << "Given parameter a is null!";
		assert_true(b) << "Given parameter a is null!";
		assert_true(combinerLambda) << "Given parameter combinerLambda is null!";
		assert_true(parallel) << "Given parameter parallel is null!";
		auto combinerLambdaType = combinerLambda->getType();
		assert_true(combinerLambdaType.isa<core::FunctionTypePtr>()) << "Type of combinerLambda is not a FunctionType, but of type: " << combinerLambdaType;
		auto& mgr = a->getNodeManager();
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		core::IRBuilder builder(mgr);
		auto treetureTypeA = TreetureType(a);
		auto treetureTypeB = TreetureType(b);
		auto combinerType = combinerLambdaType.as<core::FunctionTypePtr>();
		auto combinerParamTypes = combinerType->getParameterTypeList();
		assert_eq(combinerParamTypes.size(), 2) << "Given combinerLambda doesn't have two parameters";
		assert_eq(treetureTypeA.getValueType(), combinerParamTypes[0]) << "Type of first parameter of combinerLambda: " << combinerParamTypes[0]
				<< " doesn't match value type of parameter a: " << treetureTypeA.getValueType();
		assert_eq(treetureTypeB.getValueType(), combinerParamTypes[1]) << "Type of second parameter of combinerLambda: " << combinerParamTypes[1]
				<< " doesn't match value type of parameter b: " << treetureTypeB.getValueType();
		core::GenericTypePtr returnType = TreetureType(combinerType->getReturnType(), false);
		assert_true(parallel->getType() == builder.getLangBasic().getBool()) << "Given parallel parameter is not of boolean type, but: " << parallel->getType();
		return builder.callExpr(returnType, allS.getTreetureCombine(), a, b, combinerLambda, parallel);
	}

	core::ExpressionPtr buildTreetureGet(const core::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		core::IRBuilder builder(mgr);
		TreetureType treeture(param);
		auto returnType = treeture.getValueType();
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getTreetureGet(), param);
	}

	core::ExpressionPtr buildTreetureToRef(const core::ExpressionPtr& treetureExpr, const core::TypePtr& targetType) {
		assert_true(treetureExpr) << "Given treetureExpr is null!";
		assert_true(targetType) << "Given targetType is null!";
		auto& mgr = treetureExpr->getNodeManager();
		core::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getTreetureToRef(), treetureExpr, builder.getTypeLiteral(targetType));
	}

	core::ExpressionPtr buildTreetureFromRef(const core::ExpressionPtr& refTreetureExpr) {
		assert_true(refTreetureExpr) << "Given refTreetureExpr is null!";
		auto& mgr = refTreetureExpr->getNodeManager();
		core::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getTreetureFromRef(), refTreetureExpr);
	}

	core::ExpressionPtr buildRecfunToFun(const core::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		core::IRBuilder builder(mgr);
		RecFunType recFun(param);
		auto returnType = builder.functionType(recFun.getParamType(), (core::GenericTypePtr) TreetureType(recFun.getReturnType(), false));
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getRecfunToFun(), param);
	}

	core::ExpressionPtr buildCppLambdaToClosure(const core::ExpressionPtr& lambdaExpr, const core::FunctionTypePtr& closureType) {
		assert_eq(closureType.getKind(), core::FK_CLOSURE) << "Trying to build a closure of non-closure type.";
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getCppLambdaToClosure(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

	core::ExpressionPtr buildCppLambdaToLambda(const core::ExpressionPtr& lambdaExpr, const core::FunctionTypePtr& closureType) {
		assert_eq(closureType.getKind(), core::FK_PLAIN) << "Trying to build a lambda of non-plain type.";
		core::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getCppLambdaToLambda(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

} // end namespace lang
} // end namespace compiler
} // end namespace allscale
