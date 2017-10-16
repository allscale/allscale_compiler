
#include "allscale/compiler/lang/allscale_ir.h"

#include "allscale/compiler/allscale_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/boolean_marker.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/utils/container_utils.h"

namespace allscale {
namespace compiler {
namespace lang {

	namespace ic = insieme::core;

	/////////////////////////////// Dependencies

	bool isDependencies(const ic::NodePtr& node) {
		auto type = node.isa<ic::GenericTypePtr>();
		if(auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType().isa<ic::GenericTypePtr>();
		if(!type) return false;

		// check properties
		return type->getTypeParameter().size() == 0 && type->getParents().empty() && type->getName()->getValue() == "dependencies";
	}

	/////////////////////////////// RecFun

	RecOrPrecFunType::RecOrPrecFunType(const ic::TypePtr& param, const ic::TypePtr& ret) : param(param), ret(ret) {
		assert_true(!ic::lang::isReference(param) || ic::lang::isConstCppReference(param));
	}

	RecOrPrecFunType::RecOrPrecFunType(const ic::NodePtr& node) {
		assert_true(node) << "Given node is null!";

		ic::TypePtr type = node.isa<ic::TypePtr>();
		// support expressions as input
		if(auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType();

		assert_true(type) << "Given node " << *node << " is not an expression or a type";

		if(auto tupleType = type.isa<ic::TupleTypePtr>()) {
			*this = RecOrPrecFunType(tupleType->getElement(0));
			return;
		}

		// check given node type
		auto genType = type.isa<ic::GenericTypePtr>();
		assert_true(genType) << "Given node " << *node << " is not a generic type";
		assert_true(isRecFun(genType) || isPrecFun(genType)) << "Given node " << *node << " is not a RecFun nor a PrecFun type!\nType: " << *genType;

		*this = RecOrPrecFunType(genType->getTypeParameter(0), genType->getTypeParameter(1));
	}

	ic::GenericTypePtr RecFunType::toIRType() const {
		ic::IRBuilder builder(getParamType()->getNodeManager());
		return builder.genericType("recfun", toVector(getParamType(), getReturnType()));
	}

	RecFunType::operator ic::GenericTypePtr() const {
		return toIRType();
	}


	ic::GenericTypePtr PrecFunType::toIRType() const {
		ic::IRBuilder builder(getParamType()->getNodeManager());
		return builder.genericType("precfun", toVector(getParamType(), getReturnType()));
	}

	PrecFunType::operator ic::GenericTypePtr() const {
		return toIRType();
	}

	bool isRecFun(const ic::NodePtr& node) {
		// a quick check
		auto type = node.isa<ic::GenericTypePtr>();
		if(auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType().isa<ic::GenericTypePtr>();
		if(!type) return false;

		// check properties
		return type->getTypeParameter().size() == 2 && type->getParents().empty() && type->getName()->getValue() == "recfun";
	}

	bool isRecFunToFunCall(const ic::NodePtr& node) {
		if (!node) return false;
		const AllscaleModule& ext = node->getNodeManager().getLangExtension<AllscaleModule>();
		return ext.isCallOfRecfunToFun(node);
	}

	bool isRecFunToDepFunCall(const ic::NodePtr& node) {
		if (!node) return false;
		const AllscaleModule& ext = node->getNodeManager().getLangExtension<AllscaleModule>();
		return ext.isCallOfRecfunToDepFun(node);
	}

	bool isRecFunUnwrapperCall(const ic::NodePtr& node) {
		return isRecFunToFunCall(node) || isRecFunToDepFunCall(node);
	}

	bool isPrecFun(const ic::NodePtr& node) {
		// a quick check
		auto type = node.isa<ic::GenericTypePtr>();
		if(auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType().isa<ic::GenericTypePtr>();
		if(!type) return false;

		// check properties
		return type->getTypeParameter().size() == 2 && type->getParents().empty() && type->getName()->getValue() == "precfun";
	}

	bool isPrecFunToFunCall(const ic::NodePtr& node) {
		if (!node) return false;
		const AllscaleModule& ext = node->getNodeManager().getLangExtension<AllscaleModule>();
		return ext.isCallOfPrecfunToFun(node);
	}

	bool isPrecFunToDepFunCall(const ic::NodePtr& node) {
		if (!node) return false;
		const AllscaleModule& ext = node->getNodeManager().getLangExtension<AllscaleModule>();
		return ext.isCallOfPrecfunToDepFun(node);
	}

	bool isPrecFunUnwrapperCall(const ic::NodePtr& node) {
		return isPrecFunToFunCall(node) || isPrecFunToDepFunCall(node);
	}

	/////////////////////////////// Treeture

	TreetureType::TreetureType(const ic::TypePtr& valueType, bool released) : valueType(valueType) {
		auto& mgr = valueType->getNodeManager();
		const auto& boolExt = mgr.getLangExtension<ic::lang::BooleanMarkerExtension>();
		this->released = boolExt.getMarkerType(released);
	}

	TreetureType::TreetureType(const ic::TypePtr& valueType, const ic::TypePtr& released)
		: valueType(valueType), released(released) {}

	TreetureType::TreetureType(const ic::NodePtr& node) {
		assert_true(node) << "Given node is null!";

		// support expressions as input
		auto type = node.isa<ic::GenericTypePtr>();
		if (auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType().isa<ic::GenericTypePtr>();

		// check given node type
		assert_true(isTreeture(type)) << "Given node " << *node << " is not a Treeture type!\nType: " << *type << std::endl;

		*this = TreetureType(type->getTypeParameter(0), type->getTypeParameter(1));
	}

	bool TreetureType::isReleased() const {
		auto& mgr = valueType->getNodeManager();
		const auto& boolExt = mgr.getLangExtension<ic::lang::BooleanMarkerExtension>();
		return boolExt.isTrueMarker(released);
	}

	ic::GenericTypePtr TreetureType::toIRType() const {
		ic::IRBuilder builder(valueType->getNodeManager());
		return builder.genericType("treeture", toVector(valueType, released));
	}

	TreetureType::operator ic::GenericTypePtr() const {
		return toIRType();
	}

	bool isTreeture(const ic::NodePtr& node) {
		// a quick check
		auto type = node.isa<ic::GenericTypePtr>();
		if(auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType().isa<ic::GenericTypePtr>();
		if(!type) return false;

		// check properties
		if(type->getTypeParameter().size() != 2 || !type->getParents().empty() || type->getName()->getValue() != "treeture") { return false; }

		const auto& boolExt = node.getNodeManager().getLangExtension<ic::lang::BooleanMarkerExtension>();
		auto released = type->getTypeParameter(1);
		bool isValidReleased = ic::analysis::isGeneric(released) || boolExt.isTrueMarker(released) || boolExt.isFalseMarker(released);

		return isValidReleased;
	}

	bool isTaskReference(const ic::NodePtr& node) {
		// support expressions
		if(auto expr = node.isa<ic::ExpressionPtr>()) return isTaskReference(expr->getType());

		// check that it is a generic type
		auto type = node.isa<ic::GenericTypePtr>();
		if (!type) return false;

		// check properties
		return type->getFamilyName() == "task_ref" && type->getParents().empty() && type->getTypeParameterList().empty();
	}

	bool isAllscaleType(const ic::NodePtr& node) {
		return isTreeture(node) || isDependencies(node) || isRecFun(node) || isPrecFun(node) || isTaskReference(node);
	}

	/////////////////////////////// Completed Task

	bool isCompletedTask(const ic::NodePtr& node) {
		auto type = node.isa<ic::GenericTypePtr>();
		if(auto expr = node.isa<ic::ExpressionPtr>()) type = expr->getType().isa<ic::GenericTypePtr>();
		if(!type) return false;

		return type->getName()->getValue() == "completed_task" && type->getTypeParameter().size() == 1;
	}

	/////////////////////////////// PrecOperation utility


	PrecFunction::PrecFunction(const ic::ExpressionPtr& baseCaseTest, const ic::ExpressionList& baseCases, const ic::ExpressionList& stepCases)
		: baseCaseTest(baseCaseTest), baseCases(baseCases), stepCases(stepCases) {

		// check that actual values are present
		assert_true(baseCaseTest);
		assert_false(baseCases.empty());
		assert_false(stepCases.empty());

		assert_true(all(baseCases, id<ic::ExpressionPtr>()));
		assert_true(all(stepCases, id<ic::ExpressionPtr>()));

		// check that all cases have the same type
		assert_true(all(baseCases, [&](const ic::ExpressionPtr& cur) { return *getBaseCaseType() == *cur->getType(); }));
		assert_true(all(stepCases, [&](const ic::ExpressionPtr& cur) { return *getStepCaseType() == *cur->getType(); }));

		// check that they are all functions
		assert_true(baseCaseTest->getType().isa<ic::FunctionTypePtr>());
		assert_true(baseCases[0]->getType().isa<ic::FunctionTypePtr>());
		assert_true(stepCases[0]->getType().isa<ic::FunctionTypePtr>());

		// check the write arity of those functions
		assert_eq(1, getBaseCaseTestType()->getParameterTypes().size());
		assert_eq(1, getBaseCaseType()->getParameterTypes().size());
		assert_eq(2, getStepCaseType()->getParameterTypes().size());

		// check that they all have the same first parameter type
		assert_eq(*getParameterType(), *getBaseCaseTestType()->getParameterTypes()[0]);
		assert_eq(*getParameterType(), *getBaseCaseType()->getParameterTypes()[0]);
		assert_eq(*getParameterType(), *getStepCaseType()->getParameterTypes()[0]);

		// check that cases have all the same type
		assert_true(all(baseCases,[&](const ic::ExpressionPtr& cur) { return *baseCases[0]->getType() == *cur->getType(); }));
		assert_true(all(stepCases,[&](const ic::ExpressionPtr& cur) { return *stepCases[0]->getType() == *cur->getType(); }));

		// check the return type of those
		assert_eq("bool", toString(*getBaseCaseTestType()->getReturnType()));
		assert_eq(getTreetureType().getValueType(), getBaseCaseType()->getReturnType());
		assert_eq(getTreetureType().toIRType(), getStepCaseType()->getReturnType());

		// check the recursive parameter of the step cases
		assert_true(getStepCaseType()->getParameterType(1).isa<ic::TupleTypePtr>());
		assert_true(all(getRecursiveFunctionParameterTypes(),[&](const ic::TypePtr& type){
			auto genType = type.isa<ic::GenericTypePtr>();
			return genType && genType->getFamilyName() == "recfun" && genType->getTypeParameter()->size() == 2;
		}));

	}


	// -- getters and setters --

	void PrecFunction::setBaseCaseTest(const ic::ExpressionPtr& test) {
		assert_eq(*getBaseCaseTestType(), *test->getType());
		baseCaseTest = test;
	}

	void PrecFunction::setBaseCases(const ic::ExpressionList& cases) {
		assert_false(cases.empty());
		assert_true(all(cases, [&](const ic::ExpressionPtr& cur) { return *getBaseCaseType() == *cur->getType(); }));
		baseCases = cases;
	}

	void PrecFunction::addBaseCase(const ic::ExpressionPtr& baseCase) {
		assert_eq(*getBaseCaseType(),*baseCase->getType());
		baseCases.push_back(baseCase);
	}

	void PrecFunction::setStepCases(const ic::ExpressionList& cases) {
		assert_false(cases.empty());
		assert_true(all(cases, [&](const ic::ExpressionPtr& cur) { return *getStepCaseType() == *cur->getType(); }));
		stepCases = cases;
	}

	void PrecFunction::addStepCase(const ic::ExpressionPtr& stepCase) {
		assert_eq(*getStepCaseType(),*stepCase->getType());
		stepCases.push_back(stepCase);
	}


	// -- more observers --

	ic::FunctionTypePtr PrecFunction::getBaseCaseTestType() const {
		return baseCaseTest->getType().as<ic::FunctionTypePtr>();
	}

	ic::FunctionTypePtr PrecFunction::getBaseCaseType() const {
		assert_false(baseCases.empty());
		return baseCases.front()->getType().as<ic::FunctionTypePtr>();
	}

	ic::FunctionTypePtr PrecFunction::getStepCaseType() const {
		assert_false(stepCases.empty());
		return stepCases.front()->getType().as<ic::FunctionTypePtr>();
	}

	ic::TypePtr PrecFunction::getParameterType() const {
		return getBaseCaseTestType()->getParameterTypes()[0];
	}

	ic::TypePtr PrecFunction::getResultType() const {
		return getBaseCaseType()->getReturnType();
	}

	TreetureType PrecFunction::getTreetureType() const {
		return TreetureType(getResultType(), false);
	}

	ic::TypePtr PrecFunction::getRecursiveFunctionType() const {
		auto& mgr = baseCaseTest->getNodeManager();
		return ic::GenericType::get(mgr, "recfun", { getParameterType(), getResultType() });
	}

	ic::TypeList PrecFunction::getRecursiveFunctionParameterTypes() const {
		return getStepCaseType()->getParameterType(1).as<ic::TupleTypePtr>()->getElementTypes();
	}


	// -- encoder interface --

	ic::TypePtr PrecFunction::getEncodedType(ic::NodeManager&) {
		assert_fail() << "This object is encoded as a generic type, and thus has no general type!";
		return ic::TypePtr();
	}

	bool PrecFunction::isEncoding(const ic::ExpressionPtr& expr) {
		auto& mgr = expr->getNodeManager();
		auto& ext = mgr.getLangExtension<AllscaleModule>();

		// check that the given expression is a build_recfun call
		if (!ic::analysis::isCallOf(expr,ext.getBuildRecfun())) return false;

		// check that the arguments are list encodings
		auto recFunCall = expr.as<ic::CallExprPtr>();
		if (!ic::encoder::isEncodingOf<ic::ExpressionList,ic::encoder::DirectExprListConverter>(recFunCall->getArgument(1))) return false;
		if (!ic::encoder::isEncodingOf<ic::ExpressionList,ic::encoder::DirectExprListConverter>(recFunCall->getArgument(2))) return false;

		// ok, test passed
		return true;
	}

	ic::ExpressionPtr PrecFunction::toIR(ic::NodeManager&) const {
		// build up the rec operator
		return buildBuildRecFun(baseCaseTest, baseCases, stepCases);
	}

	PrecFunction PrecFunction::fromIR(const ic::ExpressionPtr& expr) {
		assert_pred1(isEncoding,expr);

		// decompose the rec fun call
		auto recFunCall = expr.as<ic::CallExprPtr>();

		// extract the parameters
		auto baseCaseTest = recFunCall->getArgument(0);
		auto baseCases = ic::encoder::toValue<ic::ExpressionList,ic::encoder::DirectExprListConverter>(recFunCall->getArgument(1));
		auto stepCases = ic::encoder::toValue<ic::ExpressionList,ic::encoder::DirectExprListConverter>(recFunCall->getArgument(2));

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
				std::vector<ic::TypePtr> types;
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

	bool PrecOperation::isPrecOperation(const ic::NodePtr& node) {
		auto expr = node.isa<ic::ExpressionPtr>();
		return expr && isEncoding(expr);
	}

	// -- more observers --

	ic::TypePtr PrecOperation::getParameterType() const {
		return getFunction().getParameterType();
	}

	ic::TypePtr PrecOperation::getResultType() const {
		return getFunction().getResultType();
	}

	TreetureType PrecOperation::getTreetureType() const {
		return getFunction().getTreetureType();
	}


	// -- encoder interface --

	ic::TypePtr PrecOperation::getEncodedType(ic::NodeManager&) {
		assert_fail() << "This object is encoded as a generic type, and thus has no general type!";
		return ic::TypePtr();
	}

	bool PrecOperation::isEncoding(const ic::ExpressionPtr& expr) {
		auto& mgr = expr->getNodeManager();
		auto& ext = mgr.getLangExtension<AllscaleModule>();

		// check that it is a prec call
		if (!ic::analysis::isCallOf(expr, ext.getPrec())) return false;

		// check that the argument is a tuple
		auto arg = expr.as<ic::CallExprPtr>()->getArgument(0);
		if (!arg.isa<ic::TupleExprPtr>()) return false;

		// check that all elements in the tuple are recursive functions
		for(const auto& cur : arg.as<ic::TupleExprPtr>()->getExpressions()) {
			if (!PrecFunction::isEncoding(cur)) return false;
		}

		// ok, in this way we accept
		return true;
	}

	ic::ExpressionPtr PrecOperation::toIR(ic::NodeManager& mgr) const {

		// convert the recursive functions
		ic::ExpressionList funs;
		for(const auto& cur : functions) {
			funs.push_back(cur.toIR(mgr));
		}

		// build up the prec call
		return buildPrec(funs);
	}

	PrecOperation PrecOperation::fromIR(const ic::ExpressionPtr& expr) {
		assert_pred1(isEncoding,expr);

		// decompose the prec call
		auto encodedFuns = expr.as<ic::CallExprPtr>()->getArgument(0).as<ic::TupleExprPtr>()->getExpressions();

		// convert the encoded functions
		std::vector<PrecFunction> funs;
		for(const auto& cur : encodedFuns) {
			funs.push_back(PrecFunction::fromIR(cur));
		}

		// build up result
		return PrecOperation(funs);
	}


	/////////////////////////////// Builders

	ic::ExpressionPtr buildBuildRecFun(const ic::ExpressionPtr& cutoffBind,
	                                     const ic::ExpressionList& baseBinds,
	                                     const ic::ExpressionList& stepBinds) {
		assert_false(baseBinds.empty()) << "baseBinds must not be empty";
		assert_false(stepBinds.empty()) << "stepBinds must not be empty";
		auto& mgr = cutoffBind->getNodeManager();
		ic::IRBuilder builder(mgr);
		const auto& firstBaseType = baseBinds.front()->getType().as<ic::FunctionTypePtr>();
		ic::GenericTypePtr returnType = RecFunType(firstBaseType.getParameterType(0), firstBaseType.getReturnType());
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getBuildRecfun(), cutoffBind,
		                        ic::encoder::toIR<ic::ExpressionList, ic::encoder::DirectExprListConverter>(mgr, baseBinds),
		                        ic::encoder::toIR<ic::ExpressionList, ic::encoder::DirectExprListConverter>(mgr, stepBinds));
	}

	ic::ExpressionPtr buildPrec(const ic::ExpressionPtr& recFunTuple) {
		assert_true(recFunTuple) << "recFunTuple must not be null";
		assert_true(recFunTuple->getType().isa<ic::TupleTypePtr>()) << "recFunTuple must be of TupleType";
		auto& mgr = recFunTuple->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto precfunType = PrecFunType(recFunTuple);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(precfunType.toIRType(), allS.getPrec(), recFunTuple);
	}

	ic::ExpressionPtr buildPrec(const ic::ExpressionList& recFuns) {
		assert_false(recFuns.empty()) << "recFuns must not be empty";
		auto& firstRecFun = recFuns.front();
		auto& mgr = firstRecFun->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto precfunType = PrecFunType(firstRecFun);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(precfunType.toIRType(), allS.getPrec(), builder.tupleExpr(recFuns));
	}


	ic::ExpressionPtr buildNoDependencies(ic::NodeManager& mgr) {
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getDependencyAfter());
	}

	ic::ExpressionPtr buildTreetureDone(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		ic::GenericTypePtr returnType = TreetureType(param->getType(), false);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getTreetureDone(), param);
	}

	ic::ExpressionPtr buildTreetureRun(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto treetureType = TreetureType(param);
		ic::GenericTypePtr returnType = TreetureType(treetureType.getValueType(), true);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getTreetureRun(), param);
	}

	ic::ExpressionPtr buildTreetureCombine(const ic::ExpressionPtr& a, const ic::ExpressionPtr& b,
	                                         const ic::ExpressionPtr& combinerLambda, const ic::ExpressionPtr& parallel) {
		assert_true(a) << "Given parameter a is null!";
		assert_true(b) << "Given parameter a is null!";
		assert_true(combinerLambda) << "Given parameter combinerLambda is null!";
		assert_true(parallel) << "Given parameter parallel is null!";
		auto combinerLambdaType = combinerLambda->getType();
		assert_true(combinerLambdaType.isa<ic::FunctionTypePtr>()) << "Type of combinerLambda is not a FunctionType, but of type: " << combinerLambdaType;
		auto& mgr = a->getNodeManager();
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		ic::IRBuilder builder(mgr);
		auto treetureTypeA = TreetureType(a);
		auto treetureTypeB = TreetureType(b);
		auto combinerType = combinerLambdaType.as<ic::FunctionTypePtr>();
		auto combinerParamTypes = combinerType->getParameterTypeList();
		assert_eq(combinerParamTypes.size(), 2) << "Given combinerLambda doesn't have two parameters";
		assert_eq(treetureTypeA.getValueType(), combinerParamTypes[0]) << "Type of first parameter of combinerLambda: " << combinerParamTypes[0]
				<< " doesn't match value type of parameter a: " << treetureTypeA.getValueType();
		assert_eq(treetureTypeB.getValueType(), combinerParamTypes[1]) << "Type of second parameter of combinerLambda: " << combinerParamTypes[1]
				<< " doesn't match value type of parameter b: " << treetureTypeB.getValueType();
		ic::GenericTypePtr returnType = TreetureType(combinerType->getReturnType(), false);
		assert_true(parallel->getType() == builder.getLangBasic().getBool()) << "Given parallel parameter is not of boolean type, but: " << parallel->getType();
		return builder.callExpr(returnType, allS.getTreetureCombine(), a, b, combinerLambda, parallel);
	}

	ic::ExpressionPtr buildTreetureGet(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		TreetureType treeture(param);
		auto returnType = treeture.getValueType();
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(returnType, allS.getTreetureGet(), param);
	}

	ic::ExpressionPtr buildTreetureToRef(const ic::ExpressionPtr& treetureExpr, const ic::TypePtr& targetType) {
		assert_true(treetureExpr) << "Given treetureExpr is null!";
		assert_true(targetType) << "Given targetType is null!";
		auto& mgr = treetureExpr->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getTreetureToRef(), treetureExpr, builder.getTypeLiteral(targetType));
	}

	ic::ExpressionPtr buildTreetureFromRef(const ic::ExpressionPtr& refTreetureExpr) {
		assert_true(refTreetureExpr) << "Given refTreetureExpr is null!";
		auto& mgr = refTreetureExpr->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getTreetureFromRef(), refTreetureExpr);
	}

	ic::ExpressionPtr buildRecfunToFun(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getRecfunToFun(), param);
	}
	ic::ExpressionPtr buildRecfunToDepFun(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getRecfunToDepFun(), param);
	}

	ic::ExpressionPtr buildPrecfunToFun(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getPrecfunToFun(), param);
	}
	ic::ExpressionPtr buildPrecfunToDepFun(const ic::ExpressionPtr& param) {
		assert_true(param) << "Given node is null!";
		auto& mgr = param->getNodeManager();
		ic::IRBuilder builder(mgr);
		auto& allS = mgr.getLangExtension<AllscaleModule>();
		return builder.callExpr(allS.getPrecfunToDepFun(), param);
	}

	ic::ExpressionPtr buildCppLambdaToClosure(const ic::ExpressionPtr& lambdaExpr, ic::FunctionTypePtr closureType) {
		assert_eq(closureType.getKind(), ic::FK_CLOSURE) << "Trying to build a closure of non-closure type.";
		ic::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getCppLambdaToClosure(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

	ic::ExpressionPtr buildCppLambdaToLambda(const ic::ExpressionPtr& lambdaExpr, ic::FunctionTypePtr closureType) {
		assert_eq(closureType.getKind(), ic::FK_PLAIN) << "Trying to build a lambda of non-plain type.";
		ic::IRBuilder builder(lambdaExpr->getNodeManager());
		auto& allS = lambdaExpr->getNodeManager().getLangExtension<AllscaleModule>();
		return builder.callExpr(closureType, allS.getCppLambdaToLambda(), lambdaExpr, builder.getTypeLiteral(closureType));
	}

} // end namespace lang
} // end namespace compiler
} // end namespace allscale
