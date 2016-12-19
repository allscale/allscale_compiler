#include "allscale/compiler/checks/allscale_type_checks.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/error_codes.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/types/match.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/allscale_utils.h"

using namespace insieme::core;
using namespace insieme::core::checks;

namespace allscale {
namespace compiler {
namespace checks {

	OptionalMessageList CppLambdaToClosureCheck::visitCallExpr(const CallExprAddress& address) {
		OptionalMessageList res;

		auto& allscaleExt = address.getNodeManager().getLangExtension<lang::AllscaleModule>();

		if(!allscaleExt.isCallOfCppLambdaToClosure(address)) return res;

		// strip ref
		auto initType = analysis::getArgument(address, 0)->getType();
		if(analysis::isRefType(initType)) initType = analysis::getReferencedType(initType);

		// check lambda
		auto lambdaType = initType.isa<TagTypePtr>();
		if(!lambdaType || !lambdaType.isStruct()) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE, format("passed lambda is not a struct, but %s", *lambdaType), Message::ERROR));
			return res;
		}

		auto funType = address->getType().as<FunctionTypePtr>();

		//check that we found a call operator with the correct number of arguments
		auto structType = lambdaType.getStruct();
		auto operatorType = utils::extractCallOperatorType(structType);
		if(!operatorType || operatorType->getParameterTypeList().size() < 2 || operatorType->getParameterTypeList().size() > 3
				|| operatorType->getParameterTypeList().size() != funType.getParameterTypeList().size() + 1) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE, "passed lambda does not provide a valid call operator", Message::ERROR));
			return res;
		}

		// check first parameter type
		auto expectedParamType = funType.getParameterType(0);
		auto actualParamType = operatorType.getParameterType(1);
		if(expectedParamType != actualParamType) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
			                 format("wrong parameter type for lambda, expected: %s actual: %s", *expectedParamType, *actualParamType), Message::ERROR));
		}

		// check the tuple with the recursive step function arguments if we are checking a step case here
		if(operatorType->getParameterTypeList().size() == 3) {
			expectedParamType = funType.getParameterType(1);
			actualParamType = operatorType.getParameterType(2);
			if(expectedParamType != actualParamType) {
				add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
												 format("wrong parameter type for lambda, expected: %s actual: %s", *expectedParamType, *actualParamType), Message::ERROR));
			}
		}

		// check return type
		auto expectedReturnType = funType.getReturnType();
		auto actualReturnType = operatorType.getReturnType();
		if(expectedReturnType != actualReturnType) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
			                 format("wrong return type for lambda, expected: %s actual: %s", *expectedReturnType, *actualReturnType), Message::ERROR));
		}

		return res;
	}

	OptionalMessageList CppLambdaToLambdaCheck::visitCallExpr(const CallExprAddress& address) {
		OptionalMessageList res;

		auto& allscaleExt = address.getNodeManager().getLangExtension<lang::AllscaleModule>();

		if(!allscaleExt.isCallOfCppLambdaToLambda(address)) return res;

		// strip ref
		auto initType = analysis::getArgument(address, 0)->getType();
		if(analysis::isRefType(initType)) initType = analysis::getReferencedType(initType);

		// check lambda
		auto lambdaType = initType.isa<TagTypePtr>();
		if(!lambdaType || !lambdaType.isStruct()) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE, format("passed lambda is not a struct, but %s", *lambdaType), Message::ERROR));
			return res;
		}

		auto funType = address->getType().as<FunctionTypePtr>();

		//check that we found a call operator with the correct number of arguments
		auto structType = lambdaType.getStruct();
		auto operatorType = utils::extractCallOperatorType(structType);
		if(!operatorType || operatorType->getParameterTypeList().size() != funType.getParameterTypeList().size() + 1) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE, "passed lambda does not provide a valid call operator", Message::ERROR));
			return res;
		}

		// check parameter types
		for(unsigned paramIndex = 0; paramIndex < funType.getParameterTypeList().size(); ++paramIndex) {
			auto expectedParamType = funType.getParameterType(paramIndex);
			auto actualParamType = operatorType.getParameterType(paramIndex + 1);
			if(types::typeMatchesWithOptionalMaterialization(address->getNodeManager(), expectedParamType, actualParamType)) {
				add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
												 format("wrong parameter type for lambda, expected: %s actual: %s", *expectedParamType, *actualParamType), Message::ERROR));
			}
		}

		// check return type
		auto expectedReturnType = funType.getReturnType();
		auto actualReturnType = operatorType.getReturnType();
		if(expectedReturnType != actualReturnType) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
			                 format("wrong return type for lambda, expected: %s actual: %s", *expectedReturnType, *actualReturnType), Message::ERROR));
		}

		return res;
	}

} // end namespace checks
} // end namespace compiler
} // end namespace allscale
