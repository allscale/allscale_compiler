#include "allscale/compiler/checks/allscale_type_checks.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/error_codes.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_types.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/utils.h"

using namespace insieme::core;
using namespace insieme::core::checks;

namespace allscale {
namespace compiler {
namespace checks {

	OptionalMessageList LambdaToClosureCheck::visitCallExpr(const CallExprAddress& address) {
		OptionalMessageList res;

		auto& allscaleExt = address.getNodeManager().getLangExtension<lang::AllscaleModule>();

		if(!allscaleExt.isCallOfLambdaToClosure(address)) return res;

		// TODO remove
		// LANG_EXT_LITERAL(LambdaToClosure, "lambda_to_closure", "('l, type<('a) => 'b>) -> ('a) => 'b")

		// strip ref
		auto initType = analysis::getArgument(address, 0)->getType();
		if(analysis::isRefType(initType)) initType = analysis::getReferencedType(initType);

		// check lambda
		auto lambdaType = initType.isa<TagTypePtr>();
		if(!lambdaType || !lambdaType.isStruct()) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE, format("passed lambda is not a struct, but %s", *lambdaType), Message::ERROR));
			return res;
		}

		auto structType = lambdaType.getStruct();
		auto operatorType = utils::extractCallOperatorType(structType);
		if(!operatorType || operatorType->getParameterTypeList().size() != 2) {
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE, "passed lambda is not provide a valid call operator", Message::ERROR));
			return res;
		}

		auto funType = address->getType().as<FunctionTypePtr>();

		// check first parameter type
		auto expectedParamType = funType.getParameterType(0);
		auto actualParamType = operatorType.getParameterType(1);
		if(expectedParamType != actualParamType)
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
			                 format("wrong parameter type for lambda, expected: %s actual %s", expectedParamType, actualParamType), Message::ERROR));

		// check return type
		auto expectedReturnTYpe = funType.getReturnType();
		auto actualReturnType = operatorType.getReturnType();
		if(expectedReturnTYpe != actualReturnType)
			add(res, Message(address, EC_TYPE_INVALID_ARGUMENT_TYPE,
			                 format("wrong return type for lambda, expected: %s actual %s", expectedReturnTYpe, actualReturnType), Message::ERROR));

		return res;
	}

} // end namespace checks
} // end namespace compiler
} // end namespace allscale
