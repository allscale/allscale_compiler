#include "allscale/compiler/core/cpp_lambda_to_ir_conversion.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/allscale_utils.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	namespace {

		// -- Conversion of CppLambdas to Lambdas ----------------------------------------------------------------------

		LambdaExprPtr convertToLambda(const CallExprPtr& call) {
			NodeManager& mgr = call->getNodeManager();
			IRBuilder builder(mgr);

			assert_pred2(analysis::isCallOf, call, mgr.getLangExtension<lang::AllscaleModule>().getCppLambdaToLambda());

			// get the call operator of the passed lambda
			auto callOperator = utils::getCallOperatorImplementation(call->getArgument(0));

			assert_false(callOperator->isRecursive())
				<< "Recursive functions are not supported here!";

			auto opParams = callOperator->getParameterList();
			VariableList params(opParams.begin()+1,opParams.end());

			return builder.lambdaExpr(call->getType().as<FunctionTypePtr>(), params, callOperator->getBody());
		}

	}


	NodePtr convertCppLambdaToIR(const NodePtr& code) {

		NodeManager& mgr = code.getNodeManager();
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

		// transform all lambda_to_closure calls
		auto res = transform::transformBottomUp(code,[&](const NodePtr& node)->NodePtr {

			// only interested in calls ..
			auto call = node.isa<CallExprPtr>();
			if (!call) return node;

			// .. to the lambda_to_lambda
			if (analysis::isCallOf(call,ext.getCppLambdaToLambda())) {
				return convertToLambda(call);
			}

			// not interested in the rest
			return node;

		}, transform::globalReplacement);

		// check the result
		assert_correct_ir(res);

		// return result
		return res;
	}


} // end namespace core
} // end namespace compiler
} // end namespace allscale
