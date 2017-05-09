
#pragma once

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace frontend {

	/// In case the passed lambda has return statements which don't return a treeture, this function will insert the implicit treeture_done cals on all paths with returns
	insieme::core::LambdaExprPtr fixStepLambdaReturns(const insieme::core::LambdaExprPtr& lambdaExpr);

}
}
}
