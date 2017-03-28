
#pragma once

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace frontend {

	/// In case the passed lambda has a unit return value, this function will change it to treeture<unit, f> and fix all paths with returns
	insieme::core::LambdaExprPtr fixTreetureUnitLambda(const insieme::core::LambdaExprPtr& lambdaExpr);

}
}
}
