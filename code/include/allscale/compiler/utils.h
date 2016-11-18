#pragma once

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace utils {

	insieme::core::FunctionTypePtr extractCallOperatorType(const insieme::core::StructPtr& sourceLambda);

}
}
}