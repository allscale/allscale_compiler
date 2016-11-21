#pragma once

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace utils {

	insieme::core::MemberFunctionPtr extractCallOperator(const insieme::core::StructPtr& sourceLambda);

	insieme::core::FunctionTypePtr extractCallOperatorType(const insieme::core::StructPtr& sourceLambda);

}
}
}
