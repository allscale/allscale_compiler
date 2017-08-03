#pragma once

#include "allscale/compiler/core/allscale_core.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Converts all Cpp lambdas in the given code fragment into IR constructs.
	 */
	insieme::core::NodePtr convertCppLambdaToIR(const insieme::core::NodePtr&);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
