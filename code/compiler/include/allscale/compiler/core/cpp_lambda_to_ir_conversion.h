#pragma once

#include "allscale/compiler/core/allscale_core.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Converts all Cpp lambdas in the given program into IR construts.
	 */
	insieme::core::ProgramPtr convertCppLambdaToIR(const insieme::core::ProgramPtr&);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
