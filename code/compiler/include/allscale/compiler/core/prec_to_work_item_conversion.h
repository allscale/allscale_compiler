#pragma once

#include "allscale/compiler/core/allscale_core.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Converts prec calls in the given input program to work item constructs.
	 */
	ConversionResult convertPrecToWorkItem(const insieme::core::NodePtr&, const ProgressCallback&);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
