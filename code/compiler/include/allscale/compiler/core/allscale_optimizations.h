#pragma once

#include "allscale/compiler/core/allscale_core.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Hoists DataItem access calls out of loops if that is possible (i.e. the access expression is invariant in the loop)
	 */
	insieme::core::NodePtr performDataItemGetLoopHoisting(const insieme::core::NodePtr&, const ProgressCallback& = detail::ignoreProgress);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
