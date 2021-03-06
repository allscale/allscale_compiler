#pragma once

#include "insieme/core/ir_address.h"

#include "insieme/analysis/cba/haskell/context.h"

namespace allscale {
namespace compiler {
namespace analysis {

	enum class OutOfBoundsResult : int {
		MayBeOutOfBounds,
		IsNotOutOfBounds,
		IsOutOfBounds,
	};

	OutOfBoundsResult getOutOfBounds(insieme::analysis::cba::haskell::Context& ctxt,
	                                 const insieme::core::CallExprAddress& expr);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
