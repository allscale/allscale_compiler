#pragma once

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Propagates the value of global constants throughout the given program.
	 */
	insieme::core::NodePtr propagateGlobalConstants(const insieme::core::NodePtr&);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
