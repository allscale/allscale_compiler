
// TODO:
// - check treeture validity (generic type components) -- based on "TreetureType" impl like "RefType"
// - all elements in each list of build_recfun must be of the same type
// - all elements in the tuple of prec must be of the same type

#pragma once

#include "insieme/core/checks/ir_checks.h"

#include "allscale/compiler/checks/allscale_type_checks.h"

namespace allscale {
namespace compiler {
namespace checks {

	/**
	 * Obtains a combined check case containing all the checks defined within this header file.
	 */
	insieme::core::checks::CheckPtr getFullCheck();

	/**
	 * Allies all known semantic checks on the given node and returns the obtained message list.
	 */
	insieme::core::checks::MessageList check(const insieme::core::NodePtr& node);

} // end namespace checks
} // end namespace compiler
} // end namespace allscale
