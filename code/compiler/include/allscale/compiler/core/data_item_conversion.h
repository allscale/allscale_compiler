#pragma once

#include "allscale/compiler/core/allscale_core.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Converts the given program into a program utilizing the AllScale runtime's
	 * data item infrastructure.
	 */
	insieme::core::ProgramPtr convertDataItemReferences(const insieme::core::ProgramPtr&, const ProgressCallback&);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
