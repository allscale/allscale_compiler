#pragma once

#include "allscale/compiler/core/allscale_core.h"

namespace allscale {
namespace compiler {
namespace core {

	/**
	 * Converts the given code into equivalent code utilizing the AllScale runtime's
	 * data item infrastructure.
	 */
	insieme::core::NodePtr convertDataItemReferences(const insieme::core::NodePtr&, const ProgressCallback& = detail::ignoreProgress);

	/**
	 * Converts data item references captured by references into data item references captured by value.
	 */
	insieme::core::NodePtr convertCapturedDataItemReferences(const insieme::core::NodePtr&, const ProgressCallback& = detail::ignoreProgress);


} // end namespace core
} // end namespace compiler
} // end namespace allscale
