#pragma once

#include "insieme/backend/operator_converter.h"

namespace allscale {
namespace compiler {
namespace backend {

	/**
	 * Adds support for runtime-specific operators to the given operator converter
	 * table.
	 *
	 * @param manager the node manager to be used to obtain instances of operators used as key within the given table
	 * @param table the table to be extended
	 * @return a reference to the handed in table
	 */
	insieme::backend::OperatorConverterTable& addRuntimeSpecificOps(insieme::core::NodeManager& manager, insieme::backend::OperatorConverterTable& table);

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
