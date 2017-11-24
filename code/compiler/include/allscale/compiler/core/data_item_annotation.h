#pragma once

#include "allscale/compiler/core/allscale_core.h"

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace core {

	void markAsDataItem(const insieme::core::TypePtr& node);
	void removeDataItemMark(const insieme::core::TypePtr& node);
	bool isDataItem(const insieme::core::NodePtr& node);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
