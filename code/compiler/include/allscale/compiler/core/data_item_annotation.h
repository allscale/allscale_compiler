#pragma once

#include "allscale/compiler/core/allscale_core.h"

#include "insieme/core/dump/annotations.h"
#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace core {

	struct DataItemTag : public insieme::core::value_annotation::copy_on_migration {};

	void markAsDataItem(const insieme::core::TypePtr& node);
	bool isDataItem(const insieme::core::NodePtr& node);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
