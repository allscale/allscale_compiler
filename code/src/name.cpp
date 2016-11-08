#include "allscale/name.h"

#include <string>

#include "insieme/core/ir_node.h"

namespace allscale {

	std::string name(void) {
		// check if insieme stuff can be used
		insieme::core::Node* ptr = nullptr;

		return ptr == nullptr ? "AllScale" : "Broken Software";
	}

} // end namespace allscale
