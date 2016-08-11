#pragma once

#include <vector>

namespace allscale {
namespace utils {

	template<typename Element>
	Element sumPrefixes(std::vector<Element>& vector) {
		Element counter = 0;
		for(std::size_t i = 0; i<vector.size(); i++) {
			std::size_t cur = vector[i];
			vector[i] = counter;
			counter += cur;
		}
		return counter;
	}

} // end namespace utils
} // end namespace allscale
