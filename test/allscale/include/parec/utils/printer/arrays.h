#pragma once

#include <array>
#include <iostream>

#include "parec/utils/printer/join.h"

namespace std {

	template<typename E, std::size_t N>
	ostream& operator<<(ostream& out, const array<E,N>& data) {
		return out << "[" << parec::utils::join(",", data) << "]";
	}

}
