#pragma once

#include <vector>
#include <iostream>

#include "parec/utils/printer/join.h"

namespace std {

	template<typename E>
	ostream& operator<<(ostream& out, const vector<E>& data) {
		return out << "[" << parec::utils::join(",", data) << "]";
	}

}
