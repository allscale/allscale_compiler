#pragma once

#include <string>
#include <sstream>

template<typename T>
std::string toString(const T& value) {
	std::stringstream res;
	res << value;
	return res.str();
}

