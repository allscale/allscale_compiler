#pragma once

#include <ostream>
#include <istream>
#include <type_traits>

// some convenience utilities
template <typename T>
void write(std::ostream& out, T value) {
	out.write((char*)&value, sizeof(T));
}

template <typename Iter>
void write(std::ostream& out, const Iter& a, const Iter& b) {
	for(auto it = a; it != b; ++it) {
		out.write((char*)&(*it), sizeof(std::remove_reference_t<decltype(*it)>));
	}
}

template <typename T>
T read(std::istream& in) {
	T value = T();
	in.read((char*)&value, sizeof(T));
	return value;
}

template <typename Iter>
void read(std::istream& in, const Iter& a, const Iter& b) {
	for(auto it = a; it != b; ++it) {
		*it = read<std::remove_reference_t<decltype(*it)>>(in);
	}
}
