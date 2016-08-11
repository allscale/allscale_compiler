#pragma once

#include <string>
#include <ostream>
#include <array>
#include <vector>
#include <set>
#include <algorithm>

#include "parec/utils/print_utils.h"

namespace detail {

	template<typename Iter, typename Formatter>
	struct joiner {
		Iter begin, end;
		std::string sep;
		Formatter formatter;

		joiner(const Iter& begin, const Iter& end, const std::string& sep, const Formatter& formatter)
			: begin(begin), end(end), sep(sep), formatter(formatter) {}

		friend std::ostream& operator<<(std::ostream& out, const joiner& j) {
			for(auto it = j.begin; it != j.end; ++it) {
				j.formatter(out, *it);
				auto next = it;
				++next;
				if (next != j.end) {
					out << j.sep;
				}
			}
			return out;
		}
	};

	struct default_formatter {
		template<typename T>
		void operator()(std::ostream& out, const T& value) const {
			out << value;
		}
	};

}

template<typename Iter, typename Formatter>
detail::joiner<Iter,Formatter> join(const Iter& a, const Iter& b, const std::string& sep = ",", const Formatter& formatter = detail::default_formatter()) {
	return detail::joiner<Iter,Formatter>(a,b,sep,formatter);
}

template<typename Container, typename Formatter>
auto join(const Container& c, const std::string& sep = ",", const Formatter& formatter = detail::default_formatter()) -> decltype(join(c.begin(), c.end(), sep, formatter)) {
	return join(c.begin(), c.end(), sep, formatter);
}

template<typename Container>
auto join(const Container& c, const std::string& sep = ",") -> decltype(join(c,sep,detail::default_formatter())) {
	return join(c,sep,detail::default_formatter());
}

template<typename Container>
bool contains(const Container& a, const typename Container::value_type& value) {
	return std::find(a.begin(), a.end(), value) != a.end();
}

template<typename ContainerA, typename ContainerB>
bool containsAll(const ContainerA& a, const ContainerB& b) {
	for(const auto& cur : a) {
		if (std::find(b.begin(), b.end(), cur) == b.end()) {
			return false;
		}
	}
	return true;
}


namespace std {

	template<typename T, typename A>
	ostream& operator<<(ostream& out, const vector<T,A>& v) {
		return out << "[" << join(v,",") << "]";
	}

	template<typename K, typename C, typename A>
	ostream& operator<<(ostream& out, const set<K,C,A>& s) {
		return out << "{" << join(s,",") << "}";
	}

	template<typename A, typename B>
	ostream& operator<<(ostream& out, const std::pair<A,B>& p) {
		return out << "[" << p.first << "," << p.second << "]";
	}

}
