#pragma once

#include "parec/utils/printable.h"
#include "parec/utils/printer/join.h"

namespace parec {
namespace utils {

	namespace detail {

		template<typename T>
		struct const_seq_iter {
			T value;

			const_seq_iter(const T& v) : value(v) {}

			const_seq_iter(const const_seq_iter&) = default;

			const_seq_iter& operator=(const const_seq_iter& other) =default;

			bool operator==(const const_seq_iter& other) const {
				return value == other.value;
			}

			bool operator!=(const const_seq_iter& other) const {
				return !(*this == other);
			}

			template<typename V>
			const_seq_iter operator+(const V& offset) const {
				return value + offset;
			}

			auto operator-(const const_seq_iter& other) const -> decltype(value - other.value) {
				return value - other.value;
			}

			const_seq_iter& operator++() {
				value++;
				return *this;
			}

			const_seq_iter operator++(int) {
				const_seq_iter res = *this;
				value++;
				return res;
			}

			const T& operator*() const {
				return value;
			}
		};

	}


	template<typename T>
	struct sequence : public utils::Printable {
		typedef detail::const_seq_iter<T> const_iterator;

	private:

		const_iterator b;
		const_iterator e;

	public:

		sequence(const T& b, const T& e) : b(b), e(e) {}

		const const_iterator& begin() const {
			return b;
		}

		const const_iterator& end() const {
			return e;
		}

		auto size()->decltype(e-b) const {
			return e - b;
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << "[" << join(",",b,e) << "]";
		}

	};


	template<typename T>
	sequence<T> seq(const T& b, const T& e) {
		return sequence<T>(b,e);
	}

} // end namespace utils
} // end namespace parec

namespace std {

	template<typename T>
	size_t distance(const parec::utils::detail::const_seq_iter<T>& a, const parec::utils::detail::const_seq_iter<T>& b) {
		return b - a;
	}

}
