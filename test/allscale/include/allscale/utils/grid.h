#pragma once

#include <initializer_list>

#include "parec/ops.h"
#include "utils/container_utils.h"

namespace allscale {
namespace utils {

	template<typename T, size_t Dims>
	class vec {

		std::array<T,Dims> elements;

	public:

		vec() {};

		vec(const T& e) {
			for(size_t i = 0; i < Dims; i++) (*this)[i] = e;
		}

		vec(const vec&) = default;
		vec(vec&&) = default;

		vec(const std::array<T,Dims>& other) : elements(other) {}

		vec(const std::initializer_list<T>& values) {
			size_t pos = 0;
			for(const auto& cur : values) {
				elements[pos++] = cur;
				if (pos > Dims) return;
			}
		}

		operator std::array<T,Dims>&() {
			return elements;
		}

		operator const std::array<T,Dims>&() const {
			return elements;
		}

		T& operator[](std::size_t pos) {
			return elements[pos];
		}

		const T& operator[](std::size_t pos) const {
			return elements[pos];
		}

		vec& operator=(const vec& other) = default;
		vec& operator=(vec&& other) = default;

		vec& operator+=(const vec& other) {
			for(size_t i = 0; i<Dims; i++) {
				elements[i] += other[i];
			}
			return *this;
		}

		vec& operator-=(const vec& other) {
			for(size_t i = 0; i<Dims; i++) {
				elements[i] -= other[i];
			}
			return *this;
		}

		template<typename S>
		vec& operator*=(const S& scalar) {
			for(size_t i = 0; i<Dims; i++) {
				elements[i] *= scalar;
			}
			return *this;
		}

		vec operator+(const vec& other) const {
			return vec(*this) += other;
		}

		vec operator-(const vec& other) const {
			return vec(*this) -= other;
		}

		template<typename S>
		vec operator*(const S& scalar) const {
			return vec(*this) *= scalar;
		}

		bool operator==(const vec& other) const {
			return elements == other.elements;
		}

		bool operator!=(const vec& other) const {
			return elements != other.elements;
		}

		auto begin() const -> decltype(elements.begin()) {
			return elements.begin();
		}

		auto end() const -> decltype(elements.end()) {
			return elements.end();
		}


		friend std::ostream& operator<<(std::ostream& out, const vec& v) {
			return out << v.elements;
		}

	};


	template <size_t Dims>
	using Coordinate = vec<int, Dims>;

	template<size_t Dims>
	using Size = Coordinate<Dims>;

	template<typename Elem, size_t Dims, typename Op>
	void pfor(const vec<Elem,Dims>& a, const vec<Elem,Dims>& b, const Op& op) {
		const std::array<Elem,Dims>& x = a;
		const std::array<Elem,Dims>& y = b;
		parec::pfor(x,y,[&](const std::array<Elem,Dims>& pos) {
			op(vec<Elem,Dims>(pos));
		});
	}

	template<typename Elem, size_t Dims, typename Op>
	void pfor(const vec<Elem,Dims>& a, const Op& op) {
		pfor(vec<Elem,Dims>(0),a,op);
	}

	template<size_t Dims>
	size_t area(const Size<Dims>& size) {
		size_t res = 1;
		for(size_t i = 0; i<Dims; i++) {
			res *= size[i];
		}
		return res;
	}


	template<size_t dims>
	class coordinate_iterator {

		Size<dims> size;
		Coordinate<dims> cur;
		bool end;

	public:

		coordinate_iterator(const Size<dims>& size) : size(size), cur(0), end(false) {}
		coordinate_iterator() : end(true) { cur[0] = -1; }

		coordinate_iterator& operator++() {

			// implement a counter
			int i = dims-1;
			cur[i]++;
			while(i >= 0 && cur[i] == size[i]) {
				cur[i] = 0;
				i--;
				if (i>=0) cur[i]++;
			}

			// check whether we are done
			if (i < 0) end = true;
			// done

			return *this;
		}

		bool operator==(const coordinate_iterator& other) const {
			return end == other.end || cur == other.cur;
		}

		bool operator!=(const coordinate_iterator& other) const {
			return !(*this == other);
		}

		const Coordinate<dims>& operator*() const {
			return cur;
		}

	};

	template<size_t Dims>
	struct range {
		coordinate_iterator<Dims> b,e;

		range(const coordinate_iterator<Dims>& a, const coordinate_iterator<Dims>& b)
			: b(a), e(b) {}

		const coordinate_iterator<Dims>& begin() const {
			return b;
		}
		const coordinate_iterator<Dims>& end() const {
			return e;
		}
	};

	template<size_t Dims>
	range<Dims> fullRange(const Size<Dims>& size) {
		return range<Dims>(coordinate_iterator<Dims>(size), coordinate_iterator<Dims>());
	}

	template<typename Elem, size_t Dims, typename Op>
	void for_each(const vec<Elem,Dims>& a, const Op& op) {
		for(const auto& cur : fullRange(a)) {
			op(cur);
		}
	}


	namespace detail {

		template<typename ElementType, size_t Dimensions>
		struct default_boundary_condition {
			template<typename Grid>
			ElementType operator()(const Grid& grid, const Coordinate<Dimensions>&, const Size<Dimensions>&) const {
				return ElementType();		// just the default instance
			}
		};

	}

	template<typename ElementType, size_t dims>
	struct nearest_neighbor_boundary_condition {
		template<typename Grid>
		ElementType operator()(const Grid& grid, const Coordinate<dims>& pos, const Size<dims>& size) const {
			auto nearest = pos;
			for(size_t i=0; i<dims; i++) {
				nearest[i] = std::max(std::min(pos[i],size[i]-1), 0);
			}
			return grid[nearest];
		}
	};


	template <
		typename ElementType,
		size_t Dimensions,
		typename BoundaryHandler = detail::default_boundary_condition<ElementType,Dimensions>
	>
	class Grid {

	public:

		static const unsigned dimensions = Dimensions;

	private:

		typedef ElementType Elem;

		Size<Dimensions> dimension_size;

		int buffer_size;
		Elem* storage;

	public:

		Grid(const Size<Dimensions>& size) : dimension_size(size)
		{
			buffer_size = 1;
			for (const auto i : size) buffer_size *= i;

			storage = new Elem[buffer_size];
		}

		// do not allow copy
		Grid(const Grid<Elem, Dimensions>& o) = delete;
		Grid(Grid<Elem, Dimensions>&& o) = delete;

		~Grid() {
			delete[] storage;
		}

		const Size<Dimensions>& getSize() const {
			return dimension_size;
		}

	private:

		int flatten(const Coordinate<Dimensions>& pos) const {
			int res = 0;
			int size = 1;

			for(int i=Dimensions-1; i>=0; i--) {
				res += pos[i] * size;
				size *= dimension_size[i];
			}

			return res;
		}

	public:

		Elem& operator[](const Coordinate<Dimensions>& pos) {
			for(size_t i = 0; i<dimensions; i++) {
				assert(0 <= pos[i] && pos[i] < dimension_size[i]);
			}
			return storage[flatten(pos)];
		}

		Elem operator[](const Coordinate<Dimensions>& pos) const {
			static const BoundaryHandler handler = BoundaryHandler();
			// check whether it is out of bounds
			for(size_t i = 0; i<Dimensions; i++) {
				if (pos[i] < 0 || dimension_size[i] <= pos[i]) {
					return handler(*this, pos, dimension_size);
				}
			}
			// check inner
			return storage[flatten(pos)];
		}

		void swap(Grid& other) {
			// swap the content
			std::swap(dimension_size,other.dimension_size);
			std::swap(buffer_size,other.buffer_size);
			std::swap(storage,other.storage);
		}

	};

} // end namespace utils
} // end namespace allscale
