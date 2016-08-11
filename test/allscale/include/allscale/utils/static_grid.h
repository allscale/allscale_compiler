#pragma once

namespace allscale {
namespace utils {

	template<size_t size>
	struct grid_offset {

		int pos;
		grid_offset<size-1> nested;

		grid_offset() : pos(0), nested() {}

		grid_offset(int pos, const grid_offset<size-1>& nested) : pos(pos), nested(nested) {}

		template<typename Container>
		grid_offset(const Container& c) : grid_offset(c.begin(), c.end()) {}

		grid_offset(const std::initializer_list<int>& list) : grid_offset(list.begin(), list.end()) {}

		template<typename Iter>
		grid_offset(const Iter& a, const Iter& b) : pos((a==b)?0:*a), nested((a==b)?b:a+1,b) {}

		grid_offset operator-() const {
			return grid_offset(-pos, -nested);
		}

	};

	template<>
	struct grid_offset<0> {

		grid_offset() {}

		template<typename Container>
		grid_offset(const Container& c) : grid_offset(c.begin(), c.end()) {}

		grid_offset(const std::initializer_list<int>& list) : grid_offset(list.begin(), list.end()) {}

		template<typename Iter>
		grid_offset(const Iter& a, const Iter& b) {
			assert(a==b && "Excess elements!");
		}

		grid_offset operator-() const {
			return *this;
		}
	};

	template<size_t size>
	std::ostream& operator<<(std::ostream& out, const grid_offset<size>& off) {
		return out << off.pos << "," << off.nested;
	}

	inline std::ostream& operator<<(std::ostream& out, const grid_offset<1>& off) {
		return out << off.pos;
	}

	inline std::ostream& operator<<(std::ostream& out, const grid_offset<0>& off) {
		return out;
	}



	template<size_t ... sizes>
	struct grid_address;

	template<size_t f, size_t ... rest>
	struct grid_address<f,rest...> {

		using offset = grid_offset<sizeof...(rest)+1>;

		int pos;
		grid_address<rest...> nested;

		grid_address() : pos(0), nested() {}

		template<typename Container>
		grid_address(const Container& c) : grid_address(c.begin(), c.end()) {}

		grid_address(const std::initializer_list<int>& list) : grid_address(list.begin(), list.end()) {}

		template<typename Iter>
		grid_address(const Iter& a, const Iter& b) : pos((a==b)?0:((*a % f + f) % f)), nested((a==b)?b:a+1,b) {}

		bool operator==(const grid_address& other) const {
			return pos == other.pos && nested == other.nested;
		}

		offset& move(offset& cur) {

			auto newPos = pos + cur.pos;

			// update position
			pos = ((newPos % ((int)f)) + f) % f;

			// update offset
			cur.pos = newPos / (int) f;
			cur.pos -= (newPos < 0) ? 1 : 0;

			// process recursive
			nested.move(cur.nested);

			// done
			return cur;
		}
	};

	template<>
	struct grid_address<> {

		using offset = grid_offset<0>;

		grid_address() {}

		template<typename Container>
		grid_address(const Container& c) : grid_address(c.begin(), c.end()) {}

		grid_address(const std::initializer_list<size_t>& list) : grid_address(list.begin(), list.end()) {}

		template<typename Iter>
		grid_address(const Iter& a, const Iter& b) {
			assert(a==b && "Excess elements!");
		}

		bool operator==(const grid_address& other) const {
			return true;
		}

		offset& move(offset& cur) {
			return cur;
		}
	};


	template<size_t ... sizes>
	std::ostream& operator<<(std::ostream& out, const grid_address<sizes...>& addr) {
		return out << addr.pos << "," << addr.nested;
	}

	template<size_t size>
	inline std::ostream& operator<<(std::ostream& out, const grid_address<size>& addr) {
		return out << addr.pos;
	}

	inline std::ostream& operator<<(std::ostream& out, const grid_address<>& addr) {
		return out;
	}



	template<typename Cell, size_t ... size>
	struct grid;

	template<typename Cell, size_t a, size_t ... rest>
	struct grid<Cell,a,rest...> {
		using data_type = std::array<grid<Cell,rest...>,a>;
		using addr_type = grid_address<a,rest...>;
		data_type data;

		Cell& operator[](const addr_type& addr) {
			return data[addr.pos][addr.nested];
		}

		const Cell& operator[](const addr_type& addr) const {
			return data[addr.pos][addr.nested];
		}

		template<typename Lambda>
		void for_each(const Lambda& lambda) const {
			for(const auto& cur : data) {
				cur.for_each(lambda);
			}
		}

		template<typename Lambda>
		void for_each(const Lambda& lambda) {
			for(auto& cur : data) {
				cur.for_each(lambda);
			}
		}

		template<typename Lambda>
		void for_each(addr_type& addr, const Lambda& lambda) const {
			for(size_t i = 0; i<a; i++) {
				addr.pos = i;
				data[i].for_each(addr.nested, lambda);
			}
		}

		template<typename Lambda>
		void for_each(addr_type& addr, const Lambda& lambda) {
			for(size_t i = 0; i<a; i++) {
				addr.pos = i;
				data[i].for_each(addr.nested, lambda);
			}
		}
	};

	template<typename Cell>
	struct grid<Cell> {
		using data_type = Cell;
		using addr_type = grid_address<>;

		data_type data;

		Cell& operator[](const addr_type& addr) {
			return data;
		}

		const Cell& operator[](const addr_type& addr) const {
			return data;
		}

		template<typename Lambda>
		void for_each(const Lambda& lambda) const {
			lambda(data);
		}

		template<typename Lambda>
		void for_each(const Lambda& lambda) {
			lambda(data);
		}

		template<typename Lambda>
		void for_each(addr_type&, const Lambda& lambda) const {
			lambda(data);
		}

		template<typename Lambda>
		void for_each(addr_type&, const Lambda& lambda) {
			lambda(data);
		}

	};

} // end utils
} // end namespace allscale
