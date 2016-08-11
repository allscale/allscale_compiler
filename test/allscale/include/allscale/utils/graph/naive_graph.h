#pragma once

#include <vector>

#include "utils/container_utils.h"

namespace allscale {
namespace utils {
namespace graph {


	template<
		typename static_data,
		typename dynamic_data
	>
	struct NaiveNodeReference;

	template<
		typename static_data,
		typename dynamic_data
	>
	struct NaiveNode {
		static_data initial_static_data;
		dynamic_data initial_dynamic_data;
		bool boundary;

		std::vector<NaiveNodeReference<static_data,dynamic_data>> neighbors;

		NaiveNode(const static_data& s, const dynamic_data& d, bool boundary = false)
			: initial_static_data(s), initial_dynamic_data(d), boundary(boundary) {}
	};

	template<
		typename static_data,
		typename dynamic_data
	>
	struct NaiveNodeReference {

		std::size_t index;

	public:

		NaiveNodeReference() : index(0) {}

		NaiveNodeReference(std::size_t index) : index(index) {}

		operator std::size_t() const { return index; }

		bool operator==(const NaiveNodeReference& other) const {
			return index == other.index;
		}

		bool operator!=(const NaiveNodeReference& other) const {
			return index != other.index;
		}

		bool operator<(const NaiveNodeReference& other) const {
			return index < other.index;
		}

		friend std::ostream& operator<<(std::ostream& out, const NaiveNodeReference& ref) {
			return out << ref.index;
		}
	};

	template<
		typename static_data,
		typename dynamic_data
	>
	class NaiveNodeSet {

	public:

		using node_type = NaiveNode<static_data,dynamic_data>;
		using node_ref_type = NaiveNodeReference<static_data,dynamic_data>;
		using node_collection_type = std::vector<node_ref_type>;
		using node_list = std::vector<node_type>;


		const node_list* graph;

		using const_iterator = typename node_collection_type::const_iterator;

	private:

		node_collection_type nodes;

	public:

		NaiveNodeSet(const node_list& graph, const node_collection_type& nds = node_collection_type())
			: graph(&graph), nodes(nds) {}

		NaiveNodeSet(const NaiveNodeSet& other) = default;
		NaiveNodeSet(NaiveNodeSet&& other) = default;

		template<typename Iter>
		NaiveNodeSet(const node_list& graph, const Iter& a, const Iter& b)
			: graph(&graph), nodes(a,b) {}

		NaiveNodeSet& operator=(const NaiveNodeSet& other) = default;

		const_iterator begin() const {
			return nodes.begin();
		}

		const_iterator end() const {
			return nodes.end();
		}

		bool empty() const {
			return nodes.empty();
		}

		std::size_t size() const {
			return nodes.size();
		}

		std::vector<NaiveNodeSet> split() const {
			if (empty()) return std::vector<NaiveNodeSet>();
			if (size() == 1) return {*this};
			auto left = begin();
			auto mid = left + size() / 2;
			auto right = end();
			return {
				NaiveNodeSet((*graph), left, mid),
				NaiveNodeSet((*graph), mid, right)
			};
		}

		// -- topological operations --

		// https://en.wikipedia.org/wiki/Topological_space

		// https://en.wikipedia.org/wiki/Interior_(topology)

		NaiveNodeSet interior(int steps = 1) const {
			if (steps == 0) return *this;

			NaiveNodeSet res((*graph));
			for(const auto& cur : nodes) {
				if (containsAll((*graph)[cur].neighbors, nodes)) {
					res.nodes.push_back(cur);
				}
			}
			return res.interior(steps-1);
		}

		// https://en.wikipedia.org/wiki/Closure_(topology)

		NaiveNodeSet closure(int steps = 1) const {
			if (steps == 0) return *this;

			std::set<node_ref_type> refs;
			for(const auto& cur : nodes) {
				refs.insert(cur);
				for(const auto& adj : (*graph)[cur].neighbors) {
					refs.insert(adj);
				}
			}
			return NaiveNodeSet((*graph), refs.begin(), refs.end()).closure(steps-1);
		}

		// https://en.wikipedia.org/wiki/Boundary_(topology)

		NaiveNodeSet boundary() const {
			return closure() - *this;
		}

		// https://de.wikipedia.org/wiki/Durchmesser#Durchmesser_in_metrischen_R.C3.A4umen

		int radius() const {
			int max = std::numeric_limits<int>::max();

			if (empty()) return 0;
			if (boundary().empty()) return max;

			// there might be an unconnected component part of this set
			auto res = interior().radius();
			return (res == max) ? max : res + 1;
		}


		// -- membership test --

		bool contains(const node_ref_type& node) const {
			return std::find(nodes.begin(), nodes.end(), node) != nodes.end();
		}

		// -- sub-set test --

		bool subsetOf(const NaiveNodeSet& other) const {
			for(const auto& cur : nodes) {
				if (!other.contains(cur)) return false;
			}
			return true;
		}


		bool operator==(const NaiveNodeSet& other) const {
			return nodes == other.nodes;
		}

		bool operator!=(const NaiveNodeSet& other) const {
			return !(*this == other);
		}

		// -- set disjunct union --

		NaiveNodeSet operator+(const NaiveNodeSet& other) const {
			return NaiveNodeSet(*this) += other;
		}

		NaiveNodeSet& operator+=(const NaiveNodeSet& other) {
			for(const auto& cur : other) nodes.push_back(cur);
			return *this;
		}


		// -- set union --

		NaiveNodeSet operator|(const NaiveNodeSet& other) const {
			return NaiveNodeSet(*this) |= other;
		}

		NaiveNodeSet& operator|=(const NaiveNodeSet& other) {
			for(const auto& cur : other.nodes) {
				if (!contains(cur)) nodes.push_back(cur);
			}
			std::sort(nodes.begin(), nodes.end());
			return *this;
		}

		// -- set intersection --

		NaiveNodeSet operator&(const NaiveNodeSet& other) const {
			return NaiveNodeSet(*this) &= other;
		}

		NaiveNodeSet& operator&=(const NaiveNodeSet& other) {
			node_collection_type res;
			for(const auto& cur : nodes) {
				if (other.contains(cur)) res.push_back(cur);
			}
			nodes.swap(res);
			std::sort(nodes.begin(), nodes.end());
			return *this;
		}


		// -- set difference --

		NaiveNodeSet operator-(const NaiveNodeSet& other) const {
			return NaiveNodeSet(*this) -= other;
		}

		NaiveNodeSet& operator-=(const NaiveNodeSet& other) {
			node_collection_type res;
			for(const auto& cur : nodes) {
				if (!other.contains(cur)) res.push_back(cur);
			}
			nodes.swap(res);
			return *this;
		}

		friend std::ostream& operator<<(std::ostream& out, const NaiveNodeSet& set) {
			return out << set.nodes;
		}
	};


	template<
		typename static_data,
		typename dynamic_data
	>
	struct NaiveTopology<node_info<static_data,dynamic_data>> {

		using set_type = NaiveNodeSet<static_data,dynamic_data>;
		using ref_type = NaiveNodeReference<static_data,dynamic_data>;

		using node_type = NaiveNode<static_data,dynamic_data>;
		using node_ref_type = ref_type;

		using topology_data = std::vector<node_type>;
		using static_node_data = std::vector<static_data>;
		using dynamic_node_data = std::vector<dynamic_data>;

	private:

		topology_data nodes;

	public:

		node_ref_type createNode(const static_data& s, const dynamic_data& d) {
			nodes.emplace_back(s,d);
			return nodes.size() - 1;
		}

		node_ref_type createBorderNode(const static_data& s, const dynamic_data& d) {
			nodes.emplace_back(s,d,true);
			return nodes.size() - 1;
		}

		void addEdge(node_ref_type a, node_ref_type b) {
			if (!contains(nodes[a].neighbors, b)) {
				nodes[a].neighbors.push_back(b);
				nodes[b].neighbors.push_back(a);
			}
		}

		const topology_data& getTopologyData() const {
			return nodes;
		}

		set_type partition() const {
			std::vector<ref_type> nds(nodes.size());
			for(std::size_t i = 0; i<nodes.size(); i++) {
				nds[i] = i;
			}
			return set_type(nodes, nds);
		}

		static_node_data createStaticData() const {
			static_node_data res;
			for(const auto& cur : nodes) {
				res.push_back(cur.initial_static_data);
			}
			return res;
		}

		dynamic_node_data createDynamicData() const {
			dynamic_node_data res;
			for(const auto& cur : nodes) {
				res.push_back(cur.initial_dynamic_data);
			}
			return res;
		}

	};


} // end namespace graph
} // end namespace utils
} // end namespace allscale
