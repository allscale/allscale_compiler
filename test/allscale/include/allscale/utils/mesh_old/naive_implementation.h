#pragma once

#include <algorithm>
#include <typeindex>
#include <map>

namespace allscale {
namespace utils {
namespace mesh {
namespace naive {

	template<typename Kind>
	struct NodeRef {
		std::size_t id;

		bool operator==(const NodeRef& other) const {
			return id == other.id;
		}

		bool operator<(const NodeRef& other) const {
			return id < other.id;
		}

		friend std::ostream& operator<<(std::ostream& out, const NodeRef& ref) {
			return out << "n" << ref.id;
		}
	};

	template<typename Kind>
	class NodeSet {

		using set_type = std::set<NodeRef<Kind>>;

		std::size_t m_begin;
		std::size_t m_end;

	public:


		class const_iterator {

			std::size_t cur;

		public:

			const_iterator(std::size_t pos) : cur(pos) {}

			bool operator==(const const_iterator& other) const {
				return cur == other.cur;
			}

			bool operator!=(const const_iterator& other) const {
				return cur != other.cur;
			}

			bool operator<(const const_iterator& other) const {
				return cur < other.cur;
			}

			std::size_t operator-(const const_iterator& other) const {
				return cur - other.cur;
			}

			const_iterator& operator+=(std::size_t offset) {
				cur += offset;
				return *this;
			}

			NodeRef<Kind> operator*() const {
				return NodeRef<Kind>{cur};
			}

			const_iterator& operator++() {
				++cur;
				return *this;
			}

		};

		NodeSet() : m_begin(0), m_end(0) {}

		NodeSet(std::size_t begin, std::size_t end)
			: m_begin(begin), m_end(end) {}

		NodeSet(const NodeSet&) = default;
		NodeSet(NodeSet&&) = default;

		NodeSet& operator=(const NodeSet&) = default;
		NodeSet& operator=(NodeSet&&) = default;

		bool empty() const {
			return m_begin == m_end;
		}

		std::size_t size() const {
			return m_end - m_begin;
		}

		const_iterator begin() const {
			return const_iterator(m_begin);
		}

		const_iterator end() const {
			return const_iterator(m_end);
		}

	private:

		template<typename N, typename E>
		friend class Mesh;

		static NodeSet range(std::size_t a, std::size_t b) {
			return NodeSet(a,std::max(a,b));
		}


	};

	template<
		typename Nodes,
		typename Edges
	> struct MeshData;

	namespace detail {

		template<typename ... NodeTypes>
		struct NodeSet;

		template<typename First, typename ... Rest>
		class NodeSet<First,Rest...> {

			std::size_t node_counter;

			NodeSet<Rest...> nested;

		public:

			NodeSet() : node_counter(0) {}

			template<typename T>
			typename std::enable_if<std::is_same<First,T>::value,NodeSet&>::type get() {
				return *this;
			}

			template<typename T>
			typename std::enable_if<std::is_same<First,T>::value,const NodeSet&>::type get() const {
				return *this;
			}

			template<typename T>
			auto get() -> typename std::enable_if<!std::is_same<First,T>::value,decltype(nested.template get<T>())>::type {
				return nested.template get<T>();
			}

			template<typename T>
			auto get() const -> typename std::enable_if<!std::is_same<First,T>::value,decltype(nested.template get<T>())>::type {
				return nested.template get<T>();
			}

			NodeRef<First> create() {
				return { node_counter++ };
			}

			std::size_t numNodes() const {
				return node_counter;
			}

		};

		template<>
		struct NodeSet<> {

			template<typename T>
			NodeSet& get() {
				assert(false && "Invalid type accessed!");
				return *this;
			}

			template<typename T>
			const NodeSet& get() const {
				assert(false && "Invalid type accessed!");
				return *this;
			}

		};

		template<typename ... EdgeTypes>
		struct EdgeSet;

		template<typename First, typename ... Rest>
		class EdgeSet<First,Rest...> {

			using Src = typename First::src_node_type;
			using Trg = typename First::trg_node_type;

			std::vector<std::vector<NodeRef<Trg>>> edges;

			EdgeSet<Rest...> nested;

		public:

			EdgeSet() {}

			template<typename T>
			typename std::enable_if<std::is_same<First,T>::value,EdgeSet&>::type get() {
				return *this;
			}

			template<typename T>
			typename std::enable_if<std::is_same<First,T>::value,const EdgeSet&>::type get() const {
				return *this;
			}

			template<typename T>
			auto get() -> typename std::enable_if<!std::is_same<First,T>::value,decltype(nested.template get<T>())>::type {
				return nested.template get<T>();
			}

			template<typename T>
			auto get() const -> typename std::enable_if<!std::is_same<First,T>::value,decltype(nested.template get<T>())>::type {
				return nested.template get<T>();
			}

			void addEdge(const NodeRef<Src>& src, const NodeRef<Trg>& trg) {
				while(src.id >= edges.size()) {
					edges.resize(std::max<std::size_t>(10, edges.size() * 2));
				}
				auto& list = edges[src.id];
				for(auto& cur : list) if (cur == trg) return;
				list.push_back(trg);
			}

			const std::vector<NodeRef<Trg>>& getNeighbors(const NodeRef<Src>& src) const {
				static const std::vector<NodeRef<Trg>> empty;
				if (src.id >= edges.size()) return empty;
				return edges[src.id];
			}

		};

		template<>
		struct EdgeSet<> {

			template<typename T>
			EdgeSet& get() {
				assert(false && "Invalid type accessed!");
				return *this;
			}

			template<typename T>
			const EdgeSet& get() const {
				assert(false && "Invalid type accessed!");
				return *this;
			}

		};


	}


	template<
		typename ... Nodes,
		typename ... Edges
	>
	struct MeshData<nodes<Nodes...>,edges<Edges...>> {

		// nodes are just indexed by node type
		detail::NodeSet<Nodes...> nodes;

		// the present edges
		detail::EdgeSet<Edges...> edges;

	};

	template<typename Kind, typename Data>
	struct DataContainer {

		std::vector<Data> data;

	public:

		DataContainer(std::size_t size) : data(size) {}

		std::size_t size() const {
			return data.size();
		}

		Data& operator[](const NodeRef<Kind>& index) {
			return data[index.id];
		}

		const Data& operator[](const NodeRef<Kind>& index) const {
			return data[index.id];
		}

	};


	template<
		typename NodeTypes,
		typename EdgeTypes
	>
	class Mesh {

		MeshData<NodeTypes,EdgeTypes> data;

	public:

		template<typename Kind>
		using node_ref_type = NodeRef<Kind>;

		template<typename Kind>
		using node_set_type = NodeSet<Kind>;

		template<typename Kind, typename Data>
		using data_container = DataContainer<Kind,Data>;


		Mesh(const MeshData<NodeTypes,EdgeTypes>& data) : data(data) {}

		// -- mesh querying --

		template<typename Kind>
		std::size_t numNodes() const {
			return data.nodes.template get<Kind>().numNodes();
		}


		// -- mesh processing --

		template<
			typename EdgeKind,
			typename A,
			typename B = typename EdgeKind::trg_node_type
		>
		node_ref_type<B> getNeighbor(const node_ref_type<A>& a) const {
			auto& set = getNeighbors<EdgeKind>(a);
			assert(set.size() == 1);
			return set.front();
		}

		template<
			typename EdgeKind,
			typename A,
			typename B = typename EdgeKind::trg_node_type
		>
		const std::vector<node_ref_type<B>>& getNeighbors(const node_ref_type<A>& a) const {
			return data.edges.template get<EdgeKind>().getNeighbors(a);
		}

		/**
		 * Creates a decomposable description of the set of included nodes.
		 */
		template<typename Kind>
		node_set_type<Kind> partition() const {
			return node_set_type<Kind>::range(0,numNodes<Kind>());
		}


		// -- graph data --

		template<typename NodeKind, typename T>
		data_container<NodeKind,T> createNodeData() const {
			return data_container<NodeKind,T>(numNodes<NodeKind>());
		}

	};


	template<
		typename NodeTypes,
		typename EdgeTypes
	>
	class Builder {

		MeshData<NodeTypes,EdgeTypes> data;

	public:

		// -- type definitions --

		template<typename Kind>
		using node_ref_type = NodeRef<Kind>;

		using mesh_type = Mesh<NodeTypes,EdgeTypes>;

		// -- mesh modeling --

		template<typename Kind>
		node_ref_type<Kind> create() {
			return data.nodes.template get<Kind>().create();
		}

		template<typename EdgeKind>
		void link(const node_ref_type<typename EdgeKind::src_node_type>& a, const node_ref_type<typename EdgeKind::trg_node_type>& b) {
			// link the two edges
			data.edges.template get<EdgeKind>().addEdge(a,b);
		}

		mesh_type build() const {
			return mesh_type(data);
		}

	};


} // end namespace naive
} // end namespace mesh
} // end namespace utils
} // end namespace allscale
