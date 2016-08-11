#pragma once

#include <algorithm>
#include <iterator>
#include <typeindex>
#include <map>

#include "parec/ops.h"
#include "utils/io_utils.h"
#include "allscale/utils/range.h"

#include "allscale/utils/mesh/reference_implementation.h"
#include "allscale/utils/mesh/partition_tree.h"
#include "allscale/fine_open/fine_open_mesh_partition.h"

namespace allscale {
namespace utils {
namespace mesh {
namespace partitioned {

	template<typename Kind,unsigned Level>
	using NodeRef = reference::NodeRef<Kind,Level>;

	template<typename Kind,unsigned Level>
	using NodeSet = reference::NodeSet<Kind,Level>;

	template<typename Kind,unsigned Level>
	using NodeList = utils::range<const NodeRef<Kind,Level>*>;

	template<
		unsigned Layers,
		typename NodeTypes,
		typename EdgeTypes,
		typename Hierarchies
	>
	class Builder;


	template<
		unsigned Levels,
		typename Nodes,
		typename Edges,
		typename Hierarchies
	> struct MeshData;

	namespace detail {

		template<unsigned Level, typename ... NodeTypes>
		class NodeSet;

		template<unsigned Level, typename First, typename ... Rest>
		class NodeSet<Level,First,Rest...> {

			std::size_t node_counter;

			NodeSet<Level,Rest...> nested;

		public:

			NodeSet(const reference::detail::NodeSet<Level,First,Rest...>& other)
				: node_counter(other.node_counter), nested(other.nested) {}

			NodeSet(NodeSet&& other)
				: node_counter(other.node_counter), nested(std::move(other.nested)) {}

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

			NodeRef<First,Level> create() {
				return { node_counter++ };
			}

			std::size_t numNodes() const {
				return node_counter;
			}

			bool operator==(const NodeSet& other) const {
				return node_counter == other.node_counter && nested == other.nested;
			}

			void store(std::ostream& out) const {
				assert(false && "Not implemented!");

//				// store the number of nodes
//				write<std::size_t>(out, node_counter);
//
//				// store the nested hierarchy
//				nested.store(out);
			}

			static NodeSet load(std::istream& in) {
				assert(false && "Not implemented!");

//				NodeSet res;
//
//				// restore the number of nodes
//				res.node_counter = read<std::size_t>(in);
//
//				// load nested
//				res.nested = NodeSet<Level,Rest...>::load(in);
//
//				// done
//				return res;

				return NodeSet( reference::detail::NodeSet<Level,First,Rest...>() );
			}
		};

		template<unsigned Level>
		class NodeSet<Level> {
		public:

			NodeSet(const reference::detail::NodeSet<Level>& ) {}

			NodeSet(NodeSet&& ) {}

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

			bool operator==(const NodeSet& other) const {
				return true;
			}

			void store(std::ostream& out) const {
				// nothing
			}

			static NodeSet load(std::istream& in) {
				return NodeSet();
			}
		};



		template<unsigned Level, typename ... EdgeTypes>
		class EdgeSet;

		template<unsigned Level, typename First, typename ... Rest>
		class EdgeSet<Level,First,Rest...> {

			using Src = typename First::src_node_type;
			using Trg = typename First::trg_node_type;
			using TrgRef = NodeRef<Trg,Level>;
			using range_type = NodeList<Trg,Level>;

			// TODO: re-format edge representation
			std::vector<reference::NodeID> offsets;			// the offsets to the target vector
			std::vector<TrgRef> targets;			// the edge targets

			EdgeSet<Level,Rest...> nested;

		public:

			EdgeSet(const reference::detail::EdgeSet<Level,First,Rest...>& other)
				: offsets(other.forward_offsets), targets(other.forward_targets), nested(other.nested) {}

			EdgeSet(EdgeSet&& other)
				: offsets(std::move(other.offsets)),
				  targets(std::move(other.targets)),
				  nested(std::move(other.nested)) {}


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

			range_type getSinks(const NodeRef<Src,Level>& src) const {
				if (src.id >= offsets.size() - 1) return range_type {nullptr, nullptr};
				return range_type {&targets[offsets[src.id]], &targets[offsets[src.id+1]]};
			}

			bool operator==(const EdgeSet& other) const {
				return offsets == other.offsets && targets == other.targets && nested == other.nested;
			}

			void store(std::ostream& out) const {
				assert(false && "Not implemented");
				/*
				// store this hierarchy data
				write<std::size_t>(out, forward.size());
				for(const auto& cur : forward) {
					write<std::size_t>(out, cur.size());
					for(const auto& n : cur) {
						write<std::size_t>(out, n.id);
					}
				}

				// store the nested hierarchy
				nested.store(out);
				*/
			}

			static EdgeSet load(std::istream& in) {

				assert(false && "Not implemented");
				/*
				// restore this edge set
				EdgeSet res;

				// load edge data
				std::size_t size = read<std::size_t>(in);
				res.forward.resize(size);
				for(std::size_t i=0; i<size; i++) {
					std::size_t cur_size = read<std::size_t>(in);
					res.forward[i].resize(cur_size);
					for(std::size_t j=0; j<cur_size; j++) {
						res.forward[i][j] = NodeRef<Trg,Level> { read<std::size_t>(in) };
					}
				}

				// fill backward edges
				for(std::size_t i = 0; i<size; i++) {
					for(const auto& trg : res.forward[i]) {
						res.addBackwardEdge(NodeRef<Src,Level>{i},trg);
					}
				}

				// load nested
				res.nested = EdgeSet<Level,Rest...>::load(in);

				// done
				return res;
				*/
				return EdgeSet(reference::detail::EdgeSet<Level,First,Rest...>());
			}
		};

		template<unsigned Level>
		class EdgeSet<Level> {
		public:

			EdgeSet(const reference::detail::EdgeSet<Level>& other) {}

			EdgeSet(EdgeSet&& other) = default;

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

			bool operator==(const EdgeSet& other) const {
				return true;
			}

			template<typename Body>
			void forAll(const Body& body) const {
				// nothing to do
			}

			void store(std::ostream& out) const {
				// nothing
			}

			static EdgeSet load(std::istream& in) {
				return EdgeSet();
			}
		};

		template<unsigned Level, typename ... HierachyTypes>
		class HierarchySet;

		template<unsigned Level, typename First, typename ... Rest>
		class HierarchySet<Level,First,Rest...> {

			using Src = typename First::parent_node_type;
			using Trg = typename First::child_node_type;
			using TrgRef = NodeRef<Trg,Level-1>;
			using range_type = NodeList<Trg,Level-1>;

			std::vector<size_t> offsets;
			std::vector<TrgRef> children;

			HierarchySet<Level,Rest...> nested;

		public:

			HierarchySet(const reference::detail::HierarchySet<Level,First,Rest...>& other) : nested(other.nested) {

				// create offset
				offsets.resize(other.children.size() + 1);

				// fill offsets
				int pos = 0;
				std::size_t offset = 0;
				for(const auto& cur : other.children) {
					offsets[pos++] = offset;
					offset += cur.size();
				}
				offsets[pos] = offset;

				// create targets
				children.resize(offset);

				// fill targets
				pos = 0;
				for(const auto& cur : other.children) {
					for(const auto& trg : cur) {
						children[pos++] = trg;
					}
				}
			}


			HierarchySet(HierarchySet&& other)
				: offsets(std::move(other.offsets)),
				  children(std::move(other.children)),
				  nested(std::move(other.nested)) {}

			template<typename T>
			typename std::enable_if<std::is_same<First,T>::value,HierarchySet&>::type get() {
				return *this;
			}

			template<typename T>
			typename std::enable_if<std::is_same<First,T>::value,const HierarchySet&>::type get() const {
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

			range_type getChildren(const NodeRef<Src,Level>& parent) const {
				if (parent.id >= offsets.size()-1) return range_type{nullptr, nullptr};
				return range_type{&children[offsets[parent.id]], &children[offsets[parent.id+1]]};
			}

			bool operator==(const HierarchySet& other) const {
				return offsets == other.offsets && children == other.children && nested == other.nested;
			}

			void store(std::ostream& out) const {
				assert(false && "Not implemented yet");
				/*
				// store this hierarchy data
				write<std::size_t>(out, children.size());
				for(const auto& cur : children) {
					write<std::size_t>(out, cur.size());
					for(const auto& n : cur) {
						write<std::size_t>(out, n.id);
					}
				}

				// store the nested hierarchy
				nested.store(out);
				*/
			}

			static HierarchySet load(std::istream& in) {
				assert(false && "Not implemented yet");
				/*
				// restore this hierarchy
				HierarchySet res;

				// load hierarchy data
				std::size_t size = read<std::size_t>(in);
				for(std::size_t i=0; i<size; i++) {
					NodeRef<Src,Level> parent{i};
					std::size_t cur_size = read<std::size_t>(in);
					for(std::size_t j=0; j<cur_size; j++) {
						NodeRef<Trg,Level-1> child{ read<std::size_t>(in) };
						res.addChild(parent,child);
					}
				}

				// load nested
				res.nested = HierarchySet<Level,Rest...>::load(in);

				// done
				return res;
				*/
				return HierarchySet(reference::detail::HierarchySet<Level,First,Rest...>());
			}
		};

		template<unsigned Level>
		class HierarchySet<Level> {
		public:

			HierarchySet(const reference::detail::HierarchySet<Level>& other) {}

			HierarchySet(HierarchySet&& other) = default;

			template<typename T>
			HierarchySet& get() {
				assert(false && "Invalid type accessed!");
				return *this;
			}

			template<typename T>
			const HierarchySet& get() const {
				assert(false && "Invalid type accessed!");
				return *this;
			}

			bool operator==(const HierarchySet& other) const {
				return true;
			}

			template<typename Body>
			void forAll(const Body& body) const {
				// nothing to do
			}

			void store(std::ostream& out) const {
				// nothing
			}

			static HierarchySet load(std::istream& in) {
				return HierarchySet();
			}
		};

		template<
			unsigned NumLevels,
			typename Nodes,
			typename Edges,
			typename Hierarchies
		>
		struct Levels;

		template<
			unsigned Level,
			typename ... Nodes,
			typename ... Edges,
			typename ... Hierarchies
		>
		struct Levels<
				Level,
				nodes<Nodes...>,
				edges<Edges...>,
				hierarchies<Hierarchies...>
			> {

			template<unsigned Lvl>
			using LevelData = Levels<Lvl,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

			using ref_level_type = reference::detail::Levels<Level,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

			// the data of the lower levels
			LevelData<Level-1> nested;

			// the set of nodes on this level
			NodeSet<Level,Nodes...> m_nodes;

			// the set of edges on this level
			EdgeSet<Level,Edges...> m_edges;

			// the set of hierarchies connecting this level to the sub-level
			HierarchySet<Level,Hierarchies...> m_hierarchies;

			Levels(const ref_level_type& other)
				: nested(other.nested),
				  m_nodes(other.nodes),
				  m_edges(other.edges),
				  m_hierarchies(other.hierarchies)
			{}

			Levels(Levels&& other)
				: nested(std::move(other.nested)),
				  m_nodes(std::move(other.m_nodes)),
				  m_edges(std::move(other.m_edges)),
				  m_hierarchies(std::move(other.m_hierarchies))
			{}

			Levels(LevelData<Level-1>&& nested, NodeSet<Level,Nodes...>&& nodes, EdgeSet<Level,Edges...>&& edges, HierarchySet<Level,Hierarchies...>&& hierarchies)
				: nested(std::move(nested)),
				  m_nodes(std::move(nodes)),
				  m_edges(std::move(edges)),
				  m_hierarchies(std::move(hierarchies))
			{}

			template<unsigned Lvl>
			typename std::enable_if<Lvl == Level, LevelData<Lvl>&>::type
			getLevel() {
				return *this;
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl != Level, LevelData<Lvl>&>::type
			getLevel() {
				return nested.template getLevel<Lvl>();
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl == Level, const LevelData<Lvl>&>::type
			getLevel() const {
				return *this;
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl != Level, const LevelData<Lvl>&>::type
			getLevel() const {
				return nested.template getLevel<Lvl>();
			}

			bool operator==(const Levels& other) const {
				return m_nodes == other.m_nodes && m_edges == other.m_edges && nested == other.nested && m_hierarchies == other.m_hierarchies;
			}
/*
			template<typename Body>
			void forAllEdges(const Body& body) const {
				edges.forAll(body);
				nested.forAllEdges(body);
			}

			template<typename Body>
			void forAllHierarchies(const Body& body) const {
				hierarchies.forAll(body);
				nested.forAllHierarchies(body);
			}
*/
			void store(std::ostream& out) const {
				nested.store(out);
				m_nodes.store(out);
				m_edges.store(out);
				m_hierarchies.store(out);
			}

			static Levels load(std::istream& in) {
				auto nested = LevelData<Level-1>::load(in);
				auto nodes = NodeSet<Level,Nodes...>::load(in);
				auto edges = EdgeSet<Level,Edges...>::load(in);
				auto hierarchies = HierarchySet<Level,Hierarchies...>::load(in);
				return Levels ( std::move(nested), std::move(nodes), std::move(edges), std::move(hierarchies) );
			}
		};

		template<
			typename ... Nodes,
			typename ... Edges,
			typename ... Hierarchies
		>
		struct Levels<0,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>> {

			template<unsigned Lvl>
			using LevelData = Levels<Lvl,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

			using ref_level_type = reference::detail::Levels<0,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

			// the set of nodes on this level
			NodeSet<0,Nodes...> m_nodes;

			// the set of edges on this level
			EdgeSet<0,Edges...> m_edges;

			Levels(const ref_level_type& other)
				: m_nodes(other.nodes),
				  m_edges(other.edges)
			{}


			Levels(Levels&& other)
				: m_nodes(std::move(other.m_nodes)),
				  m_edges(std::move(other.m_edges))
			{}

			Levels(NodeSet<0,Nodes...>&& nodes, EdgeSet<0,Edges...>&& edges)
				: m_nodes(std::move(nodes)), m_edges(std::move(edges)) {}

			template<unsigned Lvl>
			typename std::enable_if<Lvl == 0, LevelData<Lvl>&>::type
			getLevel() {
				return *this;
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl == 0, const LevelData<Lvl>&>::type
			getLevel() const {
				return *this;
			}

			bool operator==(const Levels& other) const {
				return m_nodes == other.m_nodes && m_edges == other.m_edges;
			}

			void store(std::ostream& out) const {
				m_nodes.store(out);
				m_edges.store(out);
			}

			static Levels load(std::istream& in) {
				auto nodes = NodeSet<0,Nodes...>::load(in);
				auto edges = EdgeSet<0,Edges...>::load(in);
				return Levels ( std::move(nodes), std::move(edges) );
			}
		};

	}


	template<
		unsigned Levels,
		typename ... Nodes,
		typename ... Edges,
		typename ... Hierarchies
	>
	struct MeshData<Levels,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>> {

		using input_mesh_data = reference::MeshData<Levels,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

		using data_store = detail::Levels<Levels,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

		// all the topological data of all the nodes, edges and hierarchy relations on all levels
		data_store data;

		MeshData(const input_mesh_data& data) : data(data.data) { }

		MeshData(data_store&& data) : data(std::move(data)) {}

		template<unsigned Level>
		detail::NodeSet<Level,Nodes...>& getNodes() {
			return data.template getLevel<Level>().m_nodes;
		}

		template<unsigned Level>
		const detail::NodeSet<Level,Nodes...>& getNodes() const {
			return data.template getLevel<Level>().m_nodes;
		}

		template<unsigned Level>
		detail::EdgeSet<Level,Edges...>& getEdges() {
			return data.template getLevel<Level>().m_edges;
		}

		template<unsigned Level>
		const detail::EdgeSet<Level,Edges...>& getEdges() const {
			return data.template getLevel<Level>().m_edges;
		}

		template<unsigned Level>
		detail::HierarchySet<Level,Hierarchies...>& getHierarchies() {
			return data.template getLevel<Level>().m_hierarchies;
		}

		template<unsigned Level>
		const detail::HierarchySet<Level,Hierarchies...>& getHierarchies() const {
			return data.template getLevel<Level>().m_hierarchies;
		}

		template<typename Body>
		void forAllEdges(const Body& body) const {
			data.forAllEdges(body);
		}

		template<typename Body>
		void forAllHierarchies(const Body& body) const {
			data.forAllHierarchies(body);
		}

		bool operator==(const MeshData& other) const {
			return data == other.data;
		}

		void store(std::ostream& out) const {
			// store nested data
			data.store(out);
		}

		static MeshData load(std::istream& in) {
			return MeshData ( data_store::load(in) );
		}

	};

	template<
		unsigned Layers,
		typename NodeTypes,
		typename EdgeTypes,
		typename Hierarchies,
		typename NodeCollection = NodeRange,
		unsigned PartitionLevels = 16
	>
	class Mesh {

	public:

		using reference_mesh_type = reference::Mesh<Layers,NodeTypes,EdgeTypes,Hierarchies>;
		using partition_table_type = fine_open::MeshNodePartition<reference_mesh_type>;

		using reference_data_type = reference::MeshData<Layers-1,NodeTypes,EdgeTypes,Hierarchies>;
		using data_type = MeshData<Layers-1,NodeTypes,EdgeTypes,Hierarchies>;

		using tree_type = PartitionTree<reference_mesh_type,PartitionLevels>;

	private:

		tree_type partitions;
		data_type data;

	public:

		using builder_type = MeshBuilder<NodeTypes,EdgeTypes,Hierarchies,Layers,Builder>;

		enum { layers = Layers };

		template<typename Kind,unsigned Level = 0>
		using node_ref_type = NodeRef<Kind,Level>;

		template<typename Kind,unsigned Level = 0>
		using node_set_type = NodeSet<Kind,Level>;

		template<typename Kind, unsigned Level, typename Data>
		using data_container = reference::DataContainer<Kind,Level,Data>;

		Mesh(const reference_data_type& data = reference_data_type())
			: partitions(partition_table_type(reference_mesh_type(data))), data(data) {}

//		Mesh(const tree_type& partitions, const data_type& data = data_type())
//			: partitions(partitions), data(data) {}

		Mesh(tree_type&& partitions, data_type&& data)
			: partitions(std::move(partitions)), data(std::move(data)) {}

		Mesh(const Mesh&) = default;
		Mesh(Mesh&&) = default;
		Mesh(const reference_mesh_type& mesh, const partition_table_type& partition)
			: partitions(partition), data(mesh.getData()) {}

		// -- general operations --

		bool operator==(const Mesh& other) const {
			return data == other.data;
		}

		bool operator!=(const Mesh& other) const {
			return data != other.data;
		}

		// -- mesh querying --

		template<typename Kind,unsigned Level = 0>
		std::size_t numNodes() const {
			return data.template getNodes<Level>().template get<Kind>().numNodes();
		}


		// -- mesh processing --

		template<
			typename EdgeKind,
			typename A,
			unsigned Level,
			typename B = typename EdgeKind::trg_node_type
		>
		node_ref_type<B,Level> getNeighbor(const node_ref_type<A,Level>& a) const {
			const auto& set = getNeighbors<EdgeKind>(a);
			assert(set.size() == 1);
			return set.front();
		}

		template<
			typename EdgeKind,
			typename A,
			unsigned Level,
			typename B = typename EdgeKind::trg_node_type
		>
		NodeList<B,Level> getNeighbors(const node_ref_type<A,Level>& a) const {
			return getSinks<EdgeKind,A,Level,B>(a);
		}

		template<
			typename EdgeKind,
			typename A,
			unsigned Level,
			typename B = typename EdgeKind::trg_node_type
		>
		NodeList<B,Level> getSinks(const node_ref_type<A,Level>& a) const {
			return data.template getEdges<Level>().template get<EdgeKind>().getSinks(a);
		}

		template<
			typename Hierarchy,
			typename A, unsigned Level,
			typename B = typename Hierarchy::child_node_type
		>
		NodeList<B,Level-1> getChildren(const node_ref_type<A,Level>& a) const {
			return data.template getHierarchies<Level>().template get<Hierarchy>().getChildren(a);
		}


		template<typename Kind, unsigned Level = 0, typename Body>
		void forAll(const Body& body) const {
			// run a loop over all nodes of the requested kind sequentially
			for(const auto& cur : node_set_type<Kind,Level>::range(NodeRef<Kind,Level>{0},NodeRef<Kind,Level>{numNodes<Kind,Level>()})) {
				body(cur);
			}
		}

		template<typename Kind, unsigned Level = 0, typename Body>
		void pforAll(const Body& body) const {
			// run a loop over all nodes of the requested kind in parallel
			parec::pfor(node_set_type<Kind,Level>::range(NodeRef<Kind,Level>{0},NodeRef<Kind,Level>{numNodes<Kind,Level>()}), body);
		}

		template<typename Body>
		void forAllEdges(const Body& body) const {
			data.forAllEdges(body);
		}

		template<typename Body>
		void forAllHierarchies(const Body& body) const {
			data.forAllHierarchies(body);
		}

		template<typename Body>
		void forAllLinks(const Body& body) const {
			forAllEdges(body);
			forAllHierarchies(body);
		}

		// -- graph data --

		template<typename NodeKind, typename T, unsigned Level = 0>
		data_container<NodeKind,Level,T> createNodeData() const {
			return data_container<NodeKind,Level,T>(numNodes<NodeKind,Level>());
		}

		// -- load / store --

		void store(std::ostream& out) const {

			// print magic number
			write<int>(out, MAGIC_NUMBER);

			// print number of layers, nodes, edges, and hierarchies
			write<int>(out, layers);
			write<int>(out, NodeTypes::size);
			write<int>(out, EdgeTypes::size);
			write<int>(out, Hierarchies::size);

			// store mesh data
			data.store(out);

		}

		static Mesh load(std::istream& in) {

			// check magic number
			int magic = read<int>(in);
			if (magic != MAGIC_NUMBER) {
				std::cout << "Invalid input format - aborted!\n";
				exit(1);
			}

			// check number type parameters
			int in_layers = read<int>(in);
			if (layers != in_layers) {
				std::cout << "Invalid input format - aborted!\n";
				exit(1);
			}

			int num_node_types = read<int>(in);
			if (NodeTypes::size != num_node_types) {
				std::cout << "Invalid input format - aborted!\n";
				exit(1);
			}

			int num_edge_types = read<int>(in);
			if (EdgeTypes::size != num_edge_types) {
				std::cout << "Invalid input format - aborted!\n";
				exit(1);
			}

			int num_hierarchies = read<int>(in);
			if (Hierarchies::size != num_hierarchies) {
				std::cout << "Invalid input format - aborted!\n";
				exit(1);
			}

			// load the mesh
			assert(false && "Not implemented!");
			auto& partitions = *((tree_type*)(nullptr));

			auto data = data_type::load(in);
			return Mesh(std::move(partitions),std::move(data));
		}

	private:

		const static int MAGIC_NUMBER = 0x4875368;		// some random number

	};


	template<
		unsigned Layers,
		typename NodeTypes,
		typename EdgeTypes,
		typename Hierarchies
	>
	class Builder {

		reference::MeshData<Layers-1,NodeTypes,EdgeTypes,Hierarchies> data;

	public:

		// -- type definitions --

		template<typename Kind,unsigned Level>
		using node_ref_type = NodeRef<Kind,Level>;

		template<typename Kind,unsigned Level>
		using node_list_type = reference::ContinousNodeList<Kind,Level>;

		using mesh_type = Mesh<Layers,NodeTypes,EdgeTypes,Hierarchies>;

		// -- mesh modeling --

		template<typename Kind,unsigned Level>
		node_ref_type<Kind,Level> create() {
			return data.template getNodes<Level>().template get<Kind>().create();
		}

		template<typename EdgeKind,unsigned Level>
		void link(const node_ref_type<typename EdgeKind::src_node_type,Level>& a, const node_ref_type<typename EdgeKind::trg_node_type,Level>& b) {
			// link the two edges
			data.template getEdges<Level>().template get<EdgeKind>().addEdge(a,b);
		}

		template<typename Hierarchy,unsigned LevelA, unsigned LevelB>
		void link(const node_ref_type<typename Hierarchy::parent_node_type,LevelA>& a, const node_ref_type<typename Hierarchy::child_node_type,LevelB>& b) {
			// link the two edges
			data.template getHierarchies<LevelA>().template get<Hierarchy>().addChild(a,b);
		}

		mesh_type build() {
			data.close();
			return mesh_type(data);
		}

	};


} // end namespace partitioned
} // end namespace mesh
} // end namespace utils
} // end namespace allscale
