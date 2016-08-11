#pragma once

#include <algorithm>
#include <atomic>
#include <cassert>
#include <iterator>
#include <typeindex>
#include <map>

#include "parec/ops.h"
#include "utils/io_utils.h"

#include "allscale/utils/range.h"
#include "allscale/utils/prefix_sum.h"

namespace allscale {
namespace utils {
namespace mesh {
namespace reference {

	using NodeID = uint32_t;

	template<typename Kind,unsigned Level>
	struct NodeRef {

		using node_type = Kind;
		enum { level = Level };

		NodeID id;

		NodeRef() = default;

		NodeRef(std::size_t id) : id(id) {}

		NodeID getOrdinal() const {
			return id;
		}

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

	template<typename Kind,unsigned Level>
	class NodeSet {

		using set_type = std::set<NodeRef<Kind,Level>>;

		std::size_t m_begin;
		std::size_t m_end;

	public:


		class const_iterator : public std::iterator<std::random_access_iterator_tag, std::size_t> {

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

			const_iterator operator+(std::size_t offset) const {
				return const_iterator(*this) += offset;
			}

			NodeRef<Kind,Level> operator*() const {
				return NodeRef<Kind,Level>{cur};
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

		static NodeSet range(NodeRef<Kind,Level> a, NodeRef<Kind,Level> b) {
			return NodeSet(a.id,std::max(a.id,b.id));
		}

	};

	template<typename Kind,unsigned Level>
	using NodeList = utils::range<const NodeRef<Kind,Level>*>;

	template<
		unsigned Levels,
		typename Nodes,
		typename Edges,
		typename Hierarchies
	> struct MeshData;

	namespace detail {

		template<unsigned Level, typename ... NodeTypes>
		struct NodeSet;

		template<unsigned Level, typename First, typename ... Rest>
		struct NodeSet<Level,First,Rest...> {

			std::size_t node_counter;

			NodeSet<Level,Rest...> nested;

			NodeSet() : node_counter(0) {}
			NodeSet(const NodeSet&) = default;
			NodeSet(NodeSet&& other)
				: node_counter(other.node_counter),
				  nested(std::move(other.nested))
			{}

			NodeSet(std::size_t node_counter, NodeSet<Level,Rest...>&& nested)
				: node_counter(node_counter),
				  nested(std::move(nested))
			{}

			NodeSet& operator=(NodeSet&&) =default;

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

			template<typename Body>
			void forAll(const Body& body) const {
				// iterate over nodes of this kind
				for(std::size_t i = 0; i<node_counter; i++) {
					body(NodeRef<First,Level>(i));
				}
				// and on remaining kinds
				nested.forAll(body);
			}

			template<typename Body>
			void forAllTypes(const Body& body) const {
				// call for this type
				body(First(), level<Level>());
				// and the nested types
				nested.forAllTypes(body);
			}

			void store(std::ostream& out) const {
				// store the number of nodes
				write<std::size_t>(out, node_counter);

				// store the nested hierarchy
				nested.store(out);
			}

			static NodeSet load(std::istream& in) {

				// restore the number of nodes
				std::size_t node_counter = read<std::size_t>(in);

				// load nested
				auto nested = NodeSet<Level,Rest...>::load(in);

				// done
				return NodeSet(node_counter,std::move(nested));
			}
		};

		template<unsigned Level>
		struct NodeSet<Level> {

			NodeSet() = default;
			NodeSet(const NodeSet&) = default;
			NodeSet(NodeSet&& other) {}

			NodeSet& operator=(NodeSet&&) =default;

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

			template<typename Body>
			void forAll(const Body&) const {
				// no more nodes
			}

			template<typename Body>
			void forAllTypes(const Body&) const {
				// nothing to do
			}

			void store(std::ostream& out) const {
				// nothing
			}

			static NodeSet load(std::istream& in) {
				return NodeSet();
			}
		};

		template<unsigned Level, typename ... EdgeTypes>
		struct EdgeSet;

		template<unsigned Level, typename First, typename ... Rest>
		struct EdgeSet<Level,First,Rest...> {

			using Src = typename First::src_node_type;
			using Trg = typename First::trg_node_type;

			std::vector<uint32_t> forward_offsets;
			std::vector<NodeRef<Trg,Level>> forward_targets;

			std::vector<uint32_t> backward_offsets;
			std::vector<NodeRef<Src,Level>> backward_targets;

			std::vector<std::pair<NodeRef<Src,Level>,NodeRef<Trg,Level>>> edges;


			EdgeSet<Level,Rest...> nested;

			EdgeSet() = default;
			EdgeSet(const EdgeSet&) = default;
			EdgeSet(EdgeSet&& other)
				: forward_offsets(std::move(other.forward_offsets)),
				  forward_targets(std::move(other.forward_targets)),
				  backward_offsets(std::move(other.backward_offsets)),
				  backward_targets(std::move(other.backward_targets)),
				  edges(std::move(other.edges)),
				  nested(std::move(other.nested))
			{}

			EdgeSet& operator=(EdgeSet&&) =default;

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

		public:

			void addEdge(const NodeRef<Src,Level>& src, const NodeRef<Trg,Level>& trg) {
				edges.push_back({src,trg});
			}

			template<typename MeshData>
			void close(const MeshData& data) {

				// init forward / backward vectors
				forward_offsets.resize(data.template numNodes<Src,Level>() + 1, 0);
				forward_targets.resize(edges.size());

				// count number of sources / sinks
				for(const auto& cur : edges) {
					forward_offsets[cur.first.id]++;
				}

				// compute prefix sums
				sumPrefixes(forward_offsets);

				// fill in targets
				auto forward_pos = forward_offsets;
				for(const auto& cur : edges) {
					forward_targets[forward_pos[cur.first.id]++] = cur.second;
				}
				forward_pos.clear();

				// clear edges
				edges.clear();

				// also fill in backward edges
				restoreBackward(data.template numNodes<Trg,Level>() + 1);

				// close nested edges
				nested.close(data);
			}

			NodeList<Trg,Level> getSinks(const NodeRef<Src,Level>& src) const {
				assert(forward_offsets.size() > 0);
				if (src.id >= forward_offsets.size()-1) return NodeList<Trg,Level>{nullptr,nullptr};
				return NodeList<Trg,Level>{&forward_targets[forward_offsets[src.id]], &forward_targets[forward_offsets[src.id+1]]};
			}

			NodeList<Src,Level> getSources(const NodeRef<Trg,Level>& src) const {
				assert(backward_offsets.size() > 0);
				if (src.id >= backward_offsets.size()-1) return NodeList<Src,Level>{nullptr,nullptr};
				return NodeList<Src,Level>{&backward_targets[backward_offsets[src.id]], &backward_targets[backward_offsets[src.id+1]]};
			}

			bool operator==(const EdgeSet& other) const {
				return forward_offsets == other.forward_offsets &&
						forward_targets == other.forward_targets &&
						backward_offsets == other.backward_offsets &&
						backward_targets == other.backward_targets &&
						edges == other.edges && nested == other.nested;
			}

			template<typename Body>
			void forAllEdges(const Body& body) const {

				// visit all edges
				for(const auto& cur : edges) {
					body(First(),cur.first,cur.second);
				}

				// visit all forward edges
				for(std::size_t i = 0 ; i < forward_offsets.size()-1; ++i) {
					NodeRef<Src,Level> src {i};
					for(const auto& trg : getSinks(src)) {
						body(First(),src,trg);
					}
				}

			}

			template<typename Body>
			void forAll(const Body& body) const {

				// visit all local edges
				forAllEdges(body);

				// visit links of remaining hierarchies
				nested.forAll(body);
			}

			template<typename Body>
			void forAllTypes(const Body& body) const {
				// visit all links for this type
				body(First(), level<Level>());
				// visit links of remaining hierarchies
				nested.forAllTypes(body);
			}

			void store(std::ostream& out) const {
				// store this edge data (forward only)
				write<std::size_t>(out, forward_offsets.size());
				write(out,forward_offsets.begin(), forward_offsets.end());
				write<std::size_t>(out, forward_targets.size());
				write(out,forward_targets.begin(), forward_targets.end());

				// also store sizes of backward edges (simplicity)
				write<std::size_t>(out, backward_offsets.size());

				// store the nested hierarchy
				nested.store(out);
			}

			static EdgeSet load(std::istream& in) {

				// restore this edge set
				EdgeSet res;

				// load forward edges data
				std::size_t offsets = read<std::size_t>(in);
				res.forward_offsets.resize(offsets);
				read(in,res.forward_offsets.begin(), res.forward_offsets.end());

				std::size_t targets = read<std::size_t>(in);
				res.forward_targets.resize(targets);
				read(in,res.forward_targets.begin(), res.forward_targets.end());

				// load backward edges data
				offsets = read<std::size_t>(in);
				res.restoreBackward(offsets);

				// load nested
				res.nested = EdgeSet<Level,Rest...>::load(in);

				// done
				return res;
			}

		private:

			void restoreBackward(std::size_t numTargetNodes) {

				// fix sizes of backward vectors
				backward_offsets.resize(numTargetNodes, 0);
				backward_targets.resize(forward_targets.size());		// the same length as forward

				// count number of sources
				forAllEdges([&](const auto&, const auto& src, const auto& trg){
					++backward_offsets[trg.id];
				});

				// compute prefix sums
				sumPrefixes(backward_offsets);

				// fill in sources
				auto backward_pos = backward_offsets;
				forAllEdges([&](const auto&, const auto& src, const auto& trg){
					backward_targets[backward_pos[trg.id]++] = src;
				});

			}
		};

		template<unsigned Level>
		struct EdgeSet<Level> {

			EdgeSet() = default;
			EdgeSet(const EdgeSet&) = default;
			EdgeSet(EdgeSet&& other) {}

			EdgeSet& operator=(EdgeSet&&) =default;

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
			void forAll(const Body&) const {
				// nothing to do
			}

			template<typename Body>
			void forAllTypes(const Body&) const {
				// nothing to do
			}

			template<typename MeshData>
			void close(const MeshData& data) {
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
		struct HierarchySet;

		template<unsigned Level, typename First, typename ... Rest>
		struct HierarchySet<Level,First,Rest...> {

			using Src = typename First::parent_node_type;
			using Trg = typename First::child_node_type;

			std::vector<std::vector<NodeRef<Trg,Level-1>>> children;
			std::vector<NodeRef<Src,Level>> parents;

			HierarchySet<Level,Rest...> nested;

			HierarchySet() = default;
			HierarchySet(const HierarchySet&) = default;
			HierarchySet(HierarchySet&& other)
				: children(std::move(other.children)),
				  parents(std::move(other.parents)),
				  nested(std::move(other.nested))
			{}

			HierarchySet& operator=(HierarchySet&&) =default;

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

			void addChild(const NodeRef<Src,Level>& parent, const NodeRef<Trg,Level-1>& child) {
				// register child as a child of parent
				while(parent.id >= children.size()) {
					children.resize(std::max<std::size_t>(10, children.size() * 2));
				}
				auto& list = children[parent.id];
				for(auto& cur : list) if (cur == child) return;
				list.push_back(child);


				// register parent of child
				while(child.id >= parents.size()) {
					parents.resize(std::max<std::size_t>(10, parents.size() * 2));
				}
				auto& trg = parents[child.id];
				if (trg.id != 0 && trg.id != parent.id) {
					std::cerr << "Double-assignment of parent for " << child << "\n";
					exit(1);
				}
				trg = parent;
			}

			const std::vector<NodeRef<Trg,Level-1>>& getChildren(const NodeRef<Src,Level>& parent) const {
				static const std::vector<NodeRef<Trg,Level-1>> empty;
				if (parent.id >= children.size()) return empty;
				return children[parent.id];
			}

			const NodeRef<Src,Level>& getParent(const NodeRef<Trg,Level-1>& child) const {
				assert(parents.size() >= child.id);
				return parents[child.id];
			}

			bool operator==(const HierarchySet& other) const {
				return children == other.children && nested == other.nested;
			}

			template<typename Body>
			void forAll(const Body& body) const {
				// visit all links for this type
				std::size_t counter = 0;
				for(const auto& cur : children) {
					NodeRef<Src,Level> src {counter++};
					for(const auto& trg : cur) {
						body(First(),src,trg);
					}
				}
				// visit links of remaining hierarchies
				nested.forAll(body);
			}

			template<typename Body>
			void forAllTypes(const Body& body) const {
				// visit all links for this type
				body(First(), level<Level>());
				// visit links of remaining hierarchies
				nested.forAllTypes(body);
			}

			void store(std::ostream& out) const {
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
			}

			static HierarchySet load(std::istream& in) {
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
			}
		};

		template<unsigned Level>
		struct HierarchySet<Level> {

			HierarchySet() = default;
			HierarchySet(const HierarchySet&) = default;
			HierarchySet(HierarchySet&& other) {}

			HierarchySet& operator=(HierarchySet&&) =default;

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
			void forAll(const Body&) const {
				// nothing to do
			}

			template<typename Body>
			void forAllTypes(const Body&) const {
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

			// the data of the lower levels
			LevelData<Level-1> nested;

			// the set of nodes on this level
			NodeSet<Level,Nodes...> nodes;

			// the set of edges on this level
			EdgeSet<Level,Edges...> edges;

			// the set of hierarchies connecting this level to the sub-level
			HierarchySet<Level,Hierarchies...> hierarchies;

			Levels() = default;
			Levels(const Levels&) = default;
			Levels(Levels&& other)
				: nested(std::move(other.nested)),
				  nodes(std::move(other.nodes)),
				  edges(std::move(other.edges)),
				  hierarchies(std::move(other.hierarchies))
			{}

			Levels(LevelData<Level-1>&& nested, NodeSet<Level,Nodes...>&& nodes, EdgeSet<Level,Edges...>&& edges, HierarchySet<Level,Hierarchies...>&& hierarchies)
				: nested(std::move(nested)),
				  nodes(std::move(nodes)),
				  edges(std::move(edges)),
				  hierarchies(std::move(hierarchies))
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
				return nodes == other.nodes && edges == other.edges && nested == other.nested && hierarchies == other.hierarchies;
			}

			template<typename Body>
			void forAllNodes(const Body& body) const {
				nodes.forAll(body);
				nested.forAllNodes(body);
			}

			template<typename Body>
			void forAllNodeTypes(const Body& body) const {
				nodes.forAllTypes(body);
				nested.forAllNodeTypes(body);
			}

			template<typename Body>
			void forAllEdges(const Body& body) const {
				edges.forAll(body);
				nested.forAllEdges(body);
			}

			template<typename Body>
			void forAllEdgeTypes(const Body& body) const {
				edges.forAllTypes(body);
				nested.forAllEdgeTypes(body);
			}

			template<typename Body>
			void forAllHierarchies(const Body& body) const {
				hierarchies.forAll(body);
				nested.forAllHierarchies(body);
			}

			template<typename Body>
			void forAllHierarchyTypes(const Body& body) const {
				hierarchies.forAllTypes(body);
				nested.forAllHierarchyTypes(body);
			}

			template<typename MeshData>
			void close(const MeshData& data) {
				nested.close(data);
				edges.close(data);
//				hierarchies.close(data);
			}

			void store(std::ostream& out) const {
				nested.store(out);
				nodes.store(out);
				edges.store(out);
				hierarchies.store(out);
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

			// the set of nodes on this level
			NodeSet<0,Nodes...> nodes;

			// the set of edges on this level
			EdgeSet<0,Edges...> edges;

			Levels() = default;
			Levels(const Levels&) = default;
			Levels(Levels&& other)
				: nodes(std::move(other.nodes)),
				  edges(std::move(other.edges))
			{}

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
				return nodes == other.nodes && edges == other.edges;
			}

			template<typename Body>
			void forAllNodes(const Body& body) const {
				nodes.forAll(body);
			}

			template<typename Body>
			void forAllNodeTypes(const Body& body) const {
				nodes.forAllTypes(body);
			}

			template<typename Body>
			void forAllEdges(const Body& body) const {
				edges.forAll(body);
			}

			template<typename Body>
			void forAllEdgeTypes(const Body& body) const {
				edges.forAllTypes(body);
			}

			template<typename Body>
			void forAllHierarchies(const Body&) const {
				// nothing to do here
			}

			template<typename Body>
			void forAllHierarchyTypes(const Body&) const {
				// nothing to do here
			}

			template<typename MeshData>
			void close(const MeshData& data) {
				edges.close(data);
			}

			void store(std::ostream& out) const {
				nodes.store(out);
				edges.store(out);
			}

			static Levels load(std::istream& in) {
				Levels res;
				res.nodes = NodeSet<0,Nodes...>::load(in);
				res.edges = EdgeSet<0,Edges...>::load(in);
				return res;
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

		using data_store = detail::Levels<Levels,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

		// all the topological data of all the nodes, edges and hierarchy relations on all levels
		data_store data;

		MeshData() = default;
		MeshData(const MeshData&) = default;
		MeshData(MeshData&& other) : data(std::move(other.data)) {}
		MeshData(data_store&& data) : data(std::move(data)) {}

		template<unsigned Level>
		detail::NodeSet<Level,Nodes...>& getNodes() {
			return data.template getLevel<Level>().nodes;
		}

		template<unsigned Level>
		const detail::NodeSet<Level,Nodes...>& getNodes() const {
			return data.template getLevel<Level>().nodes;
		}

		template<unsigned Level>
		detail::EdgeSet<Level,Edges...>& getEdges() {
			return data.template getLevel<Level>().edges;
		}

		template<unsigned Level>
		const detail::EdgeSet<Level,Edges...>& getEdges() const {
			return data.template getLevel<Level>().edges;
		}

		template<unsigned Level>
		detail::HierarchySet<Level,Hierarchies...>& getHierarchies() {
			return data.template getLevel<Level>().hierarchies;
		}

		template<unsigned Level>
		const detail::HierarchySet<Level,Hierarchies...>& getHierarchies() const {
			return data.template getLevel<Level>().hierarchies;
		}

		template<typename Body>
		void forAllNodes(const Body& body) const {
			data.forAllNodes(body);
		}

		template<typename Body>
		void forAllNodeTypes(const Body& body) const {
			data.forAllNodeTypes(body);
		}

		template<typename Body>
		void forAllEdges(const Body& body) const {
			data.forAllEdges(body);
		}

		template<typename Body>
		void forAllEdgeTypes(const Body& body) const {
			data.forAllEdgeTypes(body);
		}

		template<typename Body>
		void forAllHierarchies(const Body& body) const {
			data.forAllHierarchies(body);
		}

		template<typename Body>
		void forAllHierarchyTypes(const Body& body) const {
			data.forAllHierarchyTypes(body);
		}

		template<typename Kind,unsigned Level = 0>
		std::size_t numNodes() const {
			return getNodes<Level>().template get<Kind>().numNodes();
		}

		bool operator==(const MeshData& other) const {
			return data == other.data;
		}

		void close() {
			data.close(*this);
		}

		void store(std::ostream& out) const {
			// store nested data
			data.store(out);
		}

		static MeshData load(std::istream& in) {
			return MeshData { data_store::load(in) };
		}

	};

	template<typename Kind, unsigned Level, typename Data>
	struct DataContainer {

		enum State {
			Uninitialized,
			Initializing,
			Initialized
		};

		std::size_t capacity;
		mutable std::vector<Data> data;
		mutable std::atomic<State> state;

	public:

		using node_type = Kind;
		enum { level = Level };

		DataContainer(std::size_t size) : capacity(size), data(), state(Uninitialized) {}

		DataContainer(const DataContainer& other)
			: capacity(other.capacity), data(other.data), state(other.state.load())
		{}

		DataContainer(DataContainer&& other)
			: capacity(std::move(other.capacity)), data(std::move(other.data)), state(other.state.load())
		{}

		DataContainer& operator=(const DataContainer& other) {
			capacity = other.capacity;
			data = other.data;
			state = other.state.load();
			return *this;
		}

		DataContainer& operator=(DataContainer&& other) {
			capacity = other.capacity;
			data = std::move(other.data);
			state = other.state.load();
			return *this;
		}

		std::size_t size() const {
			return capacity;
		}

		Data& operator[](const NodeRef<Kind,Level>& index) {
			init();
			return data[index.id];
		}

		const Data& operator[](const NodeRef<Kind,Level>& index) const {
			init();
			return data[index.id];
		}

		bool operator==(const DataContainer& other) const {
			if (size() != other.size()) return false;
			return data == other.data;
		}

		auto begin() { init(); return data.begin(); }
		auto end() { init(); return data.end(); }

		void store(std::ostream& out) const {
			// store nested data
			write<std::size_t>(out, capacity);
			write<std::size_t>(out, data.size());
			for(const auto& cur : data) {
				write<Data>(out, cur);
			}
		}

		static DataContainer load(std::istream& in) {
			std::size_t capacity = read<std::size_t>(in);
			std::size_t size = read<std::size_t>(in);
			DataContainer res(capacity);
			res.data.resize(size);
			for(std::size_t i =0; i<size; i++) {
				res.data[i] = read<Data>(in);
			}
			return res;
		}

	private:

		void init() const {
			State is = Uninitialized;
			if (state.compare_exchange_strong(is,Initializing)) {
				data.resize(capacity);
				state.store(Initialized);
			}
			while(state.load() != Initialized);
		}
	};


	template<
		unsigned Layers,
		typename NodeTypes,
		typename EdgeTypes,
		typename Hierarchies
	>
	class Mesh {

		using data_type = MeshData<Layers-1,NodeTypes,EdgeTypes,Hierarchies>;

		data_type data;

	public:

		using builder_type = MeshBuilder<NodeTypes,EdgeTypes,Hierarchies,Layers,Builder>;

		enum { layers = Layers };

		template<typename Kind,unsigned Level = 0>
		using node_ref_type = NodeRef<Kind,Level>;

		template<typename Kind,unsigned Level = 0>
		using node_set_type = NodeSet<Kind,Level>;

		template<typename Kind, unsigned Level, typename Data>
		using data_container = DataContainer<Kind,Level,Data>;

		Mesh() = default;
		Mesh(const Mesh&) = default;
		Mesh(const data_type& data) : data(data) {};
		Mesh(data_type&& data) : data(std::move(data)) {}

		const data_type& getData() const & {
			return data;
		}

		data_type&& getData() && {
			return data;
		}

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
			return data.template numNodes<Kind,Level>();
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
			typename EdgeKind,
			typename B,
			unsigned Level,
			typename A = typename EdgeKind::src_node_type
		>
		NodeList<A,Level> getSources(const node_ref_type<B,Level>& b) const {
			return data.template getEdges<Level>().template get<EdgeKind>().getSources(b);
		}

		template<
			typename Hierarchy,
			typename A, unsigned Level,
			typename B = typename Hierarchy::child_node_type
		>
		const std::vector<node_ref_type<B,Level-1>>& getChildren(const node_ref_type<A,Level>& a) const {
			return data.template getHierarchies<Level>().template get<Hierarchy>().getChildren(a);
		}

		template<
			typename Hierarchy,
			typename A, unsigned Level,
			typename B = typename Hierarchy::parent_node_type
		>
		const node_ref_type<B,Level+1>& getParent(const node_ref_type<A,Level>& a) const {
			return data.template getHierarchies<Level+1>().template get<Hierarchy>().getParent(a);
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
		void forAllNodes(const Body& body) const {
			data.forAllNodes(body);
		}

		template<typename Body>
		void forAllNodeTypes(const Body& body) const {
			data.forAllNodeTypes(body);
		}

		template<typename Body>
		void forAllEdges(const Body& body) const {
			data.forAllEdges(body);
		}

		template<typename Body>
		void forAllEdgeTypes(const Body& body) const {
			data.forAllEdgeTypes(body);
		}

		template<typename Body>
		void forAllHierarchies(const Body& body) const {
			data.forAllHierarchies(body);
		}

		template<typename Body>
		void forAllHierarchyTypes(const Body& body) const {
			data.forAllHierarchyTypes(body);
		}

		template<typename Body>
		void forAllLinks(const Body& body) const {
			forAllEdges(body);
			forAllHierarchies(body);
		}

		template<typename Body>
		void forAllLinkedTypes(const Body& body) const {
			forAllEdgeTypes(body);
			forAllHierarchyTypes(body);
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
			return Mesh(data_type::load(in));
		}

	private:

		const static int MAGIC_NUMBER = 0x4875368;		// some random number

	};


	template<typename Kind, unsigned Level>
	class ContinousNodeList {

		template<
			unsigned Layers,
			typename NodeTypes,
			typename EdgeTypes,
			typename Hierarchies
		>
		friend class Builder;

		std::size_t start;
		std::size_t length;

	public:

		ContinousNodeList()
			: start(0), length(0) {}

	private:

		ContinousNodeList(NodeRef<Kind,Level>& start, NodeRef<Kind,Level>& end)
			: start(start.id), length(end.id - start.id + 1) {}

	public:

		NodeRef<Kind,Level> operator[](std::size_t index) const {
			return NodeRef<Kind,Level> { start + index };
		}

		std::size_t size() const {
			return length;
		}


		class const_iterator : public std::iterator<std::random_access_iterator_tag, NodeRef<Kind,Level>> {

			std::size_t cur;

		public:

			const_iterator(std::size_t pos) : cur(pos) {};

			bool operator==(const const_iterator& other) const {
				return cur == other.cur;
			}

			bool operator!=(const const_iterator& other) const {
				return !(*this == other);
			}

			bool operator<(const const_iterator& other) const {
				return cur < other.cur;
			}

			bool operator<=(const const_iterator& other) const {
				return cur <= other.cur;
			}

			bool operator>=(const const_iterator& other) const {
				return cur >= other.cur;
			}

			bool operator>(const const_iterator& other) const {
				return cur > other.cur;
			}

			NodeRef<Kind,Level> operator*() const {
				return NodeRef<Kind,Level>{cur};
			}

			const_iterator& operator++() {
				++cur;
				return *this;
			}

			const_iterator operator++(int) {
				const_iterator res = *this;
				++cur;
				return res;
			}

			const_iterator& operator--() {
				--cur;
				return *this;
			}

			const_iterator operator--(int) {
				const_iterator res = *this;
				--cur;
				return res;
			}

			const_iterator& operator+=(std::ptrdiff_t n) {
				cur += n;
				return *this;
			}

			const_iterator& operator-=(std::ptrdiff_t n) {
				cur -= n;
				return *this;
			}

			friend const_iterator operator+(const_iterator& iter, std::ptrdiff_t n) {
				const_iterator res = iter;
				res.cur += n;
				return res;

			}

			friend const_iterator& operator+(std::ptrdiff_t n, const_iterator& iter) {
				const_iterator res = iter;
				res.cur += n;
				return res;
			}

			const_iterator operator-(std::ptrdiff_t n) {
				const_iterator res = *this;
				res.cur -= n;
				return res;
			}

			std::ptrdiff_t operator-(const_iterator& other) const {
				return std::ptrdiff_t(cur - other.cur);
			}

			NodeRef<Kind,Level> operator[](std::ptrdiff_t n) const {
				return *(*this + n);
			}

		};

		const_iterator begin() const {
			return const_iterator(start);
		}

		const_iterator end() const {
			return const_iterator(start + length);
		}

	};

	template<
		unsigned Layers,
		typename NodeTypes,
		typename EdgeTypes,
		typename Hierarchies
	>
	class Builder {

		MeshData<Layers-1,NodeTypes,EdgeTypes,Hierarchies> data;

	public:

		// -- type definitions --

		template<typename Kind,unsigned Level>
		using node_ref_type = NodeRef<Kind,Level>;

		template<typename Kind,unsigned Level>
		using node_list_type = ContinousNodeList<Kind,Level>;

		using mesh_type = Mesh<Layers,NodeTypes,EdgeTypes,Hierarchies>;

		// -- mesh modeling --

		template<typename Kind,unsigned Level>
		node_ref_type<Kind,Level> create() {
			return data.template getNodes<Level>().template get<Kind>().create();
		}

		template<typename Kind, unsigned Level>
		ContinousNodeList<Kind,Level> create(unsigned number) {
			if (number == 0) {
				return ContinousNodeList<Kind,Level>();
			}

			// create the first node
			NodeRef<Kind,Level> first = create<Kind,Level>();

			// create all others, remember the last
			NodeRef<Kind,Level> last = first;
			for(unsigned i=1; i<number; i++) {
				auto next = create<Kind,Level>();
				assert(last.id+1 == next.id);
				last = next;
			}

			// return the node list
			return ContinousNodeList<Kind,Level>(first,last);
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
			return mesh_type(std::move(data));
		}

	};


} // end namespace reference
} // end namespace mesh
} // end namespace utils
} // end namespace allscale
