#pragma once

#include "allscale/utils/mesh.h"

#include <type_traits>

namespace allscale {
namespace utils {


	template<typename Mesh>
	class FlattendMesh;


	namespace detail {

		template<unsigned Level, typename Node>
		struct count_node_kind {
			template<typename Mesh>
			std::size_t operator()(const Mesh& m) const {
				return m.template numNodes<Node,Level>() + count_node_kind<Level-1,Node>()(m);
			}
		};

		template<typename Node>
		struct count_node_kind<0,Node> {
			template<typename Mesh>
			std::size_t operator()(const Mesh& m) const {
				return m.template numNodes<Node,0>();
			}
		};

		template<unsigned Layers, typename ... Nodes>
		struct count_num_nodes;

		template<unsigned Layers, typename First, typename ... Rest>
		struct count_num_nodes<Layers,First,Rest...> {
			template<typename Mesh>
			std::size_t operator()(const Mesh& m) const {
				return count_node_kind<Layers-1,First>()(m) + count_num_nodes<Layers,Rest...>()(m);
			}
		};

		template<unsigned Layers>
		struct count_num_nodes<Layers> {
			template<typename Mesh>
			std::size_t operator()(const Mesh&) const { return 0; }
		};


		// -- get the union over all node-references --

		template<typename Mesh, unsigned Level, typename Node>
		struct node_reference_for_kind {
			union type {
				typename node_reference_for_kind<Mesh,Level-1,Node>::type nested;
				typename Mesh::template node_ref_type<Node,Level> ref;
			};
		};

		template<typename Mesh, typename Node>
		struct node_reference_for_kind<Mesh,0,Node> {
			union type {
				typename Mesh::template node_ref_type<Node,0> ref;
			};
		};

		template<typename Mesh, unsigned Layers, typename ... Nodes>
		struct node_reference;


		template<typename Mesh, unsigned Layers, typename First, typename ... Rest>
		struct node_reference<Mesh,Layers,First,Rest...> {
			union type {
				typename node_reference<Mesh,Layers,Rest...>::type nested;
				typename node_reference_for_kind<Mesh,Layers-1,First>::type ref;
			};
		};

		template<typename Mesh, unsigned Layers>
		struct node_reference<Mesh,Layers> {
			union type {};
		};


		// -- a meta-operator to obtain the index of an element in a list --

		template<typename Element, typename ... List>
		struct index_of;

		template<typename Element, typename First, typename ... Rest>
		struct index_of<Element,First,Rest...> {
			enum { value = index_of<Element,Rest...>::value + 1 };
			static_assert(value < sizeof...(Rest)+1, "Element not in list!");
		};

		template<typename First, typename ... Rest>
		struct index_of<First,First,Rest...> {
			enum { value = 0 };
		};

		template<typename Element>
		struct index_of<Element> {
			enum { value = 0 };
		};


		template<typename Mesh, typename Node>
		struct node_kind_index;

		template<
			template<
				unsigned Layers,
				typename NodeTypes,
				typename EdgeTypes,
				typename Hierarchies
			>
			class Mesh,
			unsigned Layers,
			typename ... Nodes,
			typename ... Edges,
			typename ... Hierarchies,
			typename Node
		>
		struct node_kind_index<Mesh<Layers,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>,Node> {
			enum { value = index_of<Node,Nodes...>::value };
		};



		template<int Level,typename ... Nodes>
		struct offset_initializer_helper;

		template<int Level,typename First, typename ... Rest>
		struct offset_initializer_helper<Level,First,Rest...> {
			template<typename Mesh, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, std::array<std::array<std::size_t,layers>,nodes>& offset, std::size_t& counter) const {
				// init current field
				offset[node_kind_index<Mesh,First>::value][Level] = counter;

				// count elements
				counter += mesh.template numNodes<First,Level>();

				// index lower levels
				offset_initializer_helper<Level-1,First,Rest...>()(mesh,offset,counter);

			}
		};

		template<typename First, typename ... Rest>
		struct offset_initializer_helper<-1,First,Rest...> {
			template<typename Mesh, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, std::array<std::array<std::size_t,layers>,nodes>& offset, std::size_t& counter) const {
				// continue with next node type
				offset_initializer_helper<layers-1,Rest...>()(mesh,offset,counter);
			}
		};

		template<int Level>
		struct offset_initializer_helper<Level> {
			template<typename Mesh, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, std::array<std::array<std::size_t,layers>,nodes>& offset, std::size_t& counter) const {
				// done
			}
		};

		template<int Layers, typename ... Nodes>
		struct offset_initializer {
			template<typename Mesh>
			void operator()(const Mesh& mesh, std::array<std::array<std::size_t,Layers>,(unsigned)(sizeof...(Nodes))>& offset) {
				std::size_t counter = 0;
				offset_initializer_helper<Layers-1,Nodes...>()(mesh,offset,counter);
			}
		};


		template<typename Mesh, unsigned Layers, typename ... Nodes>
		struct FlattendNodeRef {
			short kind;
			short level;
			typename detail::node_reference<Mesh,Layers,Nodes...>::type ref;
			std::size_t pos;

			std::size_t getOrdinal() const {
				return pos;
			}
		};


		template<int Level, short Kind, typename Node, typename FlattendNode>
		struct mesh_node_kind_scanner {
			template<typename Mesh, typename Body>
			void operator()(const Mesh& m, const Body& body, std::size_t& counter) {

				using node_ref_type = typename Mesh::template node_ref_type<Node,Level>;

				// visit all nodes of the given Node type on this level
				FlattendNode node;
				node.kind = Kind;
				node.level = Level;
				m.template forAll<Node,Level>([&](const node_ref_type& cur) {
					reinterpret_cast<node_ref_type&>(node.ref) = cur;
					node.pos = counter++;
					body(cur,node);
				});

				// visit other levels
				mesh_node_kind_scanner<Level-1,Kind,Node,FlattendNode>()(m,body,counter);
			}
		};

		template<short Kind, typename Node, typename FlattendNode>
		struct mesh_node_kind_scanner<-1,Kind,Node,FlattendNode> {
			template<typename Mesh, typename Body>
			void operator()(const Mesh& m, const Body& body, std::size_t& counter) {
				// nothing
			}
		};

		template<unsigned Layers, short Kind, typename FlattendNode, typename ... Nodes>
		struct mesh_node_scanner;

		template<unsigned Layers, short Kind, typename FlattendNode, typename First, typename ... Rest>
		struct mesh_node_scanner<Layers,Kind,FlattendNode,First,Rest...> {
			template<typename Mesh, typename Body>
			void operator()(const Mesh& m, const Body& body, std::size_t& counter) {
				// visit this kind
				mesh_node_kind_scanner<Layers-1,Kind,First,FlattendNode>()(m,body,counter);
				// visit remaining kinds
				mesh_node_scanner<Layers,Kind+1,FlattendNode,Rest...>()(m,body,counter);
			}
		};

		template<unsigned Layers, short Kind, typename FlattendNode>
		struct mesh_node_scanner<Layers,Kind,FlattendNode> {
			template<typename Mesh, typename Body>
			void operator()(const Mesh& m, const Body& body, std::size_t& counter) {
				// done
			}
		};


		// ---- get neighbor ----

		template<int Layers, int Level, typename ... Edges>
		struct neighbor_collector;

		template<int Layers, int Level, typename First, typename ... Rest>
		struct neighbor_collector<Layers,Level,First,Rest...> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				auto src_kind = node_kind_index<Mesh,typename First::src_node_type>::value;
				auto trg_kind = node_kind_index<Mesh,typename First::trg_node_type>::value;

				using src_node_ref_type = typename Mesh::template node_ref_type<typename First::src_node_type,Level>;
				using trg_node_ref_type = typename Mesh::template node_ref_type<typename First::trg_node_type,Level>;

				if (ref.level == Level) {

					// process edge First on level Level
					if (src_kind == ref.kind) {

						auto node_ref = reinterpret_cast<const src_node_ref_type&>(ref.ref);

						for(const auto& cur : mesh.template getSinks<First>(node_ref)) {
							NodeRef trg;
							trg.kind = trg_kind;
							trg.level = Level;
							reinterpret_cast<trg_node_ref_type&>(trg.ref) = cur;
							trg.pos = offsets[trg.kind][trg.level] + cur.getOrdinal();
							res.push_back(trg);
						}

					}

					if (trg_kind == ref.kind) {

						auto node_ref = reinterpret_cast<const trg_node_ref_type&>(ref.ref);

						for(const auto& cur : mesh.template getSources<First>(node_ref)) {
							NodeRef trg;
							trg.kind = src_kind;
							trg.level = Level;
							reinterpret_cast<src_node_ref_type&>(trg.ref) = cur;
							trg.pos = offsets[trg.kind][trg.level] + cur.getOrdinal();
							res.push_back(trg);
						}

					}

				}

				// continue
				neighbor_collector<Layers,Level-1,First,Rest...>()(mesh,offsets,ref,res);
			}
		};

		template<int Layers, typename First, typename ... Rest>
		struct neighbor_collector<Layers,-1,First,Rest...> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				// continue
				neighbor_collector<Layers,Layers-1,Rest...>()(mesh,offsets,ref,res);
			}
		};

		template<int Layers, int Level>
		struct neighbor_collector<Layers,Level> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				// done
			}
		};


		// ---- get parents and children ----

		template<int Layers, int Level, unsigned Depth, typename ... Hierarchies>
		struct family_collector;

		template<int Layers, int Level, unsigned Depth, typename First, typename ... Rest>
		struct family_collector<Layers,Level,Depth,First,Rest...> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				auto parent_kind = node_kind_index<Mesh,typename First::parent_node_type>::value;
				auto child_kind = node_kind_index<Mesh,typename First::child_node_type>::value;

				if (ref.level == Level) {

					// process edge First on level Level
					if (parent_kind == ref.kind) {

						using parent_node_ref_type = typename Mesh::template node_ref_type<typename First::parent_node_type,Level>;
						using child_node_ref_type = typename Mesh::template node_ref_type<typename First::child_node_type,Level-1>;

						auto node_ref = reinterpret_cast<const parent_node_ref_type&>(ref.ref);

						for(const auto& cur : mesh.template getChildren<First>(node_ref)) {
							NodeRef trg;
							trg.kind = child_kind;
							trg.level = Level-1;
							reinterpret_cast<child_node_ref_type&>(trg.ref) = cur;
							trg.pos = offsets[trg.kind][trg.level] + cur.getOrdinal();
							res.push_back(trg);
						}

					}

					if (child_kind == ref.kind) {

						using parent_node_ref_type = typename Mesh::template node_ref_type<typename First::parent_node_type,Level+1>;
						using child_node_ref_type = typename Mesh::template node_ref_type<typename First::child_node_type,Level>;

						auto node_ref = reinterpret_cast<const child_node_ref_type&>(ref.ref);

						auto parent = mesh.template getParent<First>(node_ref);

						NodeRef trg;
						trg.kind = parent_kind;
						trg.level = Level+1;
						reinterpret_cast<parent_node_ref_type&>(trg.ref) = parent;
						trg.pos = offsets[trg.kind][trg.level] + parent.getOrdinal();
						res.push_back(trg);

					}

				}

				// continue
				family_collector<Layers,Level-1,Depth+1,First,Rest...>()(mesh,offsets,ref,res);
			}
		};

		template<int Layers, int Level, typename First, typename ... Rest>
		struct family_collector<Layers,Level,0,First,Rest...> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				auto parent_kind = node_kind_index<Mesh,typename First::parent_node_type>::value;
				auto child_kind = node_kind_index<Mesh,typename First::child_node_type>::value;


				if (ref.level == Level) {

					// process edge First on level Level
					if (parent_kind == ref.kind) {

						using parent_node_ref_type = typename Mesh::template node_ref_type<typename First::parent_node_type,Level>;
						using child_node_ref_type = typename Mesh::template node_ref_type<typename First::child_node_type,Level-1>;

						auto node_ref = reinterpret_cast<const parent_node_ref_type&>(ref.ref);

						for(const auto& cur : mesh.template getChildren<First>(node_ref)) {
							NodeRef trg;
							trg.kind = child_kind;
							trg.level = Level-1;
							reinterpret_cast<child_node_ref_type&>(trg.ref) = cur;
							trg.pos = offsets[trg.kind][trg.level] + cur.getOrdinal();
							res.push_back(trg);
						}

					}

				}

				// continue
				family_collector<Level+1,Level-1,1,First,Rest...>()(mesh,offsets,ref,res);
			}
		};

		template<int Layers, unsigned Depth, typename First, typename ... Rest>
		struct family_collector<Layers,0,Depth,First,Rest...> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				auto parent_kind = node_kind_index<Mesh,typename First::parent_node_type>::value;
				auto child_kind = node_kind_index<Mesh,typename First::child_node_type>::value;

				if (ref.level == 0) {

					if (child_kind == ref.kind) {

						using parent_node_ref_type = typename Mesh::template node_ref_type<typename First::parent_node_type,1>;
						using child_node_ref_type = typename Mesh::template node_ref_type<typename First::child_node_type,0>;

						auto node_ref = reinterpret_cast<const child_node_ref_type&>(ref.ref);

						auto parent = mesh.template getParent<First>(node_ref);

						NodeRef trg;
						trg.kind = parent_kind;
						trg.level = 1;
						reinterpret_cast<parent_node_ref_type&>(trg.ref) = parent;
						// TODO: add global counter value
						trg.pos = offsets[trg.kind][trg.level] + parent.getOrdinal();
						res.push_back(trg);

					}

				}

				// continue
				family_collector<Layers,Layers-1,0,Rest...>()(mesh,offsets,ref,res);
			}
		};

		template<int Layers, int Level, unsigned Depth>
		struct family_collector<Layers,Level,Depth> {
			template<typename Mesh, typename NodeRef, std::size_t layers, std::size_t nodes>
			void operator()(const Mesh& mesh, const std::array<std::array<std::size_t,layers>,nodes>& offsets, const NodeRef& ref, std::vector<NodeRef>& res) {
				// done
			}
		};
	}


	template<
		template<
			unsigned Layers,
			typename NodeTypes,
			typename EdgeTypes,
			typename Hierarchies
		>
		class Mesh,
		unsigned Layers,
		typename ... Nodes,
		typename ... Edges,
		typename ... Hierarchies
	>
	class FlattendMesh<Mesh<Layers,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>> {

		using mesh_type = Mesh<Layers,nodes<Nodes...>,edges<Edges...>,hierarchies<Hierarchies...>>;

		const mesh_type& mesh;

		// the offset of nodes in the flattened list
		std::array<std::array<std::size_t,Layers>,sizeof...(Nodes)> offsets;

	public:

		template<typename Node, unsigned Level>
		using MeshNodeRef = typename mesh_type::template node_ref_type<Node,Level>;

		using NodeRef = typename detail::FlattendNodeRef<mesh_type,Layers,Nodes...>;

		FlattendMesh(const mesh_type& mesh) : mesh(mesh) {
			detail::offset_initializer<Layers,Nodes...>()(mesh,offsets);
		}

		std::size_t numNodes() const {
			return detail::count_num_nodes<Layers,Nodes...>()(mesh);
		}

		template<typename Lambda>
		void forAll(const Lambda& body) const {
			std::size_t counter = 0;
			detail::mesh_node_scanner<Layers,0,NodeRef,Nodes...>()(mesh,body,counter);
		}

		template<typename Node, unsigned Level>
		NodeRef toFlatRef(const MeshNodeRef<Node,Level>& ref) const {
			NodeRef res;
			res.kind = detail::index_of<Node,Nodes...>::value;
			res.level = Level;
			reinterpret_cast<MeshNodeRef<Node,Level>&>(res.ref) = ref;
			res.pos = offsets[res.kind][res.level] + ref.getOrdinal();
			return res;
		}

		template<typename Node, unsigned Level>
		std::size_t getOrdinal(const MeshNodeRef<Node,Level>& ref) const {
			return offsets[detail::index_of<Node,Nodes...>::value][Level] + ref.getOrdinal();
		}

		std::vector<NodeRef> getNeighbors(const NodeRef& ref) const {
			std::vector<NodeRef> res;
			detail::neighbor_collector<Layers,Layers-1,Edges...>()(mesh,offsets,ref,res);
			detail::family_collector<Layers,Layers-1,0,Hierarchies...>()(mesh,offsets,ref,res);
			return res;
		}

	};

	template<typename Mesh>
	FlattendMesh<Mesh> flatten(const Mesh& mesh) {
		return FlattendMesh<Mesh>(mesh);
	}


} // end namespace utils
} // end namespace allscale
