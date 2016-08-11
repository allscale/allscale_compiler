#pragma once

#include "allscale/utils/mesh.h"

namespace allscale {
namespace fine_open {


	// ---------------------------------------------------------------------------------------------------
	// 									Type-Dependent Node Data Store
	// ---------------------------------------------------------------------------------------------------

	namespace detail {

		template<
			template<typename,typename,unsigned> class Config,
			typename Mesh, typename Node, unsigned Level
		>
		struct NodeData {

			using config = Config<Mesh,Node,Level>;
			using data_type = typename config::element_type;
			using container_type = typename Mesh::template data_container<Node,Level,data_type>;
			using node_ref_type = typename Mesh::template node_ref_type<Node,Level>;


			container_type data;

			NodeData(const Mesh& m) : data(m.template numNodes<Node,Level>()) { }

			template<typename Body>
			void forAll(const Body& body) { body(data); }

			const data_type& operator[](const node_ref_type& ref) const {
				return data[ref];
			}

			data_type& operator[](const node_ref_type& ref) {
				return data[ref];
			}
		};


		template<
			template<typename,typename,unsigned> class Config,
			typename Mesh, unsigned Layers, int Level, typename ... Nodes
		>
		struct DataStore;

		template<
			template<typename,typename,unsigned> class Config,
			typename Mesh, unsigned Layers, int Level, typename First, typename ... Rest
		>
		struct DataStore<Config,Mesh,Layers,Level,First,Rest...> {

			template<typename Node, unsigned Lvl>
			using node_ref = typename Mesh::template node_ref_type<Node,Lvl>;

			// permutation of this level and first node type
			NodeData<Config,Mesh,First,Level> data;

			// store for all other levels and nodes
			DataStore<Config,Mesh,Layers,Level-1,First,Rest...> nested;

			DataStore(const Mesh& m) : data(m), nested(m) {}

			template<typename Body>
			void forAll(const Body& body) {
				data.forAll(body);
				nested.forAll(body);
			}

			const typename Config<Mesh,First,Level>::element_type&
			operator[](const node_ref<First,Level>& ref) const {
				return data[ref];
			}

			template<typename Node, unsigned Lvl>
			const typename Config<Mesh,Node,Lvl>::element_type&
			operator[](const node_ref<Node,Lvl>& ref) const {
				return nested[ref];
			}

			typename Config<Mesh,First,Level>::element_type&
			operator[](const node_ref<First,Level>& ref) {
				return data[ref];
			}

			template<typename Node, unsigned Lvl>
			typename Config<Mesh,Node,Lvl>::element_type&
			operator[](const node_ref<Node,Lvl>& ref) {
				return nested[ref];
			}
		};

		template<
			template<typename,typename,unsigned> class Config,
			typename Mesh, unsigned Layers, typename First, typename ... Rest
		>
		struct DataStore<Config,Mesh,Layers,-1,First,Rest...> {

			template<typename Node, unsigned Level>
			using node_ref = typename Mesh::template node_ref_type<Node,Level>;

			DataStore<Config,Mesh,Layers,Layers-1,Rest...> nested;

			DataStore(const Mesh& m) : nested(m) {}

			template<typename Body>
			void forAll(const Body& body) {
				nested.forAll(body);
			}

			template<typename Node, unsigned Level>
			typename Config<Mesh,Node,Level>::element_type&
			operator[](const node_ref<Node,Level>& ref) {
				return nested[ref];
			}

			template<typename Node, unsigned Level>
			const typename Config<Mesh,Node,Level>::element_type&
			operator[](const node_ref<Node,Level>& ref) const {
				return nested[ref];
			}
		};

		template<
			template<typename,typename,unsigned> class Config,
			typename Mesh, unsigned Layers, int Level
		>
		struct DataStore<Config,Mesh,Layers,Level> {

			DataStore(const Mesh&) {}

			template<typename Body>
			void forAll(const Body& body) { }
		};


		template<
			template<typename,typename,unsigned> class Config,
			typename Mesh, unsigned Layers, typename ... Nodes
		>
		struct Data {

			template<typename Node, unsigned Level>
			using node_ref = typename Mesh::template node_ref_type<Node,Level>;

			DataStore<Config,Mesh,Layers,Layers-1,Nodes...> store;

			Data(const Mesh& m) : store(m) {}

			template<typename Body>
			void forAll(const Body& body) {
				store.forAll(body);
			}

			template<typename Node, unsigned Level>
			typename Config<Mesh,Node,Level>::element_type&
			operator[](const node_ref<Node,Level>& ref) {
				return store[ref];
			}

			template<typename Node, unsigned Level>
			const typename Config<Mesh,Node,Level>::element_type&
			operator[](const node_ref<Node,Level>& ref) const {
				return store[ref];
			}
		};

	}

	template<typename Mesh,template<typename,typename,unsigned> class Config>
	class NodeData;

	template<
		template <unsigned,typename,typename,typename> class Mesh,
		unsigned Layers,
		typename ... Nodes,
		typename Edges,
		typename Hierarchies,
		template<typename,typename,unsigned> class Config
	>
	class NodeData<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>,Config> {

		using mesh_type = Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>;

		template<typename Node, unsigned Level>
		using node_ref = typename mesh_type::template node_ref_type<Node,Level>;

		const mesh_type& mesh;

	protected:

		detail::Data<Config,mesh_type,Layers,Nodes...> data;

	public:

		NodeData(const mesh_type& mesh) : mesh(mesh), data(mesh) {}

		const mesh_type& getMesh() const {
			return mesh;
		}

		template<typename Body>
		void forAll(const Body& body) {
			data.forAll(body);
		}

		template<typename Node, unsigned Level>
		typename Config<mesh_type,Node,Level>::element_type&
		operator[](const node_ref<Node,Level>& ref) {
			return data[ref];
		}

		template<typename Node, unsigned Level>
		const typename Config<mesh_type,Node,Level>::element_type&
		operator[](const node_ref<Node,Level>& ref) const {
			return data[ref];
		}

	};

} // end namespace fine_open
} // end namespace allscale
