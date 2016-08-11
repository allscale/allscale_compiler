#pragma once

#include "allscale/utils/mesh.h"

namespace allscale {
namespace utils {


	// --- Geometry Data ---

	template<typename Node, typename T>
	struct geometry_data {
		using node_type = Node;
		using value_type = T;
	};

	namespace detail {

		template<typename Mesh, unsigned Level, typename ... Data>
		struct GeometryDataStoreLevel;

		template<typename Mesh, unsigned Level, typename First, typename ... Rest>
		struct GeometryDataStoreLevel<Mesh,Level,First,Rest...> {

			// extract nested type parameters
			using node_type = typename First::node_type;
			using value_type = typename First::value_type;

			template<typename Node, typename Value>
			using container_type = typename Mesh::template data_container<Node,Level,Value>;

			// the current data entry of this level
			container_type<node_type,value_type> data;

			// the remaining data of this level
			GeometryDataStoreLevel<Mesh,Level,Rest...> rest;

			// initialize this data store level
			GeometryDataStoreLevel(const Mesh& mesh)
				: data(mesh.template createNodeData<node_type,value_type,Level>()), rest(mesh) {}

			// initialize this data store level
			GeometryDataStoreLevel(GeometryDataStoreLevel&& other)
				: data(std::move(other.data)), rest(std::move(other.rest)) {}

		private:

			// initialize this data store level
			GeometryDataStoreLevel(container_type<node_type,value_type>&& data, GeometryDataStoreLevel<Mesh,Level,Rest...>&& rest)
				: data(std::move(data)), rest(std::move(rest)) {}

		public:

			template<typename Entry>
			typename std::enable_if<std::is_same<Entry,First>::value, container_type<typename Entry::node_type,typename Entry::value_type>&>::type
			get() {
				return data;
			}

			template<typename Entry>
			typename std::enable_if<std::is_same<Entry,First>::value, const container_type<typename Entry::node_type,typename Entry::value_type>&>::type
			get() const {
				return data;
			}

			template<typename Entry>
			typename std::enable_if<!std::is_same<Entry,First>::value, container_type<typename Entry::node_type,typename Entry::value_type>&>::type
			get() {
				return rest.template get<Entry>();
			}

			template<typename Entry>
			typename std::enable_if<!std::is_same<Entry,First>::value, const container_type<typename Entry::node_type,typename Entry::value_type>&>::type
			get() const {
				return rest.template get<Entry>();
			}

			bool operator==(const GeometryDataStoreLevel& other) const {
				return data == other.data && rest == other.rest;
			}

			void store(std::ostream& out) const {
				data.store(out);
				rest.store(out);
			}

			static GeometryDataStoreLevel load(std::istream& in) {
				auto data = container_type<node_type,value_type>::load(in);
				auto rest = GeometryDataStoreLevel<Mesh,Level,Rest...>::load(in);
				return GeometryDataStoreLevel(std::move(data), std::move(rest));
			}

		};

		template<typename Mesh, unsigned Level>
		struct GeometryDataStoreLevel<Mesh,Level> {

			// initialize this data store level
			GeometryDataStoreLevel() {}

			// initialize this data store level
			GeometryDataStoreLevel(const Mesh&) {}

			// initialize this data store level
			GeometryDataStoreLevel(GeometryDataStoreLevel&&) {}

			bool operator==(const GeometryDataStoreLevel& other) const {
				return true;
			}

			void store(std::ostream& ) const {
				/* nothing to do */
			}

			static GeometryDataStoreLevel load(std::istream& in) {
				return GeometryDataStoreLevel();
			}
		};


		template<typename Mesh,unsigned Level, typename ... Data>
		struct GeometryDataStore {

			// the nested levels
			GeometryDataStore<Mesh,Level-1,Data...> nested;

			// the data of this level
			GeometryDataStoreLevel<Mesh,Level,Data...> data;

			// initialize this data store
			GeometryDataStore(const Mesh& mesh)
				: nested(mesh), data(mesh) {};

			// move data store
			GeometryDataStore(GeometryDataStore&& other)
				: nested(std::move(other.nested)), data(std::move(other.data)) {};

		private:

			// initialize this data store with existing data
			GeometryDataStore(GeometryDataStore<Mesh,Level-1,Data...>&& nested, GeometryDataStoreLevel<Mesh,Level,Data...>&& data)
				: nested(std::move(nested)), data(std::move(data)) {};

		public:

			template<unsigned Lvl>
			typename std::enable_if<Lvl == Level, GeometryDataStoreLevel<Mesh,Lvl,Data...>&>::type
			getLevel() {
				return data;
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl == Level, const GeometryDataStoreLevel<Mesh,Lvl,Data...>&>::type
			getLevel() const {
				return data;
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl != Level, GeometryDataStoreLevel<Mesh,Lvl,Data...>&>::type
			getLevel() {
				return nested.template getLevel<Lvl>();
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl != Level, const GeometryDataStoreLevel<Mesh,Lvl,Data...>&>::type
			getLevel() const {
				return nested.template getLevel<Lvl>();
			}

			bool operator==(const GeometryDataStore& other) const {
				return nested == other.nested && data == other.data;
			}

			void store(std::ostream& out) const {
				nested.store(out);
				data.store(out);
			}

			static GeometryDataStore load(std::istream& in) {
				auto nested = GeometryDataStore<Mesh,Level-1,Data...>::load(in);
				auto data = GeometryDataStoreLevel<Mesh,Level,Data...>::load(in);
				return GeometryDataStore(std::move(nested), std::move(data));
			}
		};

		template<typename Mesh,typename ... Data>
		struct GeometryDataStore<Mesh,0,Data...> {

			// the data of this level
			GeometryDataStoreLevel<Mesh,0,Data...> data;

			// initialize this data store
			GeometryDataStore(const Mesh& mesh) : data(mesh) {};

		private:

			// initialize this data store based on existing data
			GeometryDataStore(GeometryDataStoreLevel<Mesh,0,Data...>&& data) : data(std::move(data)) {};

		public:

			// move data store
			GeometryDataStore(GeometryDataStore&& other) : data(std::move(other.data)) {};

			template<unsigned Lvl>
			typename std::enable_if<Lvl == 0, GeometryDataStoreLevel<Mesh,0,Data...>&>::type
			getLevel() {
				return data;
			}

			template<unsigned Lvl>
			typename std::enable_if<Lvl == 0, const GeometryDataStoreLevel<Mesh,0,Data...>&>::type
			getLevel() const {
				return data;
			}

			bool operator==(const GeometryDataStore& other) const {
				return data == other.data;
			}

			void store(std::ostream& out) const {
				data.store(out);
			}

			static GeometryDataStore load(std::istream& in) {
				return GeometryDataStore(GeometryDataStoreLevel<Mesh,0,Data...>::load(in));
			}

		};

	}


	template<
		typename Mesh,
		typename ... Data
	>
	class Geometry {

		using store_type = detail::GeometryDataStore<Mesh,Mesh::layers-1,Data...>;

		Mesh mesh;

		store_type m_store;

		template<typename Node, unsigned Level, typename Value>
		using container_type = typename Mesh::template data_container<Node,Level,Value>;

	public:

		using mesh_type = Mesh;

		enum { layers = Mesh::layers };

		Geometry(const Mesh& mesh)
			: mesh(mesh), m_store(this->mesh) {}

		Geometry(Mesh&& mesh)
			: mesh(std::move(mesh)), m_store(mesh) {}

		Geometry(Geometry&& other)
			: mesh(std::move(other.mesh)), m_store(std::move(other.m_store)) {}

	private:

		Geometry(Mesh&& mesh, store_type&& data)
			: mesh(std::move(mesh)), m_store(std::move(data)) {}

	public:

		const Mesh& getMesh() const {
			return mesh;
		}

		template<typename Entry, unsigned Level = 0>
		container_type<typename Entry::node_type, Level, typename Entry::value_type>& get() {
			return m_store.template getLevel<Level>().template get<Entry>();
		}

		template<typename Entry, unsigned Level = 0>
		const container_type<typename Entry::node_type, Level, typename Entry::value_type>& get() const {
			return m_store.template getLevel<Level>().template get<Entry>();
		}

		template<typename Entry, unsigned Level = 0, typename Node>
		typename Entry::value_type& get(const Node& node) {
			return get<Entry,Level>()[node];
		}

		template<typename Entry, unsigned Level = 0, typename Node>
		const typename Entry::value_type& get(const Node& node) const {
			return get<Entry,Level>()[node];
		}

		bool operator==(const Geometry& other) const {
			return mesh == other.mesh && m_store == other.m_store;
		}

		void store(std::ostream& out) const {
			mesh.store(out);
			write<std::size_t>(out,sizeof...(Data));
			m_store.store(out);
		}

		static Geometry load(std::istream& in) {
			auto mesh = Mesh::load(in);
			std::size_t num_entries = read<std::size_t>(in);
			if (sizeof...(Data) != num_entries) {
				std::cout << "Invalid input format - aborted!\n";
				exit(1);
			}
			auto data = detail::GeometryDataStore<Mesh,Mesh::layers-1,Data...>::load(in);
			return Geometry( std::move(mesh), std::move(data) );
		}
	};


} // end namespace utils
} // end namespace allscale
