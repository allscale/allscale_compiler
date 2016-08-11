
#include "allscale/utils/graph.h"

namespace allscale {
namespace utils {

	namespace multi_graph {

		template<typename node_info>
		struct NaiveTopology;
	}

	template<
		typename node_info,
		template<typename> class topology = multi_graph::NaiveTopology
	>
	class MultiLevelGraph;

	template<
		typename static_data,
		typename dynamic_data,
		template<typename> class topology
	>
	class MultiLevelGraph<node_info<static_data,dynamic_data>,topology> {

		using Topology = topology<node_info<static_data,dynamic_data>>;

	public:

		using node_set_type = typename Topology::set_type;
		using node_ref_type = typename Topology::node_ref_type;
		using topology_data = typename Topology::topology_data;
		using static_node_data = typename Topology::static_node_data;
		using dynamic_node_data = typename Topology::dynamic_node_data;

	private:

		Topology toplgy;

	public:

		node_ref_type createNode(const static_data& s, const dynamic_data& d) {
			return toplgy.createNode(s,d);
		}

		node_ref_type createBorderNode(const static_data& s, const dynamic_data& d) {
			return toplgy.createBorderNode(s,d);
		}

		node_ref_type createNode(const node_ref_type& parent, const static_data& s, const dynamic_data& d) {
			return toplgy.createNode(parent,s,d);
		}

		node_ref_type createBorderNode(const node_ref_type& parent, const static_data& s, const dynamic_data& d) {
			return toplgy.createBorderNode(parent,s,d);
		}

		void addEdge(const node_ref_type& a, const node_ref_type& b) {
			toplgy.addEdge(a,b);
		}

		// -- graph processing --


		topology_data getTopologyData() const {
			return toplgy.getTopologyData();
		}

		/**
		 * Creates a decomposable description of the set of included nodes.
		 */
		node_set_type partition(unsigned level) const {
			return toplgy.partition(level);
		}

		static_node_data createStaticData() const {
			return toplgy.createStaticData();
		}

		dynamic_node_data createDynamicData() const {
			return toplgy.createDynamicData();
		}

	};


} // end namespace utils
} // end namespace allscale

#include "allscale/utils/multi_graph/naive_multi_graph.h"
