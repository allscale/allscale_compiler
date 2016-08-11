#pragma once

#include "allscale/fine_open/fine_open_mesh_node_data.h"
#include "allscale/fine_open/fine_open_mesh_partitioner.h"

namespace allscale {
namespace fine_open {


	// ---------------------------------------------------------------------------------------------------
	// 										Mesh Partition
	// ---------------------------------------------------------------------------------------------------


	template<typename Mesh, typename Node, unsigned Level>
	struct PartitionConfig {
		using element_type = Partition;
	};


	template<typename Mesh>
	class MeshNodePartition;

	template<
		template <unsigned,typename,typename,typename> class Mesh,
		unsigned Layers,
		typename ... Nodes,
		typename Edges,
		typename Hierarchies
	>
	class MeshNodePartition<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>>
		: public NodeData<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>,PartitionConfig> {

		using super = NodeData<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>,PartitionConfig>;
		using mesh_type = Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>;

	public:

		MeshNodePartition(const mesh_type& mesh) : super(mesh) {}

	};

	template<typename Mesh>
	MeshNodePartition<Mesh> createPartition(const Mesh& mesh) {
		return MeshNodePartition<Mesh>(mesh);
	}

} // end namespace fine_open
} // end namespace allscale
