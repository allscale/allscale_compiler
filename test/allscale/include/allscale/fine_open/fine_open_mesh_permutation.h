#pragma once

#include "allscale/fine_open/fine_open_mesh_node_data.h"

namespace allscale {
namespace fine_open {


	// ---------------------------------------------------------------------------------------------------
	// 										Mesh Permutation
	// ---------------------------------------------------------------------------------------------------


	template<typename Mesh, typename Node, unsigned Level>
	struct PermutationConfig {
		using element_type = typename Mesh::template node_ref_type<Node,Level>;
	};


	template<typename Mesh>
	class MeshNodePermutation;

	template<
		template <unsigned,typename,typename,typename> class Mesh,
		unsigned Layers,
		typename ... Nodes,
		typename Edges,
		typename Hierarchies
	>
	class MeshNodePermutation<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>>
		: public NodeData<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>,PermutationConfig> {

		using super = NodeData<Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>,PermutationConfig>;
		using mesh_type = Mesh<Layers,utils::nodes<Nodes...>,Edges,Hierarchies>;

	public:

		MeshNodePermutation(const mesh_type& mesh) : super(mesh) {

			// initiate permutation with the identity permutation
			this->data.forAll([&](auto& container){
				using container_type = typename std::remove_reference<decltype(container)>::type;
				using Node = typename container_type::node_type;
				enum { Level = container_type::level };

				// create identity permutation
				mesh.template forAll<Node,Level>([&](const auto& cur){
					container[cur] = cur;
				});
			});

		}

	};

	template<typename Mesh>
	MeshNodePermutation<Mesh> createPermuation(const Mesh& mesh) {
		return MeshNodePermutation<Mesh>(mesh);
	}

} // end namespace fine_open
} // end namespace allscale
