#pragma once

#include <iostream>
#include <algorithm>

#include "allscale/fine_open/fine_open_mesh_permutation.h"

namespace allscale {
namespace fine_open {

	namespace detail {

		// ---------------------------------------------------------------------------------------------------
		// 										Mesh Permutation
		// ---------------------------------------------------------------------------------------------------

		template<typename Mesh, typename Node, unsigned Level>
		struct NodeSorterConfig {

			using element_type = std::pair<
					std::size_t,
					typename Mesh::template node_ref_type<Node,Level>
			>;

		};

	}


	template<typename mesh_type, typename partition_t, typename flat_t>
	mesh_type reordering(const mesh_type& mesh, const partition_t& fullPartition, const flat_t& flatMesh) {

		// the type to store pairs of partitions and node references
		using node_order = NodeData<mesh_type,detail::NodeSorterConfig>;

		// sort nodes according to partitions
		auto sorter = node_order(mesh);

		std::cout << "Filling in partition data ..\n";
		sorter.forAll([&](auto& container) {
			using container_type = typename std::remove_reference<decltype(container)>::type;
			using Node = typename container_type::node_type;
			enum { Level = container_type::level };

			// create identity permutation
			mesh.template forAll<Node,Level>([&](const auto& cur){
				auto partition = fullPartition[flatMesh.getOrdinal(cur)];
				container[cur] = { partition.id , cur };
			});
		});

		std::cout << "Sorting out node order ..\n";
		sorter.forAll([&](auto& container) {
			std::sort(container.begin(), container.end(), [](const auto& a, const auto& b) {
				return a.first < b.first;
			});
		});


		// permutation structure

		using builder_type = typename std::remove_reference<mesh_type>::type::builder_type;
		builder_type builder;

		// create nodes
		sorter.forAll([&](auto& container){
			using container_type = typename std::remove_reference<decltype(container)>::type;
			using Node = typename container_type::node_type;
			enum { Level = container_type::level };
			builder.template create<Node,Level>(container.size());
		});

		// create edges and hierarchies
		mesh.forAllLinks([&](const auto& edge, const auto& src, const auto& trg){
			using link_type = typename std::remove_cv<std::remove_reference_t<decltype(edge)>>::type;
			using src_ref_type = std::remove_reference_t<decltype(src)>;
			using trg_ref_type = std::remove_reference_t<decltype(trg)>;

			src_ref_type new_src = sorter[src].second;
			trg_ref_type new_trg = sorter[trg].second;

			builder.template link<link_type>(new_src,new_trg);
		});

		// get the resulting mesh
		return builder.build();
	}

} // end namespace fine_open
} // end namspace allscale
