#pragma once

#include <allscale/fine_open/fine_open_mesh_permutation.h>
#include <string>

#include "allscale/fine_open/fine_open_mesh.h"
#include "allscale/fine_open/fine_open_mesh_importer.h"
#include "allscale/fine_open/fine_open_mesh_partitioner.h"
#include "allscale/fine_open/fine_open_mesh_pointcloud_exporter.h"
#include "allscale/fine_open/fine_open_partition_propagation.h"
#include "fine_open_reordering.h"

namespace allscale {
namespace fine_open {

	template<unsigned Layers>
	Geometry<Layers> loadGeometry(const std::string& filename = "mesh_out.dat") {
		std::ifstream in(filename, std::ios_base::in | std::ios_base::binary);
		return Geometry<Layers>::load(in);
	}

	template<unsigned Layers>
	struct FineOpenMeshBuilder {

		std::string output_file;

		FineOpenMeshBuilder(const std::string& file) : output_file(file) {}

		Geometry<Layers> build() {
			// ----- load geometry -----
			Geometry<Layers> geom = loadGeometry<Layers>(output_file);

			// ----- get top-level mesh -----
			auto tlm = extractTopLevelGeometry<4>(geom);

			// ----- partition top-level-mesh -----
			auto tlp = partition(tlm);

			// ----- extract partial partition -----
			std::vector<Partition> partialPartition(tlm.getMesh().template numNodes<Cell>());
			tlm.getMesh().template forAll<Cell>([&](const auto& cur){
				partialPartition[cur.getOrdinal()] = tlp.template get<CellPartition>(cur);
			});

			// ----- create flattend mesh -----
			auto flatMesh = utils::flatten(geom.getMesh());

			// ----- propagate TLP to whole Geometry -----
			auto fullPartition = propagatePartition(flatMesh,partialPartition, geom);

			// ----- reordering ------
			auto res = reordering(geom.getMesh(), fullPartition, flatMesh);

			return res;
		}
	};

} // end namespace fine_open
} // end namespace allscale
