#pragma once

#include <fstream>

#include "allscale/fine_open/fine_open_mesh.h"
#include "allscale/fine_open/fine_open_mesh_partitioner.h"
#include "fine_open_mesh_partitioner.h"

namespace allscale{
namespace fine_open {

	void exportTlpPointCloud (const TopLevelGeometry& geom, const PartitionedTopLevelMesh& tlp, std::string outputfile) {

		bool partitioned = false;

		auto& mesh = tlp.getMesh();

		unsigned max = (partitioned) ?
				std::numeric_limits<unsigned>::max() :
				mesh.numNodes<Cell>();

		double ratio = 1.0/max;

		// open file to write on
		std::ofstream file(outputfile);

		auto partitionMap = tlp.get<CellPartition>();

		int counter = 0;
		mesh.template forAll<Cell>([&] (const auto& cur) {

			// create the hue between 0 and 360°
			double h = (partitioned) ? (partitionMap[cur].id * ratio) : ((counter++) * ratio);

			// get geometry data for the cell
			Point data = geom.template get<CellCenter>(cur);
			// print stuff to file
			file << data.x << " " << data.y << " " << data.z << " " << h << "\n";
		});

		// end of filestream
		file.close();
	}

} // end namespace fine_open
} // end namespace allscale
