#pragma once

#include "allscale/utils/flattend_mesh.h"
#include "fine_open_mesh_partitioner.h"

namespace allscale {
namespace fine_open {

	template <typename FlattendMesh, typename  Geometry >
	std::vector<Partition> propagatePartition(FlattendMesh& flatMesh, std::vector<Partition>& partialPartition, Geometry& geom) {

		using NodeRef = typename FlattendMesh::NodeRef;
		// the resulting partition
		std::vector<Partition> fullPartition(flatMesh.numNodes());

		// add seeds (the partition number of the top-level nodes
		std::vector<bool> done(flatMesh.numNodes());
		std::vector<NodeRef> worklist;
		geom.getMesh().template forAll<Cell,3>([&](const auto& cur) {
			auto ref = flatMesh.toFlatRef(cur);

			// save partition
			fullPartition[ref.getOrdinal()] = partialPartition[cur.getOrdinal()];
			done[ref.getOrdinal()] = true;

			// mark as done
			worklist.push_back(ref);
		});

		// propagate levels
		while(!worklist.empty()) {

			std::cout << "worklist size: " << worklist.size() << "\n";

			// create list of next generation
			std::vector<NodeRef> next;

			// process current list
			for(const auto& cur : worklist) {

				// get partition of current
				auto partition = fullPartition[cur.getOrdinal()];

				// process all neighbors
				for(const auto& neighbor : flatMesh.getNeighbors(cur)) {

					// check whether already processed
					if (done[neighbor.getOrdinal()]) continue;

					// fix partition
					fullPartition[neighbor.getOrdinal()] = partition;

					// mark as done
					done[neighbor.getOrdinal()] = true;

					// add to worklist
					next.push_back(neighbor);
				}
			}

			// swap worklist and next
			std::swap(worklist,next);
		}

		// now all elements should be set
		int count = 0;
		flatMesh.forAll([&](const auto&, const auto& ref) {
			auto i = ref.getOrdinal();
			if (done[i]) return;
			++count;
			fullPartition[i] = { std::numeric_limits<std::size_t>::max() };
		});
		std::cout << "Number of disconnected nodes: " << count << " of " << flatMesh.numNodes() << "\n";

		return fullPartition;
	}

} // end namespace fine_open
} // end namespace allscale