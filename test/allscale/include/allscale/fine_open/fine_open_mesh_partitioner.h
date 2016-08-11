#pragma once

#include <bitset>
#include "allscale/fine_open/fine_open_mesh.h"

namespace allscale {
namespace fine_open {

	// TopLevelMesh
	using TopLevelMeshBuilder = typename utils::MeshBuilder<
		utils::nodes<Cell>,
		utils::edges<>,
		utils::hierarchies<>,
		1
	>;

	using TopLevelMesh = typename TopLevelMeshBuilder::mesh_type;

	using TopLevelGeometry = typename utils::Geometry<
		TopLevelMesh,
		CellCenter
	>;

	template<unsigned num_levels>
	TopLevelGeometry extractTopLevelGeometry(const Geometry<num_levels>& mesh) {
		// get the builder
		TopLevelMeshBuilder builder;

		// migrate the cells
		std::map<
			typename Geometry<num_levels>::mesh_type::template node_ref_type<Cell,num_levels-1>,
			typename TopLevelGeometry::mesh_type::template node_ref_type<Cell>
		> cellMap;
		mesh.getMesh().template forAll<Cell,num_levels-1>([&](const auto& cur) {
			cellMap[cur] = builder.create<Cell>();
		});

		// migrate the cell coordinates
		TopLevelGeometry res(builder.build());

		auto& coordinatesIn = mesh.template get<CellCenter,num_levels-1>();
		auto& coordinatesOut = res.template get<CellCenter>();
		for(const auto& cur : cellMap) {
			coordinatesOut[cur.second] = coordinatesIn[cur.first];
		}

		// done
		return res;
	}


	struct Partition {
		std::size_t id = 0;
	};


	struct CellPartition : public utils::geometry_data<Cell,Partition> {};

	// PartitionedMesh
	using PartitionedTopLevelMesh = typename utils::Geometry<
		TopLevelMesh,
		CellPartition
	>;

	std::size_t computePartition(int bit, Point& range, Point& pos, std::size_t& res) {
		// we have reached the finest level
		if (bit < 0) return res;

		auto max = std::max(range.x, std::max(range.y, range.z));

		if (range.x == max) {
			// x is longest
			range.x = range.x - (range.x/2);

			if (pos.x >= range.x) {
				res |= 1ul<<bit;
				pos.x -= range.x;
			}

			return computePartition(bit-1, range, pos, res);

		} else if (range.y == max) {
			// y is longest
			range.y = range.y - (range.y/2);

			if (pos.y >= range.y) {
				res |= 1ul<<bit;
				pos.y -= range.y;
			}

			return computePartition(bit-1, range, pos, res);

		}

		// z is not shorter than any other
		range.z = range.z - (range.z/2);

		if (pos.z >= range.z) {
			res |= 1ul<<bit;
			pos.z -= range.z;
		}

		return computePartition(bit-1, range, pos, res);

	}


	Partition getPartitionOf(Point range, Point pos) {
		std::size_t p = 0;
		computePartition(sizeof(std::size_t)*8-1, range, pos, p);
		return Partition{p};
	}


	PartitionedTopLevelMesh partition(const TopLevelGeometry& geometry) {

		auto& mesh = geometry.getMesh();
		PartitionedTopLevelMesh res(mesh);

		bool first = true;
		Point min;
		Point max;

		const auto& coordinates = geometry.get<CellCenter>();
		mesh.template forAll<Cell>([&](const auto& cur) {
			const auto& pos = coordinates[cur];
			if (first) {
				min = max = pos;
				first = false;
			} else {
				min.x = std::min(min.x,pos.x);
				min.y = std::min(min.y,pos.y);
				min.z = std::min(min.z,pos.z);

				max.x = std::max(max.x,pos.x);
				max.y = std::max(max.y,pos.y);
				max.z = std::max(max.z,pos.z);
			}
		});


		std::cout << "Min: " << min << "\n";
		std::cout << "Max: " << max << "\n";
		std::cout << "Num Nodes: "<< mesh.numNodes<Cell>() << "\n";


		auto range = max - min;
		auto& partitionMap = res.get<CellPartition>();
		mesh.template forAll<Cell>([&](const auto& cur) {
			partitionMap[cur] = getPartitionOf(range,coordinates[cur]-min);
		});

		return res;
	}


} // end namespace fine_open
} // end namespace allscale
