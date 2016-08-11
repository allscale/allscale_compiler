#pragma once

#include "allscale/utils/mesh.h"
#include "allscale/fine_open/fine_open_mesh_partition.h"

namespace allscale {
namespace utils {
namespace mesh {


	template<typename TypeList, unsigned Layers, int Level, typename ValueType>
	class TypeListMapData;

	template<template<typename...> class List, typename First, typename ... Rest, unsigned Layers, int Level, typename ValueType>
	class TypeListMapData<List<First, Rest...>,Layers,Level,ValueType> {

		ValueType value;

		TypeListMapData<List<First,Rest...>,Layers,Level-1,ValueType> nested;

	public:

		template<typename NodeType, unsigned Lvl>
		typename std::enable_if<std::is_same<First,NodeType>::value && Lvl == Level, ValueType>::type& get() {
			return value;
		}

		template<typename NodeType, unsigned Lvl>
		const typename std::enable_if<std::is_same<First,NodeType>::value && Lvl == Level, ValueType>::type& get() const {
			return value;
		}

		template<typename NodeType, unsigned Lvl>
		typename std::enable_if<!(std::is_same<First,NodeType>::value && Lvl == Level), ValueType>::type& get() {
			return nested.template get<NodeType,Lvl>();
		}

		template<typename NodeType, unsigned Lvl>
		const typename std::enable_if<!(std::is_same<First,NodeType>::value && Lvl == Level), ValueType>::type& get() const {
			return nested.template get<NodeType,Lvl>();
		}

	};

	template<template<typename...> class List, typename First, typename ... Rest, unsigned Layers, typename ValueType>
	class TypeListMapData<List<First,Rest...>,Layers,-1,ValueType>{

		TypeListMapData<nodes<Rest...>,Layers,Layers-1,ValueType> nested;

	public:

		template<typename NodeType, unsigned Lvl>
		ValueType& get() {
			return nested.template get<NodeType,Lvl>();
		}

		template<typename NodeType, unsigned Lvl>
		const ValueType& get() const {
			return nested.template get<NodeType,Lvl>();
		}
	};

	template<template<typename...> class List, unsigned Layers, int Level, typename ValueType>
	class TypeListMapData<List<>,Layers,Level,ValueType>{

	};

	template<typename Mesh, typename ValueType>
	class NodeTypeMap;

	template<
		template <unsigned,typename,typename,typename> class Mesh,
		unsigned Layers, typename Nodes, typename Edges, typename Hierarchies,
		typename ValueType
	>
	class NodeTypeMap<Mesh<Layers,Nodes,Edges,Hierarchies>, ValueType> {

		TypeListMapData<Nodes, Layers, Layers-1, ValueType> data;

	public:

		template<typename NodeType, unsigned Level>
		ValueType& get(){
			return data.template get<NodeType,Level>();
		}

		template<typename NodeType, unsigned Level>
		const ValueType& get() const {
			return data.template get<NodeType,Level>();
		}

	};


	template<typename Mesh, typename ValueType>
	class EdgeTypeMap;

	template<
		template <unsigned,typename,typename,typename> class Mesh,
		unsigned Layers, typename Nodes, typename Edges, typename Hierarchies,
		typename ValueType
	>
	class EdgeTypeMap<Mesh<Layers,Nodes,Edges,Hierarchies>, ValueType> {

		TypeListMapData<Edges, Layers, Layers-1, ValueType> data;

	public:

		template<typename EdgeType, unsigned Level>
		ValueType& get(){
			return data.template get<EdgeType,Level>();
		}

		template<typename EdgeType, unsigned Level>
		const ValueType& get() const {
			return data.template get<EdgeType,Level>();
		}

	};

	struct NodeRange {
		std::size_t begin;
		std::size_t end;

		NodeRange() : begin(std::numeric_limits<std::size_t>::max()), end(0) {}
		void add(std::size_t element) {
			if(empty()) {
				begin = element;
				end = element;
			} else if (element < begin) {
				begin = element;
			} else if (element > end) {
				end = element;
			}
		}

		NodeRange(std::size_t _begin, std::size_t _end) : begin(_begin), end(_end) {}

		bool empty() const {
			return begin > end;
		}

		bool contains(const std::size_t element) const {
			return begin <= element && element <= end;
		}

		bool operator==(const NodeRange& other) const {
			return (empty() && other.empty()) || (begin == other.begin && end == other.end);
		}

		friend std::ostream& operator<< (std::ostream& out, const NodeRange& range) {
			if (range.empty()) {
				out << "[]";
			} else {
				out << "[" << range.begin << "," << range.end << "]";
			}
			return out;
		}

		static bool isSubset(const NodeRange& a, const NodeRange& b) {
			if (a.empty()) return true;
			if (b.empty()) return false;
			return b.begin <= a.begin && a.end <= b.end;
		}

		static NodeRange merge(const NodeRange& a, const NodeRange& b) {
			if(a.empty()) return b;
			if(b.empty()) return a;
			return NodeRange(std::min(a.begin,b.begin), std::max(a.end,b.end));
		}

		static NodeRange diff(const NodeRange& a, const NodeRange& b) {
			if (b.empty()) return a;
			if (!isSubset(b,a)) return a;
			if (a == b) return NodeRange();
			// TODO: be more accurate
			return a;	// fallback over-approximation
		}
	};

	template<
		typename Mesh,
		unsigned PartitionLevels,
		typename ClosureSet = NodeRange
	>
	class PartitionTree {

		struct Partition {

			NodeTypeMap<Mesh, NodeRange> ranges;
			EdgeTypeMap<Mesh, ClosureSet> edgeClosures;
//			HierarchyTypeMap<Mesh, ClosureSet> hierarchyClosures;

			// TODO:
			//		- store closures for all edges

		};

		template<typename Node, unsigned Level>
		using NodeRef = typename Mesh::template node_ref_type<Node,Level>;

		enum { num_entries = (1 << PartitionLevels) };

		Partition partitions[num_entries];

	public:

		PartitionTree(const PartitionTree& other) = default;
		PartitionTree(PartitionTree&& other) = default;

		PartitionTree(const fine_open::MeshNodePartition<Mesh>& table) {
			// TODO: initialize the partition tree

			const auto& mesh = table.getMesh();

			// ----- filling node ranges -----

			{
				// fill the finest level of the partitiontree
				mesh.forAllNodeTypes([&](const auto& nodeType, const auto& lvl){
					// get NodeType and level from nodetype
					using NodeType = plain_type<decltype(nodeType)>;
					const unsigned Lvl = get_level<decltype(lvl)>::value;

					mesh.template forAll<NodeType,Lvl>([&](const auto& node){
						auto pid = table[node].id;

						std::size_t mask = (num_entries/2)-1;
						std::size_t partitionindex = (pid >> (sizeof(pid)*8-(PartitionLevels-1)) & mask) + num_entries/2;
						auto& range_value = partitions[partitionindex].ranges.template get<NodeType,Lvl>();

						range_value.add(node.getOrdinal());

//						std::cout << "PID:            " << std::bitset<64>(pid) << "\n";
//						std::cout << "partitionIndex: " << std::bitset<64>(partitionindex) << " = " << partitionindex << "/" << num_entries << "\n";
//						std::cout << "noderange:      " << range_value << "\n\n";
					});

				});

				// fill all the other levels up to the coarsest
				std::size_t upperBoundary = num_entries/2;
				std::size_t lowerBoundary = upperBoundary/2;

				while (lowerBoundary != 0) {
					for(std::size_t i = lowerBoundary; i< upperBoundary; i++) {
						mesh.forAllNodeTypes([&](const auto& nodeType, const auto& lvl){
							// get NodeType and level from nodetype
							using NodeType = plain_type<decltype(nodeType)>;
							const unsigned Lvl = get_level<decltype(lvl)>::value;

							partitions[i].ranges.template get<NodeType,Lvl>() = NodeRange::merge(partitions[i*2].ranges.template get<NodeType,Lvl>(), partitions[2*i+1].ranges.template get<NodeType,Lvl>());

//							std::cout << "position : " << i << "\t newrange: " << partitions[i].ranges.template get<NodeType,Lvl>() << "\n";
						});
					}
					upperBoundary/=2;
					lowerBoundary/=2;
				}
			}

			// ----- filling edge closures -----

//			std::cout << "----- filling edge closures -----\n";

			{
				// fill all the other levels up to the coarsest
				std::size_t upperBoundary = num_entries;
				std::size_t lowerBoundary = upperBoundary/2;

				// fill the finest level of the partitiontree
				for(std::size_t i = lowerBoundary; i < upperBoundary; ++i) {

					mesh.forAllEdgeTypes([&](const auto& edgeType, const auto& lvl){
						// get EdgeType and level from edgeType
						using EdgeType = plain_type<decltype(edgeType)>;
						const unsigned Lvl = get_level<decltype(lvl)>::value;

						// get edge-source type
						using SrcType = typename EdgeType::src_node_type;
						using TrgType = typename EdgeType::trg_node_type;

						auto& closure = partitions[i].edgeClosures.template get<EdgeType,Lvl>();
						auto& range = partitions[i].ranges.template get<TrgType,Lvl>();

						assert(closure.empty());

						// filling closure with targets
						const auto& src_range = partitions[i].ranges.template get<SrcType,Lvl>();
						for(std::size_t j = src_range.begin; j <= src_range.end; ++j) {
							for(const auto& cur : mesh.template getSinks<EdgeType>(NodeRef<SrcType,Lvl>(j))) {
								std::size_t pos = cur.getOrdinal();
								if (!range.contains(pos)) {
									closure.add(pos);
								}
							}
						}

//						std::cout << "position : " << i << "\t Level " << Lvl << "\t new closure: " << partitions[i].edgeClosures.template get<EdgeType,Lvl>() << "\n";
					});

				}

				// fill the rest
				upperBoundary /= 2;
				lowerBoundary /= 2;
				while (lowerBoundary != 0) {
					for(std::size_t i = lowerBoundary; i< upperBoundary; i++) {
						mesh.forAllEdgeTypes([&](const auto& edgeType, const auto& lvl){
							// get EdgeType and level from edgeType
							using EdgeType = plain_type<decltype(edgeType)>;
							const unsigned Lvl = get_level<decltype(lvl)>::value;
							using TrgType = typename EdgeType::trg_node_type;

							partitions[i].edgeClosures.template get<EdgeType,Lvl>() =
									ClosureSet::diff(
										ClosureSet::merge(
											partitions[i*2].edgeClosures.template get<EdgeType,Lvl>(),
											partitions[2*i+1].edgeClosures.template get<EdgeType,Lvl>()
										),
										partitions[i].ranges.template get<TrgType,Lvl>()
									);

//							std::cout << "position : " << i << "\t Level " << Lvl << "\t new closure: " << partitions[i].edgeClosures.template get<EdgeType,Lvl>() << "\n";
						});
					}
					upperBoundary/=2;
					lowerBoundary/=2;
				}
			}
		}

	};



} // end namespace mesh
} // end namespace utils
} // end namespace allscale
