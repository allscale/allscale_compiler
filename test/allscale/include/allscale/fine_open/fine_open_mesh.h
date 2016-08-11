#pragma once

#include "utils/point.h"
#include "allscale/utils/mesh.h"
#include "allscale/utils/geometry.h"

namespace allscale {
namespace fine_open {

	using namespace utils;

	using value_t = double;

	// ---------------------------------------------------------------------------------------
	//									 Mesh Definition
	// ---------------------------------------------------------------------------------------

	// -- define types to model the topology of meshes --

	// - elements -
	struct Cell {};
	struct Face {};
	struct Node {};
	struct InternalNode {};

	struct BoundaryFace {};
	struct BoundaryNode {};
	struct GeometricalSurface {};

	struct HexCell {};
	struct TetraCell {};
	struct PrismCell {};
	struct PyramCell {};


	// - connections -

	struct GeometricSurface_2_BoundaryFace : public utils::edge<GeometricalSurface,BoundaryFace> {};

	struct Face_2_Cell_Left_Upward    : public utils::edge<Face,Cell> {};
	struct Face_2_Cell_Right_Downward : public utils::edge<Face,Cell> {};
	struct BoundaryFace_2_Cell_Left_Upward : public utils::edge<BoundaryFace,Cell> {};

	struct Face_2_Node 			: public utils::edge<Face,Node> {};
	struct BoundaryFace_2_Node 	: public utils::edge<BoundaryFace,Node> {};

	struct BoundaryFace_2_BoundaryNode 			: public utils::edge<BoundaryFace,BoundaryNode> {};
	struct BoundaryNode_2_BoundaryFace 			: public utils::edge<BoundaryNode,BoundaryFace> {};

	struct Node_2_Cell 			: public utils::edge<Node,Cell> {};
    struct Cell_2_Node 			: public utils::edge<Cell,Node> {};

	// - inter-layer connections -
	struct Cell_2_Child 			: public utils::hierarchy<Cell,Cell> {};
	struct BoundaryFace_2_Child 	: public utils::hierarchy<BoundaryFace,BoundaryFace> {};
	struct Cell_2_Face : public utils::edge<Cell,Face> {};
	struct Cell_2_BoundaryFace : public utils::edge<Cell,BoundaryFace> {};


	// - define the mesh and builder -
	template<unsigned layers = 1>
	using MeshBuilder = utils::MeshBuilder<
			utils::nodes<
					Cell,
					Face,
					Node,
					InternalNode,
					BoundaryFace,
					BoundaryNode,
					GeometricalSurface,
					HexCell,
					TetraCell,
					PrismCell,
					PyramCell
			>,
			utils::edges<
					GeometricSurface_2_BoundaryFace,
					Face_2_Cell_Left_Upward,
					Face_2_Cell_Right_Downward,
					BoundaryFace_2_Cell_Left_Upward,
					Face_2_Node,
					Node_2_Cell,
                    Cell_2_Node,
					BoundaryFace_2_Node,
					BoundaryFace_2_BoundaryNode,
					BoundaryNode_2_BoundaryFace,
					Cell_2_Face,
					Cell_2_BoundaryFace
			>,
			utils::hierarchies<
					Cell_2_Child,
					BoundaryFace_2_Child
			>,
			layers
	>;

	template<unsigned layers = 1>
	using Mesh = typename MeshBuilder<layers>::mesh_type;


	using count_t = unsigned short;
	using id_t = int;

	// -- geometry data --

	struct CellVolume : public utils::geometry_data<Cell,value_t> {};
	struct CellCenter : public utils::geometry_data<Cell,Point> {};

	struct FaceSurface : public utils::geometry_data<Face,value_t> {};
	struct FaceCenter : public utils::geometry_data<Face,Point> {};
	struct FaceNormal : public utils::geometry_data<Face,Vector> {};

	struct BoundaryFaceSurface : public utils::geometry_data<BoundaryFace,value_t> {};
	struct BoundaryFaceCenter : public utils::geometry_data<BoundaryFace,Point> {};
	struct BoundaryFaceNormal : public utils::geometry_data<BoundaryFace,Vector> {};


	struct LocalPos_Face_Left_Upward 			: public utils::geometry_data<Face,short> {};
	struct LocalPos_Face_Right_Downward 		: public utils::geometry_data<Face,short> {};
	struct LocalPos_Boundary_Face_Left_Upward 	: public utils::geometry_data<BoundaryFace,short> {};

	struct AbsolutePos_Face_Left_Upward 			: public utils::geometry_data<Face,short> {};
	struct AbsolutePos_Face_Right_Downward 			: public utils::geometry_data<Face,short> {};
	struct AbsolutePos_Boundary_Face_Left_Upward 	: public utils::geometry_data<BoundaryFace,short> {};

	struct DistanceToWall : public utils::geometry_data<Cell,value_t> {};

	struct NumNodesPerFace 				: public utils::geometry_data<Face,count_t> {};
	struct NumNodesPerBoundaryFace 		: public utils::geometry_data<BoundaryFace,count_t> {};
	struct BoundaryFace_RevertedNormal  : public utils::geometry_data<BoundaryFace,bool> {};

	struct NumBoundaryFacesPerGeometricalSurface : public utils::geometry_data<GeometricalSurface,int> {};

	struct LinearInterpolationSupported : public utils::geometry_data<Node,bool> {};
	struct NumCellsConnectedToNode 		: public utils::geometry_data<Node,count_t> {};

	struct NumBoundaryNodesPerBoundaryFace 			: public utils::geometry_data<BoundaryFace,count_t> {};
	struct NumBoundaryNode_2_BoundaryNode 	: public utils::geometry_data<BoundaryNode,count_t> {};
	struct BoundaryFaceId_2_BoundaryNode			: public utils::geometry_data<BoundaryNode, std::vector<id_t>> {};


	struct GlobalNodeID 		: public utils::geometry_data<InternalNode,id_t> {};
	struct GlobalBoundaryNodeID : public utils::geometry_data<BoundaryNode,id_t> {};


	struct LinearInterpolationCoefficient_X : public utils::geometry_data<Node,value_t> {};
	struct LinearInterpolationCoefficient_Y : public utils::geometry_data<Node,value_t> {};
	struct LinearInterpolationCoefficient_Z : public utils::geometry_data<Node,value_t> {};


	struct NodePosition : public utils::geometry_data<Node,Point> {};

	// ?????????????????? hierarchy property ??????????????????????
	struct HexCell_2_GlobalCellId : public utils::geometry_data<HexCell,id_t> {};
	struct TetraCell_2_GlobalCellId : public utils::geometry_data<TetraCell,id_t> {};
	struct PrismCell_2_GlobalCellId : public utils::geometry_data<PrismCell,id_t> {};
	struct PyramCell_2_GlobalCellId : public utils::geometry_data<PyramCell,id_t> {};

	// ------------------- hierarchy property ---------------------
	struct CellId_2_ParentId : public utils::geometry_data<Cell, id_t> {};
	struct CellId_2_ChildId : public utils::geometry_data<Cell, std::vector<id_t>> {};
	struct BoundaryFace_2_ParentBoundaryFace : public utils::geometry_data<BoundaryFace, id_t> {};

	/**
	 * A class aggregating geometric information of a full mesh.
	 */
	template<unsigned Layers = 1>
	using Geometry = typename utils::Geometry<
			Mesh<Layers>,						// < the topological data
			CellCenter,							// < the center of each cell
			CellVolume,							// < the volume of each cell
			FaceSurface,
			FaceCenter,
			FaceNormal,
			BoundaryFaceSurface,
			BoundaryFaceCenter,
			BoundaryFaceNormal,
			LocalPos_Face_Left_Upward,
			LocalPos_Face_Right_Downward,
			LocalPos_Boundary_Face_Left_Upward,
			AbsolutePos_Face_Left_Upward,
			AbsolutePos_Face_Right_Downward,
			AbsolutePos_Boundary_Face_Left_Upward,
			DistanceToWall,
			NumNodesPerFace,
			NumNodesPerBoundaryFace,
			BoundaryFace_RevertedNormal,
			NumBoundaryFacesPerGeometricalSurface,
			LinearInterpolationSupported,
			NumCellsConnectedToNode,
			NumBoundaryNodesPerBoundaryFace,
			NumBoundaryNode_2_BoundaryNode,
			GlobalNodeID,
			GlobalBoundaryNodeID,
			LinearInterpolationCoefficient_X,
			LinearInterpolationCoefficient_Y,
			LinearInterpolationCoefficient_Z,
			NodePosition,
			HexCell_2_GlobalCellId,
			TetraCell_2_GlobalCellId,
			PrismCell_2_GlobalCellId,
			PyramCell_2_GlobalCellId,
			CellId_2_ParentId,
			BoundaryFace_2_ParentBoundaryFace,
			CellId_2_ChildId,
			BoundaryFaceId_2_BoundaryNode
	>;

} // end namespace fine_open
} // end namespace allscale
