#pragma once

#include "allscale/fine_open/fine_open_mesh.h"

namespace allscale {
namespace fine_open {

	struct MalformedInputFileException : public std::exception {
		virtual const char* what() const noexcept {
			return "Malformed Input File!";
		}
	};

	template<typename T>
	T get(std::istream& in) {
		T value;
		in.read((char *) &value, sizeof(T));
		if (sizeof(T) != in.gcount()) {
			throw MalformedInputFileException();
		}
		return value;
	}

	template<typename T>
	void skip(std::istream& in, std::size_t times = 1) {
		for (std::size_t i = 0; i < times; i++) get<T>(in);
	}

	template<
			typename Node,
			unsigned level,
			typename Builder,
			typename Res = typename Builder::template node_list_type<Node,level>
	>
	Res createNodes(Builder& builder, int num_nodes) {
		return builder.template create<Node,level>(num_nodes);
	}

	template<typename Node, unsigned level>
	using node_ref = MeshBuilder<1>::mesh_type::template node_ref_type<Node,level>;

	template<typename Node, unsigned Level, typename Value>
	using DataCont = Geometry<1>::template container_type<Node, Level, Value>;


	struct MeshLoaderConfig {

		bool full;

		bool debug;

		bool loadCellCoordinates;
		bool loadCellVolume;

		bool loadFaceNormal;
		bool loadFaceArea;
		bool loadFaceCenter;

		bool loadBoundaryFaceNormal;
		bool loadBoundaryFaceArea;
		bool loadBoundaryFaceCenter;

		bool loadLocalPosFaceLeftUpward;
		bool loadLocalPosFaceRightDownward;
		bool loadLocalPosBFaceLeftUpward;

		bool loadAbsolutePosFaceLeftUpward;
		bool loadAbsolutePosFaceRightDownward;
		bool loadAbsolutePosBFaceLeftUpward;

		bool loadDistanceToWall;

		bool loadGlobalNodeId;
		bool loadGlobalBNodeID;

		bool loadLinearInterpolationCoeff_X;
		bool loadLinearInterpolationCoeff_Y;
		bool loadLinearInterpolationCoeff_Z;

		bool loadNodePosition;

		bool loadHexCell2GlobalCellId;
		bool loadTetraCell2GlobalCellId;
		bool loadPrismCell2GlobalCellId;
		bool loadPyramCell2GlobalCellId;

	private:

		MeshLoaderConfig(bool full, bool flag = true)
			: full(full),
			  debug(false),
			  loadCellCoordinates(flag),
			  loadCellVolume(flag),
		      loadFaceNormal(flag),
		      loadFaceArea(flag),
			  loadFaceCenter(flag),
			  loadBoundaryFaceNormal(flag),
			  loadBoundaryFaceArea(flag),
			  loadBoundaryFaceCenter(flag),
			  loadLocalPosFaceLeftUpward(flag),
			  loadLocalPosFaceRightDownward(flag),
			  loadLocalPosBFaceLeftUpward(flag),
			  loadAbsolutePosFaceLeftUpward(flag),
			  loadAbsolutePosFaceRightDownward(flag),
			  loadAbsolutePosBFaceLeftUpward(flag),
			  loadDistanceToWall(flag),
			  loadGlobalNodeId(flag),
			  loadGlobalBNodeID(flag),
			  loadLinearInterpolationCoeff_X(flag),
			  loadLinearInterpolationCoeff_Y(flag),
			  loadLinearInterpolationCoeff_Z(flag),
			  loadNodePosition(flag),
			  loadHexCell2GlobalCellId(flag),
			  loadTetraCell2GlobalCellId(flag),
			  loadPrismCell2GlobalCellId(flag),
			  loadPyramCell2GlobalCellId(flag)
		{}

	public:

		static MeshLoaderConfig getDefaultLight(bool loadAllFields = false) {
			return MeshLoaderConfig(false,loadAllFields);
		}

		static MeshLoaderConfig getDefaultFull(bool loadAllFields = false) {
			return MeshLoaderConfig(true,loadAllFields);
		}
	};

	template<int iLevel>
	struct MeshLoader {

		template<typename Kind, unsigned Level>
		using node_list_type = typename MeshBuilder<4>::template node_list_type<Kind,Level>;

		MeshLoader<iLevel-1> nested_loader;

		node_list_type<Cell,iLevel> cells;
		node_list_type<Face,iLevel> faces;
		node_list_type<Node,iLevel> nodes;

		node_list_type<BoundaryFace,iLevel> bfaces;
		node_list_type<GeometricalSurface,iLevel> surfaces;

		node_list_type<InternalNode,iLevel> internal_nodes;
		node_list_type<BoundaryNode,iLevel> boundary_nodes;

		node_list_type<HexCell,iLevel> hex_cells;
		node_list_type<TetraCell,iLevel> tetra_cells;
		node_list_type<PrismCell,iLevel> prism_cells;
		node_list_type<PyramCell,iLevel> pyram_cells;

		std::vector<
			std::pair<node_ref<Cell,iLevel>, int>
		> cells_2_parent;

		std::vector<
			std::pair<node_ref<BoundaryFace,iLevel>, int>
		> bfaces_2_parent;


		template<unsigned num_levels>
		void loadMesh(std::istream& in, MeshBuilder<num_levels>& builder, const MeshLoaderConfig& config) {

			// load nested level
			nested_loader.loadMesh(in,builder,config);

			// and load structure of current level
			std::size_t num_nodes = get<int>(in);
			nodes = createNodes<Node,iLevel>(builder,num_nodes);

			std::size_t num_cells = get<int>(in);
			cells = createNodes<Cell,iLevel>(builder,num_cells);

			std::size_t num_faces = get<int>(in);
			faces = createNodes<Face,iLevel>(builder,num_faces);

			std::size_t num_geometrical_surfaces = get<int>(in);
			surfaces = createNodes<GeometricalSurface,iLevel>(builder,num_geometrical_surfaces);

			std::vector<std::size_t> boundary_faces_per_surface(num_geometrical_surfaces);

			// skip the number of boundary faces per geometrical surface
			std::size_t num_boundary_faces = 0;
			for(std::size_t i=0; i<num_geometrical_surfaces; i++) {
				boundary_faces_per_surface[i] = get<int>(in);
				if (config.debug) std::cout << "GS " << i << " has " << boundary_faces_per_surface[i] << " bfaces.\n";
				num_boundary_faces += boundary_faces_per_surface[i];
			}
			bfaces = createNodes<BoundaryFace,iLevel>(builder, num_boundary_faces);

            std::vector<std::vector<std::size_t>> Face2Cell(num_faces);
            std::vector<std::vector<std::size_t>> BFace2Cell(num_boundary_faces);

            std::vector<std::vector<std::size_t>> Cell2Node(num_cells);

			if (config.debug) std::cout << "Loading Level " << iLevel << "\n";
			if (config.debug) std::cout << "Number of nodes: " << num_nodes << "\n";
			if (config.debug) std::cout << "Number of cells: " << num_cells << "\n";
			if (config.debug) std::cout << "Number of faces: " << num_faces << "\n";
			if (config.debug) std::cout << "Number of geometrical surfaces: " << num_geometrical_surfaces << "\n";
			if (config.debug) std::cout << "Number of boundary faces: " << num_boundary_faces << "\n";


			std::size_t num_internal_nodes = 0;

			std::vector<std::size_t> num_boundary_nodes_per_surface(num_geometrical_surfaces);

			// reproduce control flow of exporter
			if (iLevel == 0) {
				if (config.full) {
					// Number of internal nodes
					num_internal_nodes = get<int>(in);
					if (config.debug) std::cout << "Number of internal nodes: " << num_internal_nodes << "\n";
					internal_nodes = createNodes<InternalNode,iLevel>(builder,num_internal_nodes);

					// Number of nodes per geometrical surfaces
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
						num_boundary_nodes_per_surface[iGS] = get<int>(in);
						if (config.debug) std::cout << "GS " << iGS << " has " << num_boundary_nodes_per_surface[iGS] << " Nodes.\n";
					}

				}

				// Nodes coordinates
				for (std::size_t iNode = 0; iNode< num_nodes; iNode++) {
					skip<double>(in,3);
				}

			}

			if (config.full) {

				// Internal Face normal
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					skip<double>(in,3);
				}

				// Internal Face area
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					skip<double>(in);
				}

				// Internal Face center
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					skip<double>(in,3);
				}

				// Boundary Face normal - surface - center
				for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
					for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++)
					{
						skip<double>(in,7);
					}
				}


				// Cell center
				for (std::size_t iCell = 0; iCell< num_cells; iCell++) {
					skip<double>(in,3);
				}


				// Cell volume
				for (std::size_t iCell = 0; iCell< num_cells; iCell++) {
					skip<double>(in);
				}

				if (iLevel == 0) {

					// Nb of HEXA Cell
					std::size_t num_hex_cells = get<int>(in);
					if (config.debug) std::cout << "Number of hex cells: " << num_hex_cells << "\n";
					skip<int>(in,num_hex_cells);
					hex_cells = createNodes<HexCell,iLevel>(builder,num_hex_cells);

					// Nb of TETRA Cell
					std::size_t num_tetra_cells = get<int>(in);
					if (config.debug) std::cout << "Number of tetra cells: " << num_tetra_cells << "\n";
					skip<int>(in,num_tetra_cells);
					tetra_cells = createNodes<TetraCell,iLevel>(builder,num_tetra_cells);

					// Nb of PRISM Cell
					std::size_t num_prism_cells = get<int>(in);
					if (config.debug) std::cout << "Number of prism cells: " << num_prism_cells << "\n";
					skip<int>(in,num_prism_cells);
					prism_cells = createNodes<PrismCell,iLevel>(builder,num_prism_cells);

					// Nb of PYRAM Cell
					std::size_t num_pyram_cells = get<int>(in);
					if (config.debug) std::cout << "Number of pyram cells: " << num_pyram_cells << "\n";
					skip<int>(in,num_pyram_cells);
					pyram_cells = createNodes<PyramCell,iLevel>(builder,num_pyram_cells);

				}

			}

			// Internal Face Cell connectivity
			for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
				int cell = get<int>(in);
				builder.template link<Face_2_Cell_Left_Upward>(faces[iFace],cells[cell]);
				builder.template link<Cell_2_Face>(cells[cell],faces[iFace]);

                Face2Cell[faces[iFace].id].push_back(cell);
			}

			for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
				int cell = get<int>(in);
				builder.template link<Face_2_Cell_Right_Downward>(faces[iFace],cells[cell]);
				builder.template link<Cell_2_Face>(cells[cell],faces[iFace]);

                Face2Cell[faces[iFace].id].push_back(cell);
			}

			// Boundary Face Cell connectivity
			int bface_counter = 0;
			for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
				for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++) {
					int cell = get<int>(in);
					builder.template link<BoundaryFace_2_Cell_Left_Upward>(bfaces[bface_counter],cells[cell]);
					builder.template link<Cell_2_BoundaryFace>(cells[cell],bfaces[bface_counter]);
                    builder.template link<GeometricSurface_2_BoundaryFace>(surfaces[iGS],bfaces[bface_counter]);

                    BFace2Cell[bfaces[bface_counter].id].push_back(cell);
					bface_counter++;
				}
			}

			// Internal Face Position of face
			skip<short>(in,2*num_faces);

			// Boundary Face Position of face
			skip<short>(in,num_boundary_faces);


			// Internal Face Absolute Position of face
			for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
				skip<short>(in,2);
			}

			// Boundary Face Absolute Position of face
			for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
				for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++) {
					skip<short>(in);
				}
			}


			if (iLevel == 0) {

				// Nb node per Internal Face
				std::vector<std::size_t> num_nodes_per_face(num_faces);
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					num_nodes_per_face[iFace] = get<int>(in);
				}

				// Internal Face Node connection
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					for (std::size_t iNodeOfFace = 0; iNodeOfFace < num_nodes_per_face[iFace]; iNodeOfFace++) {
						int node = get<int>(in);
						builder.template link<Face_2_Node>(faces[iFace],nodes[node]);
                        int FaceID= faces[iFace].id;
                        for (std::size_t i = 0; i < Face2Cell[faces[iFace].id].size(); i++) {
                            Cell2Node[ Face2Cell[FaceID][i] ].push_back(nodes[node].id);
                        }
					}
				}

				// Nb node per Boundary Face
				bface_counter = 0;
				std::vector<std::size_t> num_boundary_face_nodes(num_boundary_faces);
				for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
					for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++) {
						num_boundary_face_nodes[bface_counter] = get<int>(in);
						bface_counter++;
					}
				}

				// Boundary Face Node connection
				int offset = 0;
				for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
					for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++) {
						for (std::size_t iNodeOfFace = 0; iNodeOfFace < num_boundary_face_nodes[iFace + offset]; iNodeOfFace++) {
							int node = get<int>(in);
							builder.template link<BoundaryFace_2_Node>(bfaces[iFace+offset],nodes[node]);
                            int BFaceId= bfaces[iFace+offset].id;
                            for (std::size_t i = 0; i < BFace2Cell[bfaces[iFace+offset].id].size(); i++) {
                                Cell2Node[ BFace2Cell[BFaceId][i] ].push_back(nodes[node].id);
                            }
						}
					}
					offset += boundary_faces_per_surface[iGS];
				}

                // Link the Cell_2_Node and Node_2_Cell connectivity
                for (std::size_t IdCell = 0; IdCell < num_cells; IdCell++) {
                    bool first = true;
                    std::vector<std::size_t> Nodelist;
                    std::vector<Point> points;

                    for(std::size_t IdNode = 0; IdNode < Cell2Node[IdCell].size(); IdNode++) {
                        if (first){
                            Nodelist.push_back(Cell2Node[IdCell][IdNode]);
//                            points.push_back(Cell2Node[IdCell][IdNode]);
                            first = false;
                       }
                        else {
                            bool is_in_node_list = false;
                            for (std::size_t i=0; i <  Nodelist.size(); i++){
                                if ( Nodelist[i] == Cell2Node[IdCell][IdNode])
                                {
                                    is_in_node_list=true;
                                }
                            }
                            if (is_in_node_list==false)
                            {
                                Nodelist.push_back(Cell2Node[IdCell][IdNode]);
                            }
                        }
                    }

                    for (std::size_t i=0; i<Nodelist.size(); i++){
                        builder.template link<Node_2_Cell>( nodes[Nodelist[i]], cells[IdCell] );
                        builder.template link<Cell_2_Node>( cells[IdCell], nodes[Nodelist[i]] );
                    }
                }

				if (config.full) {

					// Connection internal node to global node
					skip<int>(in, num_internal_nodes);


					// Connection boundary node to global node
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
						// load global IDs of boundary faces nodes
						skip<int>(in, num_boundary_nodes_per_surface[iGS]);
					}

					// Number of cell connected to internal node
					std::vector<std::size_t> num_cell_per_int_node(num_internal_nodes);
					for(std::size_t i = 0; i< num_internal_nodes; i++) {
						num_cell_per_int_node[i] = get<int>(in);
					}

					// Connection internal node to cell
					for (std::size_t iNode = 0; iNode < num_internal_nodes; iNode++) {
						skip<int>(in, num_cell_per_int_node[iNode]);
					}

					// sum up number of boundary nodes per faces
					int num_boundary_nodes = 0;
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
						num_boundary_nodes += num_boundary_nodes_per_surface[iGS];
					}

					if (config.debug) std::cout << "Number of boundary nodes: " << num_boundary_nodes << "\n";
					boundary_nodes = createNodes<BoundaryNode,iLevel>(builder,num_boundary_nodes);

					std::vector<std::size_t> number_of_boundary_faces_connected_to_boundary_node(num_boundary_nodes);

					// Number of boundary face connected to boundary node
					int counter = 0;
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
						for(std::size_t i=0; i<num_boundary_nodes_per_surface[iGS]; i++) {
							number_of_boundary_faces_connected_to_boundary_node[counter++] = get<int>(in);
						}
					}

					// Connection boundary node to boundary face
					counter = 0;
                    offset = 0;
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++)
					{
						for (std::size_t iNode = 0; iNode < num_boundary_nodes_per_surface[iGS]; iNode++)
						{
							for (std::size_t iBFaceOfNode = 0; iBFaceOfNode < number_of_boundary_faces_connected_to_boundary_node[counter]; iBFaceOfNode++)
							{
								// get the boundary face ID and link it to the current boundary node
								auto bface = bfaces[get<int>(in)+offset];
								auto bnode = boundary_nodes[counter];
								builder.template link<BoundaryFace_2_BoundaryNode>(bface,bnode);
								builder.template link<BoundaryNode_2_BoundaryFace>(bnode,bface);
							}
							counter++;
						}
                       offset+=boundary_faces_per_surface[iGS];
					}

					// Check if linear interpolation can be used or not
					for (std::size_t iNode = 0; iNode < num_nodes; iNode++)
					{
						skip<short>(in);
					}

					// Coefficients along X for linear node interpolation
					for (std::size_t iNode = 0; iNode < num_nodes; iNode++)
					{
						skip<double>(in);
					}

					// Coefficients along Y for linear node interpolation
					for (std::size_t iNode = 0; iNode < num_nodes; iNode++)
					{
						skip<double>(in);
					}

					// Coefficients along Z for linear node interpolation
					for (std::size_t iNode = 0; iNode < num_nodes; iNode++)
					{
						skip<double>(in);
					}

				}


			}

			if(config.full) {
				// Distance To wall
				for (std::size_t iCell = 0; iCell < num_cells; iCell++) {
					skip<double>(in);
				}
			}

			if (iLevel != (num_levels - 1)) { // Not on coarse level

				// Connection Cell to Parent Cell
				for (std::size_t iCell = 0; iCell <num_cells; iCell++) {
					auto child = cells[iCell];
					cells_2_parent.push_back({child, get<int>(in)});
				}

				// Connection Boundary Face to Boundary Face Parent
				for(std::size_t i = 0; i < num_boundary_faces; i++ ) {
					auto child = bfaces[i];
					bfaces_2_parent.push_back({child, get<int>(in)});
				}

			}


			if (iLevel != 0) { // Except for the finest level

				std::vector<std::size_t> num_children(num_cells);
				for (std::size_t iCell = 0; iCell < num_cells; iCell++)
				{
					// consume number of children
					num_children[iCell] = get<int>(in);
				}

				for (std::size_t iCell = 0; iCell < num_cells; iCell++)
				{
					for(std::size_t i = 0; i<num_children[iCell]; i++) {
						// consume entry
						skip<int>(in);		// TODO: link parent iCell with retrieved child
					}
				}

			}

			if (config.debug) std::cout << "Done\n\n";
		}

		template<unsigned num_levels>
		void loadGeometry(std::istream& in, const MeshLoaderConfig& config, Geometry<num_levels>& geometry) {

			// load nested level
			nested_loader.template loadGeometry<num_levels>(in,config,geometry);

			if (config.debug) std::cout << "Loading geometry of level " << iLevel << "\n";

			// and load structure of current level
			std::size_t num_nodes = get<int>(in);
			assert(num_nodes == nodes.size());

			std::size_t num_cells = get<int>(in);
			assert(num_cells == cells.size());

			std::size_t num_faces = get<int>(in);
			assert(num_faces == faces.size());

			std::size_t num_geometrical_surfaces = get<int>(in);
			assert(num_geometrical_surfaces == surfaces.size());

			std::vector<std::size_t> boundary_faces_per_surface(num_geometrical_surfaces);

			// collect the number of boundary faces per geometrical surface
			std::size_t num_boundary_faces = 0;
			for(std::size_t i=0; i<num_geometrical_surfaces; i++) {
				boundary_faces_per_surface[i] = get<int>(in);
				num_boundary_faces += boundary_faces_per_surface[i];
			}
			assert(num_boundary_faces == bfaces.size());


			std::size_t num_internal_nodes = 0;

			std::vector<std::size_t> num_boundary_nodes_per_surface(num_geometrical_surfaces);

			// reproduce control flow of exporter
			if (iLevel == 0) {
				// Number of internal nodes
				if (config.full) {
					num_internal_nodes = get<int>(in);
					if (config.debug) std::cout << "Number of internal nodes: " << num_internal_nodes << "\n";
					assert(internal_nodes.size() == num_internal_nodes);

					// Number of nodes per geometrical surfaces
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
						num_boundary_nodes_per_surface[iGS] = get<int>(in);
					}
				}

				// Nodes coordinates
				if (config.loadNodePosition) {
					auto& node_coordinates = geometry.template get<NodePosition,iLevel>();

					for (std::size_t iNode = 0; iNode< num_nodes; iNode++) {
						node_coordinates[nodes[iNode]].x = get<double>(in);
					}
					for (std::size_t iNode = 0; iNode< num_nodes; iNode++) {
						node_coordinates[nodes[iNode]].y = get<double>(in);
					}
					for (std::size_t iNode = 0; iNode< num_nodes; iNode++) {
						node_coordinates[nodes[iNode]].z = get<double>(in);
					}
				} else {
					skip<double>(in,3*num_nodes);
				}

			}

			if (config.full) {

				// Internal Face normal
				if (config.loadFaceNormal) {
					auto& face_normals = geometry.template get<FaceNormal,iLevel>();
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_normals[faces[iFace]].x = get<double>(in);
					}
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_normals[faces[iFace]].y = get<double>(in);
					}
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_normals[faces[iFace]].z = get<double>(in);
					}
				} else {
					skip<double>(in,3*num_faces);
				}

				// Internal Face area
				if (config.loadFaceArea) {
					auto& face_area = geometry.template get<FaceSurface,iLevel>();
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_area[faces[iFace]] = get<double>(in);
					}
				} else {
					skip<double>(in,num_faces);
				}

				// Internal Face center
				if (config.loadFaceCenter) {
					auto& face_centers = geometry.template get<FaceCenter,iLevel>();
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_centers[faces[iFace]].x = get<double>(in);
					}
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_centers[faces[iFace]].y = get<double>(in);
					}
					for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
						face_centers[faces[iFace]].z = get<double>(in);
					}
				} else {
					skip<double>(in,3*num_faces);
				}

				// Boundary Face normal - surface - center
                int offset = 0;
				for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {

					// normal
					if(config.loadBoundaryFaceNormal) {
						auto& bface_normals = geometry.template get<BoundaryFaceNormal, iLevel>();
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_normals[bfaces[iFace+offset]].x = get<double>(in);
						}
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_normals[bfaces[iFace+offset]].y = get<double>(in);
						}
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_normals[bfaces[iFace+offset]].z = get<double>(in);
						}
					} else {
						skip<double>(in, 3*boundary_faces_per_surface[iGS]);
					}

					// area
					if(config.loadBoundaryFaceArea) {
						auto& bface_area = geometry.template get<BoundaryFaceSurface, iLevel>();
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_area[bfaces[iFace+offset]] = get<double>(in);
						}
					} else {
						skip<double>(in, boundary_faces_per_surface[iGS]);
					}

					// position
					if(config.loadBoundaryFaceCenter) {
						auto& bface_center = geometry.template get<BoundaryFaceCenter, iLevel>();
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_center[bfaces[iFace+offset]].x = get<double>(in);
						}
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_center[bfaces[iFace+offset]].y = get<double>(in);
						}
						for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
							bface_center[bfaces[iFace+offset]].z = get<double>(in);
						}
					} else {
						skip<double>(in, 3*boundary_faces_per_surface[iGS]);
					}
                    offset+=boundary_faces_per_surface[iGS];
				}

				// Cell center
				if (config.loadCellCoordinates) {
					auto& cell_centers = geometry.template get<CellCenter,iLevel>();
					for (std::size_t iCell = 0; iCell< num_cells; iCell++) {
						cell_centers[cells[iCell]].x = get<double>(in);
					}
					for (std::size_t iCell = 0; iCell< num_cells; iCell++) {
						cell_centers[cells[iCell]].y = get<double>(in);
					}
					for (std::size_t iCell = 0; iCell< num_cells; iCell++) {
						cell_centers[cells[iCell]].z = get<double>(in);
					}
				} else {
					skip<double>(in,3*num_cells);
				}


				// Cell volume
				if (config.loadCellVolume) {
					auto& cell_volumes = geometry.template get<CellVolume,iLevel>();
					for (std::size_t iCell = 0; iCell< num_cells; iCell++) {
						cell_volumes[cells[iCell]] = get<double>(in);
					}
				} else {
					skip<double>(in,num_cells);
				}

				if (iLevel == 0) {

					// Nb of HEXA Cell
					std::size_t num_hex_cells = get<int>(in);
					assert(num_hex_cells == hex_cells.size());
					if(config.loadHexCell2GlobalCellId) {
						auto& hexCellIds = geometry.template get<HexCell_2_GlobalCellId, iLevel>();
						for (std::size_t iCell = 0; iCell < num_hex_cells; iCell++) {
							hexCellIds[hex_cells[iCell]] = get<int>(in);
						}
					}else {
						skip<int>(in, num_hex_cells);
					}

					// Nb of TETRA Cell
					std::size_t num_tetra_cells = get<int>(in);
					assert(num_tetra_cells == tetra_cells.size());
					if(config.loadTetraCell2GlobalCellId) {
						auto& tetraCellIds = geometry.template get<TetraCell_2_GlobalCellId,iLevel>();
						for(std::size_t iCell = 0; iCell < num_tetra_cells; iCell++) {
							tetraCellIds[tetra_cells[iCell]] = get<int>(in);
						}
					} else {
						skip<int>(in, num_tetra_cells);
					}

					// Nb of PRISM Cell
					std::size_t num_prism_cells = get<int>(in);
					assert(num_prism_cells == prism_cells.size());
					if(config.loadPrismCell2GlobalCellId) {
						auto& prismCellIds = geometry.template get<PrismCell_2_GlobalCellId, iLevel>();
						for (std::size_t iCell = 0; iCell < num_prism_cells; iCell++) {
							prismCellIds[prism_cells[iCell]] = get<int>(in);
						}
					} else {
						skip<int>(in, num_prism_cells);
					}

					// Nb of PYRAM Cell
					std::size_t num_pyram_cells = get<int>(in);
					assert(num_pyram_cells == pyram_cells.size());
					if(config.loadPyramCell2GlobalCellId) {
						auto& pyramCellIds = geometry.template get<PyramCell_2_GlobalCellId, iLevel>();
						for (std::size_t iCell = 0; iCell < num_pyram_cells; iCell++) {
							pyramCellIds[pyram_cells[iCell]] = get<int>(in);
						}
					} else {
						skip<int>(in, num_pyram_cells);
					}

				}

			}

			// Internal Face Cell connectivity
			skip<int>(in,2*num_faces);

			// Boundary Face Cell connectivity
			skip<int>(in,num_boundary_faces);


			// Internal Face Position of face
			if(config.loadLocalPosFaceLeftUpward) {
				auto& localPosFaceLeftUpward = geometry.template get<LocalPos_Face_Left_Upward, iLevel>();
				for (std::size_t iFace = 0; iFace < num_faces; iFace++) {
					localPosFaceLeftUpward[faces[iFace]] = get<short>(in);
				}
			} else {
				skip<short>(in, num_faces);
			}

			if(config.loadLocalPosFaceRightDownward) {
				auto& localPosFaceRightDownward = geometry.template get<LocalPos_Face_Right_Downward, iLevel>();
				for (std::size_t iFace = 0; iFace < num_faces; iFace++) {
					localPosFaceRightDownward[faces[iFace]] = get<short>(in);
				}
			} else {
				skip<short>(in, num_faces);
			}

			// Boundary Face Position of face
			int counter = 0;
			// LocalPos_Boundary_Face_Left_Upward;
			auto& localPosBoundaryFaceLeftUpward = geometry.template get<LocalPos_Boundary_Face_Left_Upward,iLevel>();
			for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
				if(config.loadLocalPosBFaceLeftUpward) {
					for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
						localPosBoundaryFaceLeftUpward[bfaces[counter++]] = get<short>(in);
					}
				} else {
					skip<short>(in, boundary_faces_per_surface[iGS]);
				}
			}


			// Internal Face Absolute Position of face
			if(config.loadAbsolutePosFaceLeftUpward) {
				auto& absolutePosFaceLeftUpward = geometry.template get<AbsolutePos_Face_Left_Upward, iLevel>();
				for (std::size_t iFace = 0; iFace < num_faces; iFace++) {
					absolutePosFaceLeftUpward[faces[iFace]] = get<short>(in);
				}
			} else {
				skip<short>(in, num_faces);
			}

			if(config.loadAbsolutePosFaceRightDownward) {
				auto& absolutePosFaceRightDownward = geometry.template get<AbsolutePos_Face_Right_Downward, iLevel>();
				for (std::size_t iFace = 0; iFace < num_faces; iFace++) {
					absolutePosFaceRightDownward[faces[iFace]] = get<short>(in);
				}
			} else {
				skip<short>(in, num_faces);
			}

			// Boundary Face Absolute Position of face
			counter = 0;
			auto& absolutePosBoundaryFaceLeftUpward = geometry.template get<AbsolutePos_Boundary_Face_Left_Upward,iLevel>();
			for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
				if(config.loadAbsolutePosBFaceLeftUpward) {
					for (std::size_t iFace = 0; iFace < boundary_faces_per_surface[iGS]; iFace++) {
						absolutePosBoundaryFaceLeftUpward[bfaces[counter++]] = get<short>(in);
					}
				} else {
					skip<short>(in, boundary_faces_per_surface[iGS]);
				}
			}


			if (iLevel == 0) {

				// Nb node per Internal Face
				std::vector<std::size_t> num_nodes_per_face(num_faces);
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					num_nodes_per_face[iFace] = get<int>(in);
				}

				// Internal Face Node connection
				for (std::size_t iFace = 0; iFace< num_faces; iFace++) {
					for (std::size_t iNodeOfFace = 0; iNodeOfFace < num_nodes_per_face[iFace]; iNodeOfFace++) {
						skip<int>(in);
					}
				}

				// Nb node per Boundary Face
				counter = 0;
				std::vector<std::size_t> num_boundary_face_nodes(num_boundary_faces);
				for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
					for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++) {
						num_boundary_face_nodes[counter] = get<int>(in);
						counter++;
					}
				}

				// Boundary Face Node connection
				int offset = 0;
				for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
					for (std::size_t iFace = 0; iFace< boundary_faces_per_surface[iGS]; iFace++) {
						for (std::size_t iNodeOfFace = 0; iNodeOfFace < num_boundary_face_nodes[iFace + offset]; iNodeOfFace++) {
							skip<int>(in);
						}
					}
					offset += boundary_faces_per_surface[iGS];
				}


				if (config.full) {

					// Connection internal node to global node
					if(config.loadGlobalNodeId) {
						auto& internal_node_2_global_node_ids = geometry.template get<GlobalNodeID, iLevel>();
						for (std::size_t iNode = 0; iNode < num_internal_nodes; iNode++) {
							internal_node_2_global_node_ids[internal_nodes[iNode]] = get<int>(in);
						}
					} else {
						skip<int>(in, num_internal_nodes);
					}


					// Connection boundary node to global node
                        offset = 0;
						auto& boundary_node_2_global_node_ids = geometry.template get<GlobalBoundaryNodeID, iLevel>();
						for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
							if(config.loadGlobalBNodeID) {
								for (std::size_t iNode = 0; iNode < num_boundary_nodes_per_surface[iGS]; iNode++) {
									boundary_node_2_global_node_ids[boundary_nodes[iNode+offset]] = get<int>(in);
								}
							} else {
								skip<int>(in,num_boundary_nodes_per_surface[iGS]);
							}
                            offset+=num_boundary_nodes_per_surface[iGS];
						}


					// Number of cell connected to internal node
					std::vector<std::size_t> num_cell_per_int_node(num_internal_nodes);
					for(std::size_t i = 0; i< num_internal_nodes; i++) {
						num_cell_per_int_node[i] = get<int>(in);
					}

					// Connection internal node to cell
					for (std::size_t iNode = 0; iNode < num_internal_nodes; iNode++) {
						skip<int>(in, num_cell_per_int_node[iNode]);
					}

					// sum up number of boundary nodes per faces
					int num_boundary_nodes = boundary_nodes.size();
					if (config.debug) std::cout << "Number of boundary nodes: " << num_boundary_nodes << "\n";

					std::vector<std::size_t> number_of_boundary_faces_connected_to_boundary_node(num_boundary_nodes);

					// Number of boundary face connected to boundary node
					int counter = 0;
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++) {
						for(std::size_t i=0; i<num_boundary_nodes_per_surface[iGS]; i++) {
							number_of_boundary_faces_connected_to_boundary_node[counter++] = get<int>(in);
						}
					}

					// Connection boundary node to boundary face
					counter = 0;
					for (std::size_t iGS = 0; iGS < num_geometrical_surfaces; iGS++)
					{
						for (std::size_t iNode = 0; iNode < num_boundary_nodes_per_surface[iGS]; iNode++)
						{
							for (std::size_t iBFaceOfNode = 0; iBFaceOfNode < number_of_boundary_faces_connected_to_boundary_node[counter]; iBFaceOfNode++)
							{
								// skip this entry
								skip<int>(in);
							}
							counter++;
						}
					}

					// Check if linear interpolation can be used or not
					std::vector<bool> check_linear_interpolation(num_nodes);
					for (std::size_t iNode = 0; iNode < num_nodes; iNode++)
					{
						check_linear_interpolation[iNode] = get<short>(in);
					}

					// Coefficients along X for linear node interpolation
					if(config.loadLinearInterpolationCoeff_X) {
						auto& linear_interpol_coefficient_x = geometry.template get<LinearInterpolationCoefficient_X, iLevel>();
						for (std::size_t iNode = 0; iNode < num_nodes; iNode++) {
							linear_interpol_coefficient_x[nodes[iNode]] = get<double>(in);
						}
					} else {
						skip<double>(in, num_nodes);
					}

					// Coefficients along Y for linear node interpolation
					if(config.loadLinearInterpolationCoeff_Y) {

						auto& linear_interpol_coefficient_y = geometry.template get<LinearInterpolationCoefficient_Y, iLevel>();
						for (std::size_t iNode = 0; iNode < num_nodes; iNode++) {
							linear_interpol_coefficient_y[nodes[iNode]] = get<double>(in);
						}
					} else {
						skip<double>(in, num_nodes);
					}

					// Coefficients along Z for linear node interpolation
					if(config.loadLinearInterpolationCoeff_Z) {
						auto& linear_interpol_coefficient_z = geometry.template get<LinearInterpolationCoefficient_Z, iLevel>();
						for (std::size_t iNode = 0; iNode < num_nodes; iNode++) {
							linear_interpol_coefficient_z[nodes[iNode]] = get<double>(in);
						}
					} else {
						skip<double>(in, num_nodes);
					}
				}
			}

			if(config.full) {
				// Distance To wall
				if(config.loadDistanceToWall) {
					auto& dist_2_wall = geometry.template get<DistanceToWall, iLevel>();
					for (std::size_t iCell = 0; iCell < num_cells; iCell++) {
						dist_2_wall[cells[iCell]] = get<double>(in);
					}
				} else {
					skip<double>(in, num_cells);
				}
			}

			if (iLevel != (num_levels - 1)) { // Not on coarse level

				// Connection Cell to Parent Cell
				for (std::size_t iCell = 0; iCell <num_cells; iCell++) {
					skip<int>(in);
				}

				// Connection Boundary Face to Boundary Face Parent
				for(std::size_t i = 0; i < num_boundary_faces; i++ ) {
					// ignore parent for now
					skip<int>(in);
				}

			}

			if (iLevel != 0) { // Except for the finest level

				std::vector<std::size_t> num_children(num_cells);
				for (std::size_t iCell = 0; iCell < num_cells; iCell++)
				{
					// consume number of children
					num_children[iCell] = get<int>(in);
				}

				for (std::size_t iCell = 0; iCell < num_cells; iCell++)
				{
					for(std::size_t i = 0; i<num_children[iCell]; i++) {
						// consume entry
						skip<int>(in);
					}
				}

			}

			if (config.debug) std::cout << "Done\n\n";

		}

	};

	template<>
	struct MeshLoader<-1> {
		template<unsigned num_levels>
		void loadMesh(std::istream& in, MeshBuilder<num_levels>& builder, const MeshLoaderConfig& config) {
			// nothing to do :)
		}

		template<unsigned num_levels>
		void linkHierarchy(MeshBuilder<num_levels>& builder, const MeshLoader<0>& parent) {
			// nothing to do :)
		}

		void linkHierarchy(MeshBuilder<1>& builder, const MeshLoader<0>& parent = MeshLoader<0>()) {
			// nothing to do :)
		}

		template<unsigned num_levels>
		void loadGeometry(std::istream& in, const MeshLoaderConfig& config, Geometry<num_levels>& geometry) {
			// nothing to do :)
		}
	};

	template<unsigned iLevel>
	struct HierarchyConnector {

		template<unsigned num_levels>
		void link(MeshBuilder<num_levels>& builder, const MeshLoader<iLevel>& parent) {

			// link lower levels
			HierarchyConnector<iLevel-1> nested;
			nested.link(builder, parent.nested_loader);

			// link child cells of nested layer to cells on this level
			for(const auto& cur : parent.nested_loader.cells_2_parent) {
				auto prent = parent.cells[cur.second];
				auto child = cur.first;
				builder.template link<Cell_2_Child>(prent,child);
			}

			// link child bfaces of nested layer to bfaces on this level
			for(const auto& cur : parent.nested_loader.bfaces_2_parent) {
				auto prent = parent.bfaces[cur.second];
				auto child = cur.first;
				builder.template link<BoundaryFace_2_Child>(prent,child);
			}
		}

	};

	template<>
	struct HierarchyConnector<0> {

		template<unsigned num_levels>
		void link(MeshBuilder<num_levels>& builder, const MeshLoader<0>& parent) {
			// nothing to do
		}
	};


	template<unsigned num_levels>
	Geometry<num_levels> importMesh(std::istream& in, const MeshLoaderConfig& config) {

		if (config.debug) std::cout << "Number of levels: " << num_levels << "\n";

		// Step 1 - build Mesh

		MeshBuilder<num_levels> builder;
		MeshLoader<num_levels-1> loader;

		// consume levels ...
		auto start = in.tellg();
		loader.loadMesh(in,builder,config);

		// link hierarchy levels
		HierarchyConnector<num_levels-1> connector;
		connector.link(builder, loader);

		// Step 2 - load geometric data
		if (config.debug) std::cout << "Creating Geometry ..\n";
		Geometry<num_levels> res(builder.build());
		if (config.debug) std::cout << "Filling Geometery ..\n";

		// rewind
		in.seekg(start);

		// load geometry
		loader.template loadGeometry<num_levels>(in,config,res);

		// done
		return res;
	}

	template<unsigned num_levels>
	Geometry<num_levels> importFromFile(const std::string& file_name, const MeshLoaderConfig& config) {

		// open file stream
		auto in = std::ifstream(file_name, std::ios_base::in | std::ios_base::binary);

		// load number of levels
		int actual_num_levels = get<int>(in);

		// check that the number of levels is correct
		if (num_levels != actual_num_levels) {
			assert(false && "Incorrect number of levels!");
			return Geometry<num_levels>(typename Geometry<num_levels>::mesh_type());
		}

		// import the mesh
		return importMesh<num_levels>(in,config);
	}

} // end namespace fine_open
} // end namespace allscale
