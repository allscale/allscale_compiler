namespace allscale {
namespace fine_open {
    // Creation of the cell node connectivity for export of the results
    template<unsigned NumLayers, unsigned Level>
    void get_cells_to_nodes(const Geometry<NumLayers>& geom,
                             iDincells<NumLayers,Level>& Cell2Node,
                             iDinNodes<NumLayers,Level>& Node2Cell) {

         auto& NodePos = geom.template get<NodePosition,Level>();
         auto& mesh = geom.template getMesh();
         mesh.template forAll<Cell,Level>([&](auto IdCell){

             const auto& Faces = mesh.template getNeighbors<Cell_2_Face>(IdCell);
             const auto& BFaces = mesh.template getNeighbors<Cell_2_BoundaryFace>(IdCell);

             bool first = true;
             int counter = 0;

            for(auto IdFace : Faces) {
            std::vector<Point> points;

            const auto& Nodes = mesh.template getNeighbors<Face_2_Node>(IdFace);
            if (first) {
                for (auto Node : Nodes) {
                        Cell2Node[IdCell].push_back(Node.id);
                        Node2Cell[Node].push_back(IdCell.id);
                        points.push_back(NodePos[Node]);
                        counter++;
                }
                first = false;
            }
            else {
               bool is_oposite_face = true;
               for (auto Node : Nodes) {
                   for (int i = 0; i< Cell2Node[IdCell].size(); i++){
                       if (Cell2Node[IdCell][i] == Node.id)
                       {
                           is_oposite_face = false;
                           break;
                       }
                   }
               }
               if (is_oposite_face) {
                   for (auto Node : Nodes) {
                       points.push_back(NodePos[Node]);
                       Cell2Node[IdCell].push_back(Node.id);
                       Node2Cell[Node].push_back(IdCell.id);


                   }
                   Point N1, N2;
                   N1 = cross(points[3]-points[0],points[1]-points[0]);
                   N2 = cross(points[7]-points[4],points[5]-points[4]);

                   if (N1==N2){
                       int tmp = Cell2Node[IdCell][7];
                       Cell2Node[IdCell][7] =  Cell2Node[IdCell][5];
                       Cell2Node[IdCell][5] =  tmp;
                   }
               }
            }
         }
            for(auto IdFace : BFaces) {
                std::vector<Point> points;

                 const auto& Nodes = mesh.template getNeighbors<BoundaryFace_2_Node>(IdFace);
                 if (first) {
                     for (auto Node : Nodes) {
                             Cell2Node[IdCell].push_back(Node.id);
                             Node2Cell[Node].push_back(IdCell.id);
                             points.push_back(NodePos[Node]);

                             counter++;
                     }
                     first = false;
                 }
                 else {
                    bool is_oposite_face = true;
                    for (auto Node : Nodes) {
                        for (int i = 0; i< Cell2Node[IdCell].size(); i++){
                            if (Cell2Node[IdCell][i] == Node.id)
                            {
                                is_oposite_face = false;
                                break;
                            }
                        }
                    }
                    if (is_oposite_face) {
                        for (auto Node : Nodes) {
                            points.push_back(NodePos[Node]);
                            Cell2Node[IdCell].push_back(Node.id);
                            Node2Cell[Node].push_back(IdCell.id);

                        }
                        Point N1, N2;
                        N1 = cross(points[3]-points[0],points[1]-points[0]);
                        N2 = cross(points[7]-points[4],points[5]-points[4]);

                        if (!(N1==N2)){
                            int tmp = Cell2Node[IdCell][7];
                            Cell2Node[IdCell][7] =  Cell2Node[IdCell][5];
                            Cell2Node[IdCell][5] =  tmp;
                        }
                    }
                 }

             }
        });
    }


    template<unsigned NumLayers, unsigned Level, const int NumVar>
    void update_data_from_Cell_2_Node(const Geometry<NumLayers>& geom,
                                      const fields_in_Cells<NumLayers,Level>& fieldC,
                                      fields_in_Nodes<NumLayers,Level>& fieldN, int* idVar){

        auto& mesh = geom.template getMesh();

        mesh.template pforAll<Node,Level>([&] (auto node){
            auto cells = mesh.template getNeighbors<Node_2_Cell>(node);
            for (int i = 0; i<NumVar;i++) {
                int iVar = idVar[i];
                fieldN[iVar][node] = 0;
                for (auto cell : cells){
                    fieldN[iVar][node] += fieldC[iVar][cell];
                }
                fieldN[iVar][node]= fieldN[iVar][node]/cells.size();
            }
        });
    }

    // export the results in vtk file format.
    // Note that the CFD fields are exported at the nodes and not in the cells
    template<unsigned NumLayers, unsigned Level, const int NumVar>
    void export_to_paraview(const Geometry<NumLayers>& geom,
                            const fields_in_Nodes<NumLayers,Level>& field, int* idVar,
                            const std::string& paraview_file) {
        std::cout << "Saving results to paraview file format ... \n";
        std::ofstream fout(paraview_file);

        auto& mesh = geom.template getMesh();
        auto& HexCellId   = geom.template get<HexCell_2_GlobalCellId,Level>(); 	// CellId of HEXA Cells
        auto& TetraCellId = geom.template get<TetraCell_2_GlobalCellId,Level>();	// CellId of TETRA Cells
        auto& PrismCellId = geom.template get<PrismCell_2_GlobalCellId,Level>();	// CellId of PRISM Cells
        auto& PyramCellId = geom.template get<PyramCell_2_GlobalCellId,Level>();	// CellId of PYRAM Cells


        int NbNode = mesh.template numNodes<Node,Level>();//0;		// Number of Nodes
        int NbCell = mesh.template numNodes<Cell,Level>();		// Number of Cells



        auto Cell2Node = mesh.template createNodeData<Cell,std::vector<int>,Level>();
        auto Node2Cell = mesh.template createNodeData<Node,std::vector<int>,Level>();
        get_cells_to_nodes<NumLayers,Level>(geom, Cell2Node, Node2Cell);

        auto& NodePos = geom.template get<NodePosition,Level>();
        int iOffset = 0;

        std::vector<int> nbNodepercell(NbCell);	// Number of Nodes for each cell
        std::vector<int> cellType(NbCell);	// Celltype of each cell


        fout << "<?xml version=\"1.0\"?>" << "\n";
        fout << "<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\">" << "\n";
        fout << "<UnstructuredGrid>" << "\n";
        fout << "<Piece NumberOfPoints=\"" << NbNode << "\" NumberOfCells=\"" << NbCell << "\">" << "\n";
        fout << "<Points>" << "\n";
        fout << "<DataArray type=\"Float32\" NumberOfComponents=\"3\" format=\"ascii\">" << "\n";
        mesh.template forAll<Node,Level>([&](auto n){
            fout<< NodePos[n].x << " " << NodePos[n].y << " " << NodePos[n].z << "\n";
        });
        fout << "</DataArray>" << "\n";
        fout << "</Points>" << "\n";
        fout << "<Cells>" << "\n";
        fout << "<DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">" << "\n";


        mesh.template forAll<Cell,Level>([&](auto IdCell){
            auto nodes = mesh.template getNeighbors<Cell_2_Node>(IdCell);

            mesh.template forAll<HexCell,Level>([&](auto c){
                if (IdCell == HexCellId[c]) {
                    assert(Cell2Node[IdCell].size() == 8);
                    fout << Cell2Node[IdCell][0] << "\t";
                    fout << Cell2Node[IdCell][1] << "\t";
                    fout << Cell2Node[IdCell][2] << "\t";
                    fout << Cell2Node[IdCell][3] << "\t";
                    fout << Cell2Node[IdCell][4] << "\t";
                    fout << Cell2Node[IdCell][5] << "\t";
                    fout << Cell2Node[IdCell][6] << "\t";
                    fout << Cell2Node[IdCell][7] << "\t";


                    nbNodepercell[IdCell.id] = 8;
                    cellType[IdCell.id] = 12;

                }
            });

            mesh.template forAll<TetraCell,Level>([&](auto c){
                if (IdCell == TetraCellId[c]) {
                assert( Cell2Node[IdCell].size() == 4);
                fout << Cell2Node[IdCell][0] << "\t";
                fout << Cell2Node[IdCell][1] << "\t";
                fout << Cell2Node[IdCell][2] << "\t";
                fout << Cell2Node[IdCell][3] << "\t";

                nbNodepercell[IdCell.id] = 4;
                cellType[IdCell.id] = 10;

                }
            });

            mesh.template forAll<PrismCell,Level>([&](auto c){
                if (IdCell == PrismCellId[c]) {
                    assert(Cell2Node[IdCell].size() == 6);
                    fout << Cell2Node[IdCell][0] << "\t";
                    fout << Cell2Node[IdCell][1] << "\t";
                    fout << Cell2Node[IdCell][2] << "\t";
                    fout << Cell2Node[IdCell][4] << "\t";
                    fout << Cell2Node[IdCell][3] << "\t";
                    fout << Cell2Node[IdCell][5] << "\t";

                nbNodepercell[IdCell.id] = 6;
                cellType[IdCell.id] = 13;
                }
            });
            mesh.template forAll<PyramCell,Level>([&](auto c){
                if (IdCell == PyramCellId[c]) {
                    assert(Cell2Node[IdCell].size() == 5);
                    fout << Cell2Node[IdCell][0] << "\t";
                    fout << Cell2Node[IdCell][1] << "\t";
                    fout << Cell2Node[IdCell][2] << "\t";
                    fout << Cell2Node[IdCell][3] << "\t";
                    fout << Cell2Node[IdCell][4] << "\t";

                nbNodepercell[IdCell.id] = 5;
                cellType[IdCell.id] = 14;
                }
            });
            fout << '\n';
        });

        fout << "</DataArray>" << "\n";
        fout << "<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">" << '\n';
        mesh.template forAll<Cell,Level>([&](auto idCell){
            iOffset += nbNodepercell[idCell.id];
            fout << iOffset << '\n';
        });
        fout << "</DataArray>" << '\n';
        fout << "<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">" << '\n';
        mesh.template forAll<Cell,Level>([&](auto idCell){
            fout << cellType[idCell.id] << "\n";
        });
        fout << "</DataArray>" << '\n';
        fout << "</Cells>" << "\n";


// 		fout << "<CellData Scalars=\"Scalars\">" << "\n";
// 		fout << "<DataArray type=\"Float32\" Name=\"Temperature\" NumberOfComponents=\"1\" format=\"ascii\">" << "\n";
// 		mesh.forAll<Cell>([&] (auto idCell){
// 			fout << nbNode[idCell.id] << '\t'; // a remettre come il faut
// 		});
// 		fout << '\n';
// 		fout << "</DataArray>" << "\n";
// 		fout << "</CellData>" << "\n";
// 		fout << "<PointData Vectors=\"Vectors\">" << "\n";

        for (int i = 0; i<NumVar;i++) {
            int iVar = idVar[i];
            std::string varName;
            switch (iVar){
                case 0:
                    varName = "Temperature";

            }

            fout << "<PointData Scalars=\"Scalars\">" << "\n";
            fout << "<DataArray type=\"Float32\" Name=\""<< varName << "\" NumberOfComponents=\"1\" format=\"ascii\">" << "\n";


            mesh.template forAll<Node,Level>([&](auto n){
                fout<< field[iVar][n] << "\n";
            });

            fout << "</DataArray>" << "\n";
            fout << "</PointData>" << "\n";
        }

        fout << "</Piece> " << "\n";
        fout << "</UnstructuredGrid> " << '\n';
        fout << "</VTKFile>" << "\n";
        fout.close();
        std::cout << "Saving results to paraview file format ... done\n";

    }
}
}
