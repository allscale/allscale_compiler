namespace allscale {
namespace fine_open {

using value_t = double;

template <unsigned NumLayers>
using mesh_type = typename Geometry<NumLayers>::mesh_type;

template <unsigned NumLayers, unsigned Level, typename Node,typename Value>
using dataCont = typename mesh_type<NumLayers>::template data_container<Node, Level, Value>;

template <unsigned NumLayers, unsigned Level>
using iDincells = dataCont<NumLayers,Level,Cell,std::vector<int>>;

template <unsigned NumLayers, unsigned Level>
using iDinNodes = dataCont<NumLayers,Level,Node,std::vector<int>>;

template <unsigned NumLayers, unsigned Level>
using field_in_Nodes = dataCont<NumLayers,Level,Node,value_t>;

template <unsigned NumLayers, unsigned Level>
using field_in_Cells = dataCont<NumLayers,Level,Cell,value_t>;

template <unsigned NumLayers, unsigned Level>
using fields_in_Nodes =  std::vector<dataCont<NumLayers,Level,Node,value_t>>;

template <unsigned NumLayers, unsigned Level>
using fields_in_Cells =  std::vector<dataCont<NumLayers,Level,Cell,value_t>>;

template <unsigned NumLayers, unsigned Level>
using fields_in_BFaces =  std::vector<dataCont<NumLayers,Level,BoundaryFace,value_t>>;

template <unsigned Level>
using idnodes =  const allscale::utils::range<const allscale::utils::mesh::reference::NodeRef <allscale::fine_open::Node, Level>*>;

    // computation of the gradients using diamond stencil
    template <unsigned NumLayers, unsigned Level, const int numVar>
    void localGradDiamondGeneric(const Geometry<NumLayers>& geom,
                                 int nbNodePerFace,
                                 idnodes<Level>& faceNode,
                                 Point cellCUp, Point cellCDn, Point facePos,
                                 double varInCellUp[numVar], double varInCellDn[numVar],
                                 fields_in_Nodes<NumLayers,Level>& varsInNode,  int* idVar,
                                 value_t gradIntFaceVar[numVar][3]){

            const auto& nodeC = geom.template get<NodePosition>();
            value_t epsTolGeo_= 1.e-20;
            int idNodeFace[12];
            value_t varInNode[numVar][12];

            value_t locNodeC0[12];
            value_t locNodeC1[12];
            value_t locNodeC2[12];

            value_t volSubVolUp	; // volume of the sub-volume upward
            value_t volSubVolDown; //    "   "   "      "      downward
            value_t invVolSubVol	; // inverse of the sum of both previous sub-volumes

            value_t subFaceUpN[12][3]	; // normal of the nbNodePerFace sub-face [up]
            value_t subFaceDownN[12][3]	; // normal of the nbNodePerFace sub-face [down]

            value_t gradVar[numVar][3]	; // gradient of U-velocity using upward control volume
            value_t oneThird = 1. / 3. ;

            int iNodeLoc = 0;
            for (auto idNode : faceNode){
                idNodeFace[iNodeLoc++] = idNode.id;
            }
    //        for (int iNodeLoc = 0; iNodeLoc < nbNodePerFace; iNodeLoc++)
    //        {
    //            idNodeFace[iNodeLoc] = faceNode[iNodeLoc];
    //        }
            idNodeFace[nbNodePerFace] = idNodeFace[0];

            value_t locFacePos0 = facePos.x;
            value_t locFacePos1 = facePos.y;
            value_t locFacePos2 = facePos.z;

            for (int iNodeLoc = 0; iNodeLoc < nbNodePerFace; iNodeLoc++)
            {
                int locIdNode = idNodeFace[iNodeLoc];
                for (int iVar = 0; iVar<numVar; ++iVar)
                {
                  varInNode[iVar][iNodeLoc]  = varsInNode[idVar[iVar]][locIdNode];
                }

                locNodeC0[iNodeLoc] = nodeC[locIdNode].x;
                locNodeC1[iNodeLoc] = nodeC[locIdNode].y;
                locNodeC2[iNodeLoc] = nodeC[locIdNode].z;
            }

            for (int iVar = 0; iVar<numVar; ++iVar){
              varInNode[iVar][nbNodePerFace] = varInNode[iVar][0];
            }

            int locIdNode = idNodeFace[0];

            locNodeC0[nbNodePerFace] = nodeC[locIdNode].x;
            locNodeC1[nbNodePerFace] = nodeC[locIdNode].y;
            locNodeC2[nbNodePerFace] = nodeC[locIdNode].z;

            value_t coeff = 0.5;


            volSubVolUp   = 0.0;
            volSubVolDown = 0.0;

            value_t subFaceNormal0, subFaceNormal1,subFaceNormal2;

            // Calculation of the normals of the nbNodePerFace sub surf. associated to each sub-vol

            for (int iNodeLoc = 0; iNodeLoc < nbNodePerFace; iNodeLoc++)
            {
                int iNodeLoc1 = iNodeLoc + 1;
                value_t N0locNodeC0 = locNodeC0[iNodeLoc];
                value_t N1locNodeC0 = locNodeC0[iNodeLoc1];
                value_t N0locNodeC1 = locNodeC1[iNodeLoc];
                value_t N1locNodeC1 = locNodeC1[iNodeLoc1];
                value_t N0locNodeC2 = locNodeC2[iNodeLoc];
                value_t N1locNodeC2 = locNodeC2[iNodeLoc1];

                subFaceUpN[iNodeLoc][0] = coeff * ((cellCUp.z - N0locNodeC2) * (cellCUp.y - N1locNodeC1) -
                                                   (cellCUp.y - N0locNodeC1) * (cellCUp.z - N1locNodeC2));

                subFaceUpN[iNodeLoc][1] = coeff * ((cellCUp.x - N0locNodeC0) * (cellCUp.z - N1locNodeC2) -
                                                   (cellCUp.z - N0locNodeC2) * (cellCUp.x - N1locNodeC0));

                subFaceUpN[iNodeLoc][2] = coeff * ((cellCUp.y - N0locNodeC1) * (cellCUp.x - N1locNodeC0) -
                                                   (cellCUp.x - N0locNodeC0) * (cellCUp.y - N1locNodeC1));

                subFaceDownN[iNodeLoc][0] = coeff * ((cellCDn.y - N0locNodeC1) * (cellCDn.z - N1locNodeC2) -
                                                     (cellCDn.z - N0locNodeC2) * (cellCDn.y - N1locNodeC1));

                subFaceDownN[iNodeLoc][1] = coeff * ((cellCDn.z - N0locNodeC2) * (cellCDn.x - N1locNodeC0) -
                                                     (cellCDn.x - N0locNodeC0) * (cellCDn.z - N1locNodeC2));

                subFaceDownN[iNodeLoc][2] = coeff * ((cellCDn.x - N0locNodeC0) * (cellCDn.y - N1locNodeC1) -
                                                     (cellCDn.y - N0locNodeC1) * (cellCDn.x - N1locNodeC0));

                subFaceNormal0 = 0.5 * ((locFacePos1 - N0locNodeC1) * (locFacePos2 - N1locNodeC2) -
                                                (locFacePos2 - N0locNodeC2) * (locFacePos1 - N1locNodeC1));

                subFaceNormal1 = 0.5 * ((locFacePos2 - N0locNodeC2) * (locFacePos0 - N1locNodeC0) -
                                                (locFacePos0 - N0locNodeC0) * (locFacePos2 - N1locNodeC2));

                subFaceNormal2 = 0.5 * ((locFacePos0 -  N0locNodeC0) *
                                        (locFacePos1 -  N1locNodeC1) -
                                        (locFacePos1 -  N0locNodeC1) *
                                        (locFacePos0 -  N1locNodeC0));

                volSubVolUp += fabs((N0locNodeC0 - cellCUp.x) * subFaceNormal0 +
                                    (N0locNodeC1 - cellCUp.y) * subFaceNormal1 +
                                    (N0locNodeC2 - cellCUp.z) * subFaceNormal2) * oneThird;

                volSubVolDown += fabs((N0locNodeC0 - cellCDn.x) * subFaceNormal0 +
                                      (N0locNodeC1 - cellCDn.y) * subFaceNormal1 +
                                      (N0locNodeC2 - cellCDn.z) * subFaceNormal2) * oneThird;
            }



            for (int iVar = 0; iVar<numVar; iVar++)
            {

                gradVar[iVar][0] = 0.0;
                gradVar[iVar][1] = 0.0;
                gradVar[iVar][2] = 0.0;

            }
            value_t varAvSubFace[numVar];
            // Calculation of gradients - contribution of the sub faces [up] and [down]
            for (int iNodeLoc = 0; iNodeLoc < nbNodePerFace; iNodeLoc++)
            {
                // Upward
                int iNodeLoc1 = iNodeLoc + 1;

                // Averaged variables on a level with each sub-face
                for (int iVar = 0; iVar<numVar; ++iVar){
                    varAvSubFace[iVar] = (varInNode[iVar][iNodeLoc] +
                                          varInNode[iVar][iNodeLoc1] +
                                          varInCellUp[iVar]) * oneThird;
                }

                // Gradient calculation [upward]
                value_t subFaceUpN0 = subFaceUpN[iNodeLoc][0];
                value_t subFaceUpN1 = subFaceUpN[iNodeLoc][1];
                value_t subFaceUpN2 = subFaceUpN[iNodeLoc][2];
                for (int iVar = 0; iVar<numVar; ++iVar){
                    gradVar[iVar][0] += varAvSubFace[iVar] * subFaceUpN0;
                    gradVar[iVar][1] += varAvSubFace[iVar] * subFaceUpN1;
                    gradVar[iVar][2] += varAvSubFace[iVar] * subFaceUpN2;
                }
            }

            for (int iNodeLoc = 0; iNodeLoc < nbNodePerFace; iNodeLoc++)   // Downward
            {
                int iNodeLoc1 = iNodeLoc + 1;

                // Averaged variables on a level with each sub-face
                for (int iVar = 0; iVar<numVar; ++iVar){
                    varAvSubFace[iVar] = (varInNode[iVar][iNodeLoc] +
                                          varInNode[iVar][iNodeLoc1] +
                                          varInCellDn[iVar]) * oneThird;
                }
                // Gradient calculation [downward]
                value_t subFaceDownN0 = subFaceDownN[iNodeLoc][0];
                value_t subFaceDownN1 = subFaceDownN[iNodeLoc][1];
                value_t subFaceDownN2 = subFaceDownN[iNodeLoc][2];
                for (int iVar = 0; iVar<numVar; ++iVar){
                    gradVar[iVar][0] += varAvSubFace[iVar] * subFaceDownN0;
                    gradVar[iVar][1] += varAvSubFace[iVar] * subFaceDownN1;
                    gradVar[iVar][2] += varAvSubFace[iVar] * subFaceDownN2;
                }
            }
            // Gradient on a level with the internal face is obtained as follows
            invVolSubVol = 1. / (volSubVolUp + volSubVolDown + epsTolGeo_);
            for (int iVar = 0; iVar<numVar; ++iVar){
                gradIntFaceVar[iVar][0] = invVolSubVol  * gradVar[iVar][0] ;
                gradIntFaceVar[iVar][1] = invVolSubVol  * gradVar[iVar][1] ;
                gradIntFaceVar[iVar][2] = invVolSubVol  * gradVar[iVar][2] ;
            }

        }

}
}
