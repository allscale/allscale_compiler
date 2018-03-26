
#include <cstdlib>

#include <allscale/api/user/algorithm/async.h>
#include <allscale/api/user/data/mesh.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

// define a simple mesh:

class Cell {};
class Edge : public edge<Cell,Cell> {};

// define the builder
using BarMeshBuilder = MeshBuilder<
    nodes<Cell>,
    edges<Edge>,
    hierarchies<>,
    1
>;

// define the mesh
using BarMesh = BarMeshBuilder::mesh_type<5>;


// a factory function

BarMesh createBar(int length) {

    BarMeshBuilder builder;

    // create cells
    std::vector<NodeRef<Cell>> cells;
    for(int i=0; i<length; i++) {
        cells.push_back(builder.create<Cell>());
    }

    // link cells
    for(int i=1; i<length-1; i++) {
        // bi-directional
        builder.link<Edge>(cells[i],cells[i+1]);
        builder.link<Edge>(cells[i],cells[i-1]);
    }

    return builder.build<5>();
}

int main() {

    int N = 10;
    int T = 50;

    // get a mesh
    auto bar = createBar(N);

    // create a temperature property
    auto tempA = bar.createNodeData<Cell,double>();
    auto tempB = bar.createNodeData<Cell,double>();

    // a state-printing utility
    auto printState = [&](const std::string& title){
        async([&,title](){
            std::cout << title << ":\n";
            bar.forAll<Cell>([&](auto& cell) {
                std::cout << tempA[cell] << " ";
            });
            std::cout << "\n";
        }).get();
    };

    // set temperature to 0, 50 in the center
    bar.pforAll<Cell>([&,N](auto& cell) {
        tempA[cell] = ((int)cell.id) == N/2 ? 50 : 0;
    });

    // initial state
    printState("Initial State");

    // simulate a few time steps
    for(int t=0; t<T; t++) {
        bar.pforAll<Cell>([&](auto& cell) {

            // center temperature stays constant
            if (((int)cell.id) == N/2) {
                tempB[cell] = 50;
                return;
            }

            // update temperature
            int count = 0;
            double sum = 0;
            for(const auto& neighbor : bar.getSinks<Edge>(cell)) {
                sum += tempA[neighbor];
                count++;
            }

            // handle edges
            if (count == 0) {
                tempB[cell] = 0;
                return;
            }

            // handle all others
            double avg = sum / count;
            tempB[cell] = tempA[cell] + 0.2 * (avg - count*tempA[cell]);
        });

        // swap buffers
        std::swap(tempA,tempB);

    } 

    // end state
    printState("Final State");

    // done
    return EXIT_SUCCESS;
}
