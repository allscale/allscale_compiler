#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;
using namespace allscale::utils;

void expect_eq(int a, int b) {
    if (a != b) exit(1);
}

int main() {

    const int N = 100;

    Grid<int,3> data({N,N,N}); 
    using Point = Grid<int,3>::coordinate_type;

    // initialize grid - only parallel over one dimension
    pfor(0,N,[&](int i){
        for(int j=0; j<N; j++) {
            for(int k=0; k<N; k++) {
                data[{i,j,k}] = 1;
            }
        }
    });

    // check the initialization
    pfor(Point{0,0,0},Point{N,N,N},[&](const Point& p) {
        expect_eq(1,data[p]);
    });

    // ---

/* -- still buggy ... generated requiremetns are wrong

    // initialize grid - with two dimensions collapsed
    pfor(Vector<int,2>{0,0},Vector<int,2>{N,N},[&](const Vector<int,2>& p) {
        for(int j=0; j<N; j++) {
            data[{p.x,j,p.y}] = 2;
        }
    });

    // check the initialization
    pfor(Point{0,0,0},Point{N,N,N},[&](const Point& p) {
        expect_eq(2,data[p]);
    });
*/
    // ---

    // initialize grid - over all three dimensions
    pfor(Point{0,0,0},Point{N,N,N},[&](const Point& p) {
        data[p] = 3;
    });

    // check the initialization
    pfor(Point{0,0,0},Point{N,N,N},[&](const Point& p) {
        expect_eq(3,data[p]);
    });

	return 0;
}
