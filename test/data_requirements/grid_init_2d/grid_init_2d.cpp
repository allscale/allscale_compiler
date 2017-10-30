#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

void expect_eq(int a, int b) {
    if (a != b) exit(1);
}

int main() {

    const int N = 1000;

    Grid<int,2> data({N,N}); 
    using Point = Grid<int,2>::coordinate_type;

    // initialize grid - row-wise
    pfor(0,N,[&](int i){
        for(int j=0; j<N; j++) {
            data[{i,j}] = 1;
        }
    });

    // check the initialization
    pfor(Point{0,0},Point{N,N},[&](const Point& p) {
        expect_eq(1,data[p]);
    });

    // ---

    // initialize grid - column-wise
    pfor(0,N,[&](int i){
        for(int j=0; j<N; j++) {
            data[{j,i}] = 2;
        }
    });

    // check the initialization
    pfor(Point{0,0},Point{N,N},[&](const Point& p) {
        expect_eq(2,data[p]);
    });

    // ---

    // initialize grid - collapsed
    pfor(Point{0,0},Point{N,N},[&](const Point& p) {
        data[p] = 3;
    });

    // check the initialization
    pfor(Point{0,0},Point{N,N},[&](const Point& p) {
        expect_eq(3,data[p]);
    });

	return 0;
}
