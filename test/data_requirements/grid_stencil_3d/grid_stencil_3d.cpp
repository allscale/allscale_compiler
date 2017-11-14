#include <iostream>

#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

void expect_eq(int a, int b) {
    if (a != b) {
        std::cout << "Expected: " << a << "\n";
        std::cout << "      Is: " << b << "\n";
        exit(1);
    }
}

int main() {

    const int N = 100;
    const int T = 100;

    Grid<int,3> dataA({N,N,N}); 
    Grid<int,3> dataB({N,N,N}); 

    using Point = Grid<int,3>::coordinate_type;

    // initialize first vector
    pfor(Point{0,0,0},Point{N,N,N},[&](Point p){
        dataA[p] = 0;
    });

    // run a time loop
    for(int t=0; t<T; t++) {

        // handle buffer swapping
        Grid<int,3>& A = (t % 2) ? dataA : dataB;
        Grid<int,3>& B = (t % 2) ? dataB : dataA;

        // update state
        pfor(Point{1,1,1},Point{N-1,N-1,N-1},[t,&A,&B](Point p) {

            // check the last state
            expect_eq(t,A[p]);
            if (p.x > 1)   expect_eq(t,A[Point{p.x-1,p.y,p.z}]);
            if (p.x < N-2) expect_eq(t,A[Point{p.x+1,p.y,p.z}]);
            if (p.y > 1)   expect_eq(t,A[Point{p.x,p.y-1,p.z}]);
            if (p.y < N-2) expect_eq(t,A[Point{p.x,p.y+1,p.z}]);
            if (p.z > 1)   expect_eq(t,A[Point{p.x,p.y,p.z-1}]);
            if (p.z < N-2) expect_eq(t,A[Point{p.x,p.y,p.z+1}]);

            // update time
            B[p] = t + 1;

        });
    }

	return 0;
}
