#include <iostream>
#include <cmath>

#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

void expect_eq(int a, int b) {
    if (a != b) {
        std::cerr << "Expected: " << a << "\n";
        std::cerr << "      Is: " << b << "\n";
        exit(1);
    }
}

int main() {

    const int N = 100;
    const int T = 10;

    Grid<int,2> dataA({N,N});
    Grid<int,2> dataB({N,N});

    using Point = Grid<int,2>::coordinate_type;

    // initialize first vector
    auto ref = pfor(Point{0,0},Point{N,N},[&](Point p){
        dataA[p] = 0;
    });

    // run a time loop
    for(int t=0; t<T; t++) {

        // handle buffer swapping
        Grid<int,2>& A = (t % 2) ? dataA : dataB;
        Grid<int,2>& B = (t % 2) ? dataB : dataA;

        // update state
        ref = pfor(Point{1,1},Point{N-1,N-1},[t,N,&A,&B](Point p) {


            // check the last state
            expect_eq(t,A[p]);
            if (p.x > 1)   expect_eq(t,A[Point{p.x-1,p.y}]);
            if (p.x < N-2) expect_eq(t,A[Point{p.x+1,p.y}]);
            if (p.y > 1)   expect_eq(t,A[Point{p.x,p.y-1}]);
            if (p.y < N-2) expect_eq(t,A[Point{p.x,p.y+1}]);

            // update time
            B[p] = t + 1;

        }, small_neighborhood_sync(ref));
    }

    // wait for completions
    ref.wait();

    return 0;
}
