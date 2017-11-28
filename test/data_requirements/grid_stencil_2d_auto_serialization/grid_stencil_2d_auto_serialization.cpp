#include <iostream>

#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;
using namespace allscale::utils;

struct Data {

    // -- some values --

    int time;
    int value;      // < is actually ignored

    // -- we do not serialize this explicit, we let the compiler do the job --

};

void expect_eq(int a, int b) {
    if (a != b) {
        std::cerr << "Expected: " << a << "\n";
        std::cerr << "      Is: " << b << "\n";
        exit(1);
    }
}

int main() {

    const int N = 1000;
    const int T = 100;

    Grid<Data,2> dataA({N,N});
    Grid<Data,2> dataB({N,N});

    using Point = Grid<Data,2>::coordinate_type;

    // initialize first vector
    pfor(Point{0,0},Point{N,N},[&](Point p){
        dataA[p].time = 0;
    });

    // run a time loop
    for(int t=0; t<T; t++) {

        // handle buffer swapping
        Grid<Data,2>& A = (t % 2) ? dataA : dataB;
        Grid<Data,2>& B = (t % 2) ? dataB : dataA;

        // update state
        pfor(Point{1,1},Point{N-1,N-1},[t,&A,&B](Point p) {

            // check the last state
            expect_eq(t,A[p].time);
            if (p.x > 1)   expect_eq(t,A[Point{p.x-1,p.y}].time);
            if (p.x < N-2) expect_eq(t,A[Point{p.x+1,p.y}].time);
            if (p.y > 1)   expect_eq(t,A[Point{p.x,p.y-1}].time);
            if (p.y < N-2) expect_eq(t,A[Point{p.x,p.y+1}].time);

            // update time
            B[p].time = t + 1;

        });
    }

	return 0;
}
