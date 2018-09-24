#include <iostream>

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

    const int N = 1000;
    const int T = 100;

    Grid<int,1> dataA(N);
    Grid<int,1> dataB(N);

    // initialize first vector
    pfor(0,N,[&](int i){
        dataA[i] = 0;
    });

    // run a time loop
    for(int t=0; t<T; t++) {

        // handle buffer swapping
        Grid<int,1>& A = (t % 2) ? dataB : dataA;
        Grid<int,1>& B = (t % 2) ? dataA : dataB;

        // update state
        pfor(0,N,[t,&A,&B](int i) {
            // check the last state
            if (i > 1)   expect_eq(t,A[i-1]);
            expect_eq(t,A[i]);
            if (i < N-2) expect_eq(t,A[i+1]);

            // update time
            B[i] = t + 1;

        });
    }

	return 0;
}
