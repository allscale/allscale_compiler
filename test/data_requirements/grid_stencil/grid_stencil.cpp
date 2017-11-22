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

    const int N = 1000;
    const int T = 100;

    Grid<int> dataA(N); 
    Grid<int> dataB(N); 

    // initialize first vector
    pfor(0,N,[&](int i){
        dataA[i] = 0;
    });

    // run a time loop
    for(int t=0; t<T; t++) {

        // handle buffer swapping
        Grid<int>& A = (t % 2) ? dataA : dataB;
        Grid<int>& B = (t % 2) ? dataB : dataA;

        // update state
        pfor(1,N-1,[t,&A,&B](int i) {
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
