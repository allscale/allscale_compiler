#include <iostream>

#include <allscale/api/user/data/grid.h>
#include <allscale/api/user/algorithm/stencil.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

inline void expect_eq(int a, int b) {
    if (a != b) {
        std::cerr << "Expected: " << a << "\n";
        std::cerr << "      Is: " << b << "\n";
        exit(1);
    }
}

template<typename Impl>
int run_test() {

    const int N = 1000;
    const int T = 100;

    // initialize the data buffer
	Grid<int,1> data(N);
    pfor(0,N,[&](int i) {
        data[i] = 0;
    });

	// run the stencil
	stencil<Impl>(data, T, 
        [=](time_t t, const GridPoint<1>& pos, const Grid<int,1>& data){

            // check the last state
            if (pos[0] >= 1)   expect_eq(t,data[pos[0]-1]);
            expect_eq(t,data[pos]);
            if (pos[0] <= N-2) expect_eq(t,data[pos[0]+1]);

		    // increase the time step of current cell
		    return data[pos] + 1;
    	}
    );

	// check final state
    pfor(0,N,[&,T](int i) {
        expect_eq(data[i],T);
    });

	return 0;
}
