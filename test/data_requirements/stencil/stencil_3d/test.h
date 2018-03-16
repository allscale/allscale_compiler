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

    const int N = 50;
    const int T = 100;

    using grid_t = Grid<int,3>;
    using point_t = GridPoint<3>;

    point_t size({N,N,N});

    // initialize the data buffer
	grid_t data(size);
    pfor(size,[&](point_t i) {
        data[i] = 0;
    });

	// run the stencil
	stencil<Impl>(data, T, 
        [=](time_t t, const point_t& pos, const grid_t& data){

            bool l0 = pos[0] >= 1;
            bool m0 = true;
            bool h0 = pos[0] <= N-2;

            bool l1 = pos[1] >= 1;
            bool m1 = true;
            bool h1 = pos[1] <= N-2;

            bool l2 = pos[2] >= 1;
            bool m2 = true;
            bool h2 = pos[2] <= N-2;

            // check the last state
            if (l0 && l1 && l2) expect_eq(t,data[pos+point_t(-1,-1,-1)]);
            if (l0 && l1 && m2) expect_eq(t,data[pos+point_t(-1,-1, 0)]);
            if (l0 && l1 && h2) expect_eq(t,data[pos+point_t(-1,-1,+1)]);

            if (l0 && m1 && l2) expect_eq(t,data[pos+point_t(-1, 0,-1)]);
            if (l0 && m1 && m2) expect_eq(t,data[pos+point_t(-1, 0, 0)]);
            if (l0 && m1 && h2) expect_eq(t,data[pos+point_t(-1, 0,+1)]);

            if (l0 && h1 && l2) expect_eq(t,data[pos+point_t(-1,+1,-1)]);
            if (l0 && h1 && m2) expect_eq(t,data[pos+point_t(-1,+1, 0)]);
            if (l0 && h1 && h2) expect_eq(t,data[pos+point_t(-1,+1,+1)]);


            if (m0 && l1 && l2) expect_eq(t,data[pos+point_t( 0,-1,-1)]);
            if (m0 && l1 && m2) expect_eq(t,data[pos+point_t( 0,-1, 0)]);
            if (m0 && l1 && h2) expect_eq(t,data[pos+point_t( 0,-1,+1)]);

            if (m0 && m1 && l2) expect_eq(t,data[pos+point_t( 0, 0,-1)]);
            if (m0 && m1 && m2) expect_eq(t,data[pos+point_t( 0, 0, 0)]);
            if (m0 && m1 && h2) expect_eq(t,data[pos+point_t( 0, 0,+1)]);

            if (m0 && h1 && l2) expect_eq(t,data[pos+point_t( 0,+1,-1)]);
            if (m0 && h1 && m2) expect_eq(t,data[pos+point_t( 0,+1, 0)]);
            if (m0 && h1 && h2) expect_eq(t,data[pos+point_t( 0,+1,+1)]);


            if (h0 && l1 && l2) expect_eq(t,data[pos+point_t(+1,-1,-1)]);
            if (h0 && l1 && m2) expect_eq(t,data[pos+point_t(+1,-1, 0)]);
            if (h0 && l1 && h2) expect_eq(t,data[pos+point_t(+1,-1,+1)]);

            if (h0 && m1 && l2) expect_eq(t,data[pos+point_t(+1, 0,-1)]);
            if (h0 && m1 && m2) expect_eq(t,data[pos+point_t(+1, 0, 0)]);
            if (h0 && m1 && h2) expect_eq(t,data[pos+point_t(+1, 0,+1)]);

            if (h0 && h1 && l2) expect_eq(t,data[pos+point_t(+1,+1,-1)]);
            if (h0 && h1 && m2) expect_eq(t,data[pos+point_t(+1,+1, 0)]);
            if (h0 && h1 && h2) expect_eq(t,data[pos+point_t(+1,+1,+1)]);


		    // increase the time step of current cell
		    return data[pos] + 1;
    	}
    );

	// check final state
    pfor(size,[&,T](point_t i) {
        expect_eq(data[i],T);
    });

	return 0;
}
