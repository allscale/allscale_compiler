#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

using Field = Grid<int,2>;

struct Universe {
    Field propA;
    Field propB;
};

struct UniverseHolder {
	Universe u;
};


Universe createUniverse(int N) {
    Field a ({N,N});
    Field b ({N,N});
    return { std::move(a), std::move(b) };
}

void expect_eq(int a, int b) {
    if (a != b) exit(1);
}

int main() {

    const int N = 1000;

    UniverseHolder uh = { createUniverse(N) };
    using Point = Grid<int,2>::coordinate_type;

    // initialize grid - collapsed
    pfor(Point{0,0},Point{N,N},[&uh](const Point& p) {
        uh.u.propA[p] = 3;
        uh.u.propB[p] = 4;
    });

    // check the initialization
    pfor(Point{0,0},Point{N,N},[&uh](const Point& p) {
        expect_eq(3,uh.u.propA[p]);
        expect_eq(4,uh.u.propB[p]);
    });

	return 0;
}
