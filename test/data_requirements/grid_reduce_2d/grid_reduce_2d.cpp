#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/algorithm/preduce.h>
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

    // initialize grid - collapsed
    pfor(Point{0,0},Point{N,N},[&](const Point& p) {
        data[p] = p.x*p.y;
    });

    // compute the sum of all those fields
    auto sum = preduce(
        Point{0,0},Point{N,N},                                  // < iterator range
        [&](const Point& p, int& val) { val += data[p]; },      // < fold expression
        [](int a, int b) { return a + b; },                     // < reduction
        []() { return 0; }                                      // < initialization
    ).get();

    // check the result
    expect_eq(392146832,sum);

    // done
	return 0;
}
