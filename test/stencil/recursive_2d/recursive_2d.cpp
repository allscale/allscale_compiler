#include "allscale/api/user/data/static_grid.h"
#include "allscale/api/user/algorithm/stencil.h"

using namespace allscale::api::user;
using namespace allscale::api::user::algorithm;

int main() {

    const int N = 200;
    const int T = 100;
    using Grid = data::StaticGrid<int,N,N>;
    using Point = allscale::utils::Vector<int,2>;

    Grid data;
    const double k = 0.001;

    // initialize buffer
	pfor(Point{N,N},[&](const Point& p){
		data[p] = 0;
	});

    // compute simulation steps
    stencil<implementation::parallel_recursive>(data,T,
      // inner elements
      [k,T,N](time_t t, const Point& p, const Grid& data)->double {

        // check that neighbor-points are on the correct time stemp
        assert((t == data[p+Point{ -1,  0 }]));
        assert((t == data[p+Point{  1,  0 }]));
        assert((t == data[p+Point{  0, -1 }]));
        assert((t == data[p+Point{  0,  1 }]));
        assert((t == data[p]));

        return data[p] + 1;
      },
      // boundaries
      [k,T,N](time_t t, const Point& p, const Grid& data)->double {

        // check state of neighbors
        if (p[0] >  0 ) assert((t == data[p+Point{ -1,  0 }]));
        if (p[0] < N-1) assert((t == data[p+Point{  1,  0 }]));
        if (p[1] >  0 ) assert((t == data[p+Point{  0, -1 }]));
        if (p[1] < N-1) assert((t == data[p+Point{  0,  1 }]));
        assert((t == data[p]));

        // update the boundary
        return data[p] + 1;
      }
    );

	return 0;
}
