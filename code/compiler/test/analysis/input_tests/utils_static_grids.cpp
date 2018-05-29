#include "allscale_analysis.h"

#include "allscale/utils/static_grid.h"

using namespace allscale::utils;


void check1D() {

	StaticGrid<int,10> grid;

	// in the beginning there was nothing
	cba_expect_undefined_int(grid[5]);

	// then the programmer said 5 should be 7
	grid[5] = 7;

	// in it was 7
	// TODO: fix this known issue
//	cba_expect_eq_int(7,grid[5]);
}

void check2D() {

	StaticGrid<int,10,12> grid;

	// in the beginning there was nothing
	cba_expect_undefined_int(grid[{5,3}]);

	// then the programmer said {5,3} should be 7
	grid[{5,3}] = 7;

	// in it was 7
	// TODO: fix this known issue
//	cba_expect_eq_int(7,grid[{5,3}]);
}

int main() {

	check1D();
	check2D();

}
