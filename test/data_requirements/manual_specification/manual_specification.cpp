#include <allscale/api/core/prec.h>
#include <allscale/api/core/data.h>

#include <allscale/api/user/data/static_grid.h>

int main(int argc, char** argv) {

	using namespace allscale;

	using Grid = api::user::data::StaticGrid<int, 10>;

	Grid grid;

	auto fill = api::core::prec(
		[](int x) { return x <= 0; },
		[](int) { },
		[&grid](int x, const auto& nested) {
			api::core::sema::needs_write_access(grid, Grid::region_type::single(x - 1));
			grid[x - 1] = 1;
			return nested(x - 1);
		}
	);

	fill(10).get();

	bool valid = true;
	for(int i = 0; i < grid.size()[0]; ++i) {
		if(grid[i] != 1) {
			valid = false;
			break;
		}
	}

	return valid ? EXIT_SUCCESS : EXIT_FAILURE;
}
