#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

#include <array>
#include <vector>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;


class TransferBuffers {

	using particle_list = std::vector<int>;
	using buffer_grid = allscale::api::user::data::Grid<particle_list,3>;

	using d1 = std::array<buffer_grid,3>;

	d1 buffers;

  public:

	using grid_size_t = typename buffer_grid::coordinate_type;
	using grid_pos_t = grid_size_t;

	TransferBuffers(const grid_size_t& size)
		: buffers({buffer_grid(size),buffer_grid(size),buffer_grid(size)}) {}

	particle_list& getBuffer() {
		return buffers[5][2];
	}
};

using Field = Grid<int,3>;

struct Universe {
	Field propA;
	Field propB;
};


Universe createUniverse(int N) {
	Field a ({N,N,N});
	Field b ({N,N,N});
	return { std::move(a), std::move(b) };
}

void expect_eq(int a, int b) {
	if (a != b) exit(1);
}

int main() {

	const int N = 100;

	Universe u = createUniverse(N);
	using Point = Grid<int,3>::coordinate_type;

	auto zero = Point(0);
	auto size = u.propA.size();

	TransferBuffers particleTransfers(size);

	// initialize grid - collapsed
	pfor(zero, size, [&](const Point& p) {
		u.propA[p] = 3;
		u.propB[p] = 4;
	});

	// check the initialization
	pfor(zero, size, [&](const Point& p) {
		expect_eq(3,u.propA[p]);
		expect_eq(4,u.propB[p]);
	});

	return 0;
}
