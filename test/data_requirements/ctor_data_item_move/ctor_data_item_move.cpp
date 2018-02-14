
#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

using Field = Grid<int,2>;

struct Universe {
	Field f;
	Universe(Field&& f) : f(std::move(f)) {}
};

int main() {

	Field::coordinate_type size{10,10};

	Field f(size);
	Universe u(std::move(f));

	// initialize the field
	pfor(size,[&](const auto& p) {
		u.f[p] = 10;
	});

	return EXIT_SUCCESS;
}


