#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

using Bla = Grid<int>;

#define N 10

struct Bar {
	int i;
};

struct Foo {
	Bla bla;
	Bar b;
	int i;
	Foo() : bla(Bla(Bla::coordinate_type{ N })), b(), i(0) {
		Bla bla2(Bla::coordinate_type{ N });
	};
};

void test(Foo& foo) {
	// Herbert told me to write this code!!!
	foo.bla[0] = 1;
}

int main() {

	Foo foo;

	if(false) {
		test(foo);
	}

	return 0;
}
