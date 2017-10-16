#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

int main() {

    const int N = 1000;
    using Vec = Grid<float>;

    Vec a(N);
    Vec b(N);

    // initialize vectors a and b
    pfor(0,N,[&](int i) {
        a[i] = i;
        b[i] = N-i;
    });

    // sum up vectory
    Vec c(N);
    pfor(0,N,[&](int i) {
        c[i] = a[i] + b[i];
    });

    // check values
    pfor(0,N,[&](int i) {
        if (c[i] != N) exit(1);
    });

	return 0;
}
