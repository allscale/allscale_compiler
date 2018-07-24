
#include "allscale/utils/assert.h"

#include "allscale/api/core/prec.h"
#include "allscale/api/user/algorithm/pfor.h"
#include "allscale/api/user/data/grid.h"

#include <vector>
#include <unistd.h>

using namespace allscale::api::core;
using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

int main() {

	const int N = 10;

	Grid<int, 1> v(N);

	auto a = pfor(0, N, [&v](int i) {
		v[i] = 0;
	});

	auto b = pfor(0, N, [&v](int i) {
		assert(v[i] == 0);
		if(i < N/2) sleep(1);
		v[i] = 1;
	}, one_on_one(a));

	pfor(0, N, [&v](int i) {
		if(i>0) assert(v[i-1] == 1);
		assert(v[i] == 1);
		if(i<N-1) assert(v[i+1] == 1);
	}, small_neighborhood_sync(b));

}
