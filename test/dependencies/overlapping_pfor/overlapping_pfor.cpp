
#include "allscale/utils/assert.h"

#include "allscale/api/core/prec.h"
#include "allscale/api/user/algorithm/pfor.h"

#include <vector>
#include <unistd.h>

using namespace allscale::api::core;
using namespace allscale::api::user::algorithm;

int main() {

	const int N = 10;

	std::vector<int> v(N);

	auto a = pfor(0, N, [&v](int i) {
		v[i] = 0;
	});

	auto b = pfor(0, N, [&v](int i) {
		assert(v[i] == 0);
		if(i < N/2) sleep(1);
		v[i] = 1;
	}, small_neighborhood_sync(a));

	pfor(0, N, [&v](int i) {
		assert(v[i] == 1);
		v[i] = 2;
	}, small_neighborhood_sync(b));

	for(int i : v) {
		assert(i == 2);
	}
}
