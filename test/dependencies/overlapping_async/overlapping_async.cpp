
#include "allscale/utils/assert.h"

#include "allscale/api/core/prec.h"
#include "allscale/api/user/operator/async.h"

#include <vector>
#include <unistd.h>

using namespace allscale::api::core;
using namespace allscale::api::user;

int main() {

	int i = -1;

	auto a = async([&]() {
		sleep(1);
		i = 0;
	});

	auto b = async(allscale::api::core::after(a), [&]() {
		assert(i == 0);
		i = 1;
	});

	auto c = async(allscale::api::core::after(b), [&]() {
		assert(i == 1);
		i = 2;
	});

	c.wait();

	assert(i == 2);
}
