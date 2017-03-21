
#include "allscale/api/user/operator/pfor.h"

#include <vector>

using namespace allscale::api::user;

int main() {
	; // this is required because of the clang compound source location bug

	// eager implementation of fib
	#pragma test expect_ir(R"(
		{
			;
		}
	)")
	{
		std::vector<int> v;
		pfor(v, [](const auto& elem) {
			elem;
		});
	}

	return 0;
}
