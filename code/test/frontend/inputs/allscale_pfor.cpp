
#include "allscale/api/user/operator/pfor.h"
#include "allscale/api/core/prec.h"

#include <vector>

using namespace allscale::api;

int main() {
	; // this is required because of the clang compound source location bug

	// we need to support the translation of core::pick for user::pfor to work
	#pragma test expect_ir(R"(
		{
			var ref<list<int<4>>,f,f,plain> v0 = [1,2,3];
		}
	)")
	{
		auto res = core::pick(1, 2, 3);
	}

	// eager implementation of fib
	#pragma test expect_ir(R"(
		{
			;
		}
	)")
	{
		std::vector<int> v;
		user::pfor(v, [](const auto& elem) {
			elem;
		});
	}

	return 0;
}
