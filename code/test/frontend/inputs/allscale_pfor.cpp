
#include "allscale/api/user/operator/pfor.h"
#include "allscale/api/core/prec.h"

#include <vector>

using namespace allscale::api;

int main() {
	; // this is required because of the clang compound source location bug

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
