
#include "allscale/api/user/algorithm/pfor.h"
#include "allscale/api/core/prec.h"

#include <vector>

using namespace allscale::api;

int main() {
	; // this is required because of the clang compound source location bug

	// Note that we don't perform any IR comparison here, as it would be rather pointless and the given IR might change a lot with small changes in the API.
	// Nevertheless, the testing framework will run the semantic checks on the generated IR.

	{
		std::vector<int> v;
		user::algorithm::pfor(v, [](const auto& elem) {
			elem;
		});
	}

	return 0;
}
