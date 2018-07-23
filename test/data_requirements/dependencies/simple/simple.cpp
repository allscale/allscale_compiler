
#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {

	prec(fun(
			[](int x)->bool { return x < 2; },
			[](int x)->int { return x; },
			[](int x, const auto& f) {
				auto dep1 = after();
				return f(std::move(dep1), x - 1);
			}
	))(13).get();

	auto dep2 = after();
	prec(
			[](int x)->bool { return x < 2; },
			[](int x)->int { return x; },
			[](int x, const auto& f) {
				auto dep1 = after();
				return f(std::move(dep1), x - 1);
			}
	)(std::move(dep2), 13).get();

	return 0;
}
