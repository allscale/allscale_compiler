#include "../../api/code/include/allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	auto fib = prec(
				fun(
					[](int x)->bool { return x < 2; },
					[](int x)->int { return x; },
					[](int x, const auto& f) {
						//auto a = run(f(x-1));
						//auto b = run(f(x-2));
						//return done(a.get() + b.get());
						return done(1);
					}
				)
		);

	fib(10).get();

	return 0;
}
