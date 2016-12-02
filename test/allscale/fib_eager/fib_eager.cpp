
#include "allscale/api/core/prec.h"

using namespace allscale::api::core;


int fib_static(int x) {
    return (x < 2) ? x : fib_static(x-1) + fib_static(x-2);
}

int fibEager(int x) {
	auto f = prec(fun(
		[](int x) { return x < 2; },
		[](int x) { return x; },
		[](int x, const auto& rec) {
			auto a = run(rec(x-1));
			auto b = run(rec(x-2));
			return done(a.get() + b.get());
		}
	));
    return f(x).get();
}

int main() {
    const int N = 42;
    const int fibN = fib_static(N);

    // run the evaluation
    bool success = fibN == fibEager(N);

    // return success state
	return (success) ? 0 : 1;
}
