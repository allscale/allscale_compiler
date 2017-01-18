
#include <iostream>

#include "allscale/api/core/prec.h"
#include "allscale/api/user/arithmetic.h"

using namespace allscale::api::core;

int factorial_static(int x) {
	return (x < 2) ? 1 : x * factorial_static(x - 1);
}

int factorialLazy(int x) {
	auto f = prec(fun(
		[](int x) { return x < 2; },
		[](int x) { return 1; },
		[](int x, const auto& rec) {
			return allscale::api::user::mul(done(x), rec(x - 1));
		}
	));
	return f(x).get();
}

int main() {
	const int N = 6;
	const int factorialN = factorial_static(N);
	const int factorialE = factorialLazy(N);

	std::cout << factorialE << std::endl;

	// run the evaluation
	bool success = factorialN == factorialE;

	// return success state
	return (success) ? 0 : 1;
}
