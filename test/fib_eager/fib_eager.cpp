
#include <iostream>

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int fib_static(int x) {
	return (x < 2) ? x : fib_static(x - 1) + fib_static(x - 2);
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

//int fibEagerReferences(int x) {
//	auto f = prec(fun(
//		[](const int& x) { return x < 2; },
//		[](const int& x) { return x; },
//		[](const int& x, const auto& rec) {
//			auto a = run(rec(x-1));
//			auto b = run(rec(x-2));
//			return done(a.get() + b.get());
//		}
//	));
//	return f(x).get();
//}

int main() {
	const int N = 42;
	const int fibN = fib_static(N);
	const int fibE = fibEager(N);
//	const int fibEr = fibEagerReferences(N);

	std::cout << fibN << std::endl;
	std::cout << fibE << std::endl;
//	std::cout << fibEr << std::endl;

	// run the evaluation
	bool success = fibN == fibE/* == fibEr*/;
	std::cout << success << std::endl;

	// return success state
	return (success) ? 0 : 1;
}
