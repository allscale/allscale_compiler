#include "allscale_analysis.h"

int fac(int x) {
	if (x <= 1) return 1;
	return fac(x-1) * x;
}

int fib(int x) {
	if (x < 2) return x;
	return fib(x-1) + fib(x-2);
}

int main() {

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");

		// if local data is accessed, everything is fine
		int x = 12;
		int y = 14;
		int z = x + y;
	}

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");

		// also some simple computation
		int x = 12;
		int y = fac(x);
	}

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");

		// also some simple computation
		int x = 12;
		int y = fib(x);
	}

	return 0;
}
