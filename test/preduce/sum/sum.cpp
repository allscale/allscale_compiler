#include <allscale/api/user/algorithm/preduce.h>

using namespace allscale::api::user::algorithm;

void expect_eq(int a, int b) {
    if (a == b) return;
    std::cout << "Expected " << a << ", is " << b << "\n";
    exit(1);
}

int main() {

    const int N = 1000;

    // compute the sum of all values from 0 to N
    auto sum = preduce(
        0,N,                                            // < iterator range
        [&](int p, int& val) { val += p; },             // < fold expression
        [](int a, int b) { return a + b; },             // < reduction (aggregation)
        []() { return 0; }                              // < initialization (neutral element)
    ).get();

    // check the result
    expect_eq((N*(N-1))/2,sum);

    // done
	return 0;
}
