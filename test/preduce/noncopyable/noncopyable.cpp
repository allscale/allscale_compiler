#include <allscale/api/user/algorithm/preduce.h>

using namespace allscale::api::user::algorithm;

void expect_eq(int a, int b) {
    if (a == b) return;
    std::cout << "Expected " << a << ", is " << b << "\n";
    exit(1);
}

// result type without copy constructor (just move)
struct Result {

    int x;

    Result(int x = 0) : x(x) {}

    Result(const Result&) = delete;
    Result(Result&&) = default;

    Result& operator=(const Result&) = delete;
    Result& operator=(Result&&) = default;

};


int main() {

    const int N = 1000;

    // compute the sum of all values from 0 to N
    auto sum = preduce(
        0,N,                                            // < iterator range
        [&](int p, Result& val) { val.x += p; },        // < fold expression
        [](Result&& a, const Result& b) { a.x += b.x; return std::move(a); },             // < reduction (aggregation)
        []() { return Result(0); }                              // < initialization (neutral element)
    ).get();

    // check the result
    expect_eq((N*(N-1))/2,sum.x);

    // done
	return 0;
}
