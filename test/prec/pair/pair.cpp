#include <cstdlib>
#include <iostream>
#include <random>

#include "allscale/api/core/prec.h"

using namespace std;

int main() {
	auto f = allscale::api::core::prec(allscale::api::core::fun(
		[](const pair<int,int>& p) { return p.first < 1; },
		[](const pair<int,int>& p) { return 1; },
		[](const pair<int,int>& p, const auto& rec) {
			pair<int, int> p1(p.first - 1, p.second);
			pair<int, int> p2(p.first - 1, p.second);
			auto left = allscale::api::core::run(rec(p1));
			auto right = allscale::api::core::run(rec(p2));
			return allscale::api::core::done(left.get() + right.get());
		}
	));
	pair<int, int> p(8,  0);
	auto result = f(p).get();

	cout << result << endl;

    return 0;
}
