

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// test type: (input) int -> (output) double

	using TestFunDefType = fun_def<double, int,
		std::function<bool(int)>,                   // base test
		std::tuple<std::function<double(int)>>,     // base case
		std::tuple<std::function<double(int)>>      // step case
	>;

	using TestRecDefsType = allscale::api::core::rec_defs<TestFunDefType>;

	using TestPrecOperationType = allscale::api::core::detail::prec_operation<0, int, double, TestRecDefsType>;

	// simple type tests
	#pragma test expect_ir(R"({ var ref<treeture<int<4>,f>,f,f,plain> a = treeture_done(1); })")
	{
		TestPrecOperationType* bla;
	}


	return 0;
}
