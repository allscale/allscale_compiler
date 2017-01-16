

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// test type: (input) int -> (output) double

	// fun def
	using TestFunDefType = fun_def<double, int,
		std::function<bool(int)>,                   // base test
		std::tuple<std::function<double(int)>>,     // base case
		std::tuple<std::function<double(int)>>      // step case
	>;

	#pragma test expect_ir(R"({
		var ref<ptr<recfun<int<4>,real<8>>>,f,f,plain> v0;
	})")
	{
		TestFunDefType* myFunDef;
	}

	// rec defs
	using TestRecDefsType = allscale::api::core::rec_defs<TestFunDefType>;

	#pragma test expect_ir(R"({
		var ref<ptr<(recfun<int<4>,real<8>>)>,f,f,plain> v0;
	})")
	{
		TestRecDefsType* myRecDefs;
	}

	// prec operation
	using TestPrecOperationType = allscale::api::core::detail::prec_operation<0, int, double, TestRecDefsType>;

	#pragma test expect_ir(R"({
		var ref<ptr<recfun<int<4>,real<8>>>,f,f,plain> v0;
	})")
	{
		TestPrecOperationType* myPrecOp;
	}

	// callable
	using TestCallableType = allscale::api::core::detail::callable<0, TestRecDefsType>;

	#pragma test expect_ir(R"({
		var ref<ptr<recfun<int<4>,real<8>>>,f,f,plain> v0;
	})")
	{
		TestCallableType* myCallable;
	}

	return 0;
}
