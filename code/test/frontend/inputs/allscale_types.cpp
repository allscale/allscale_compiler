

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// CALLABLES //////

	// test type: (input) int -> (output) double

	// fun def
	using TestFunDefType = fun_def<double, int,
		std::function<bool(int)>,                   // base test
		std::tuple<std::function<double(int)>>,     // base case
		std::tuple<std::function<double(int)>>      // step case -- incorrect type, but irrelevant
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

	///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// COMPLETED TASKS //////

	#pragma test expect_ir(R"({
		var ref<ptr<treeture<uint<4>,f>>,f,f,plain> v0;
	})")
	{
		allscale::api::core::detail::completed_task<unsigned>* myCompTask;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TREETURES //////

	#pragma test expect_ir(R"({
		var ref<ptr<treeture<int<4>,t>>,f,f,plain> v0;
		var ref<ptr<treeture<real<4>,f>>,f,f,plain> v1;
		var ref<ptr<treeture<real<8>,f>>,f,f,plain> v2;
	})")
	{
		impl::reference::treeture<int>* t0;
		impl::reference::unreleased_treeture<float>* t1;
		impl::reference::lazy_unreleased_treeture<double, int /* placeholder */>* t2;
	}

	#pragma test expect_ir(R"({
		var ref<ptr<treeture<int<4>,t>>,f,f,plain> v0;
		var ref<ptr<treeture<real<4>,f>>,f,f,plain> v1;
		var ref<ptr<treeture<real<8>,f>>,f,f,plain> v2;
	})")
	{
		impl::sequential::treeture<int>* t0;
		impl::sequential::unreleased_treeture<float>* t1;
		impl::sequential::lazy_unreleased_treeture<double, int /* placeholder */>* t2;
	}

	///

	#pragma test expect_ir(R"({
		var ref<ptr<dependencies>,f,f,plain> v0;
	})")
	{
		dependencies* x;
	}

	return 0;
}
