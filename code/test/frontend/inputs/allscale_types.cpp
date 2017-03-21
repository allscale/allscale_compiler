

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
		var ref<ptr<precfun<int<4>,real<8>>>,f,f,plain> v0;
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

	// step case param argument
	using TestRecFunParamParallelCallableType = allscale::api::core::detail::callable<0, TestFunDefType>::ParallelCallable;
	using TestRecFunParamSequentialCallableType = allscale::api::core::detail::callable<0, TestFunDefType>::SequentialCallable;

	#pragma test expect_ir(R"({
		var ref<ptr<recfun<int<4>,real<8>>>,f,f,plain> v0;
		var ref<ptr<recfun<int<4>,real<8>>>,f,f,plain> v1;
	})")
	{
		TestRecFunParamParallelCallableType* myRecFunParallelParam;
		TestRecFunParamSequentialCallableType* myRecFunSequentialParam;
	}

	// fun def with references
	using TestFunDefTypeReferences = fun_def<double, const int&,
		std::function<bool(const int&)>,                   // base test
		std::tuple<std::function<double(const int&)>>,     // base case
		std::tuple<std::function<double(const int&)>>      // step case -- incorrect type, but irrelevant
	>;

	#pragma test expect_ir(R"({
		var ref<ptr<recfun<ref<int<4>,t,f,cpp_ref>,real<8>>>,f,f,plain> v0;
	})")
	{
		TestFunDefTypeReferences* myFunDef;
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

	// references are not ignored
	#pragma test expect_ir(R"({
		var ref<ptr<treeture<int<4>,t>>,f,f,plain> v0;
		var ref<ptr<treeture<ref<real<4>,t,f,cpp_ref>,f>>,f,f,plain> v1;
		var ref<ptr<treeture<ref<real<8>,t,f,cpp_ref>,f>>,f,f,plain> v2;
	})")
	{
		impl::sequential::treeture<const int>* t0;
		impl::sequential::unreleased_treeture<const float&>* t1;
		impl::sequential::lazy_unreleased_treeture<const double&, int /* placeholder */>* t2;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TASK REFS //////

	#pragma test expect_ir(R"({
		var ref<ptr<task_ref>,f,f,plain> v0;
		var ref<ptr<task_ref>,f,f,plain> v1;
	})")
	{
		impl::reference::task_reference* r0;
		impl::sequential::task_reference* r1;
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// DEPENDENCIES //////

	#pragma test expect_ir(R"({
		var ref<ptr<dependencies>,f,f,plain> v0;
		var ref<ptr<dependencies>,f,f,plain> v1;
		var ref<ptr<dependencies>,f,f,plain> v2;
		var ref<ptr<dependencies>,f,f,plain> v3;
	})")
	{
		no_dependencies* x0;
		impl::sequential::dependencies* x1;
		impl::reference::dependencies<impl::reference::fixed_sized<0>>* x2;
		impl::reference::dependencies<impl::reference::dynamic_sized>* x3;
	}

	return 0;
}
