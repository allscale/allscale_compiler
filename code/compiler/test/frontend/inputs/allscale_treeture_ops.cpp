
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

#include "allscale/api/user/arithmetic.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>, f>,f,f,plain> a = treeture_done(1);
		var ref<treeture<unit, f>,f,f,plain> b = treeture_done(unit);
		var ref<int<4>,f,f,plain> i = ref_decl(type_lit(ref<int<4>,f,f,plain>));
		var ref<treeture<int<4>,f>,f,f,plain> c = treeture_done(*i);
		var ref<int<4>,f,f,cpp_ref> iRef = ref_kind_cast(i, type_lit(cpp_ref));
		var ref<treeture<int<4>,f>,f,f,plain> d = treeture_done(*iRef);
		var ref<treeture<unit,t>,f,f,plain> t = treeture_run(treeture_done(unit));
	})")
	{
		auto a = done(1);
		auto b = done();
		int i;
		auto c = done(i);
		int& iRef = i;
		auto d = done(iRef);
		impl::reference::treeture<void> t;
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// METHODS ////

	// methods on treetures

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>,t>,f,f,plain> a =  treeture_run(treeture_done(1));
		treeture_wait(*a);
		treeture_get(*a);
		treeture_left(*a);
		treeture_right(*a);
		treeture_is_done(*a);
		treeture_is_valid(*a);
	})")
	{
		impl::reference::treeture<int> a = done(1);
		a.wait();
		a.get();
		a.getLeft();
		a.getRight();
		a.isDone();
		a.isValid();
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// COMBINATIONS ////

	// treeture combination operations

	#pragma test expect_ir(R"(
		decl struct __any_string__combine_1;
		decl struct __any_string__combine_2;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_comma__space_int_rparen_:const __any_string__combine_1::() -> ptr<(int<4>, int<4>) -> int<4>,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_comma__space_int_rparen_:const __any_string__combine_2::() -> ptr<(int<4>, int<4>) -> int<4>,t,f>;
		def struct __any_string__combine_1 {
			const function IMP__operator_call_ = (v673 : ref<int<4>,f,f,plain>, v674 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v673+*v674;
			}
		};
		def struct __any_string__combine_2 {
			const function IMP__operator_call_ = (v673 : ref<int<4>,f,f,plain>, v674 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v673+*v674;
			}
		};
		{
			var ref<treeture<int<4>,f>,f,f,plain> a = treeture_done(1);
			var ref<treeture<int<4>,f>,f,f,plain> b = treeture_done(2);
			treeture_sequential(dependency_after(), *a, *b);
			treeture_parallel(dependency_after(), *a, *b);
			treeture_combine(
				dependency_after(),
				*a,
				*b,
				cpp_lambda_to_lambda(
					*<ref<__any_string__combine_1,f,f,plain>>(ref_temp(type_lit(__any_string__combine_1))) {},
					type_lit((int<4>, int<4>) -> int<4>)
				),
				true
			);

			treeture_sequential(dependency_after(), *a, *b);
			treeture_parallel(dependency_after(), *a, *b);
			treeture_combine(
				dependency_after(),
				*a,
				*b,
				cpp_lambda_to_lambda(
					*<ref<__any_string__combine_2,f,f,plain>>(ref_temp(type_lit(__any_string__combine_2))) {},
					type_lit((int<4>, int<4>) -> int<4>)
				),
				true
			);
		}
	)")
	{ // this code is not actually correct, but it is sufficient for testing
		auto a = done(1);
		auto b = done(2);
		sequential(std::move(a), std::move(b));
		parallel(std::move(a), std::move(b));
		combine(std::move(a), std::move(b), [](int m, int n) { return m + n; });

		impl::reference::sequential(std::move((impl::reference::unreleased_treeture<int>) a), std::move((impl::reference::unreleased_treeture<int>) b));
		impl::reference::parallel(std::move((impl::reference::unreleased_treeture<int>) a), std::move((impl::reference::unreleased_treeture<int>) b));
		impl::reference::combine(std::move((impl::reference::unreleased_treeture<int>) a), std::move((impl::reference::unreleased_treeture<int>) b),
		                         [](int m, int n) { return m + n; });
	}

	// user combination operations

	#pragma test expect_ir(R"(
		decl struct __any_string__adder;
		decl main : () -> int<4>;
		decl __any_string__user_api_add : (ref<treeture<int<4>,f>,f,f,cpp_rref>, ref<treeture<int<4>,f>,f,f,cpp_rref>) -> treeture<int<4>,f>;
		decl IMP__operator_call_:const __any_string__adder::(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_const_space_int_space__ampersand__comma__space_const_space_int_space__ampersand__rparen_:const __any_string__adder::() -> ptr<(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>,t,f>;
		def struct __any_string__adder {

			const function IMP__operator_call_ = (v652 : ref<int<4>,t,f,cpp_ref>, v653 : ref<int<4>,t,f,cpp_ref>) -> int<4> {
				return *v652+*v653;
			}
		};
		def __any_string__user_api_add = function (v628 : ref<treeture<int<4>,f>,f,f,cpp_rref>, v629 : ref<treeture<int<4>,f>,f,f,cpp_rref>) -> treeture<int<4>,f> {
			return treeture_combine(
				dependency_after(),
				*v628,
				*v629,
				cpp_lambda_to_lambda(
					*<ref<__any_string__adder,f,f,plain>>(ref_temp(
						type_lit(__any_string__adder)
					)) {},
					type_lit((ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>)
				),
				true
			);
		};
		{
			var ref<treeture<int<4>,f>,f,f,plain> v665 = treeture_done(1);
			var ref<treeture<int<4>,f>,f,f,plain> v666 = treeture_done(2);
			__any_string__user_api_add(
				instantiate_fun(
					lit("target_type" : (ref<treeture<int<4>,f>,f,f,cpp_ref>) -> ref<treeture<int<4>,f>,f,f,cpp_rref>),
					lit("IMP_std_colon__colon_move" : (ref<'T_0_0,f,f,cpp_rref>) -> ref<'IMP_typename_space_std_colon__colon_remove_reference_lt__Tp_gt__colon__colon_type,f,f,cpp_rref>)
				)(ref_kind_cast(v665, type_lit(cpp_ref))),
				instantiate_fun(
					lit("target_type" : (ref<treeture<int<4>,f>,f,f,cpp_ref>) -> ref<treeture<int<4>,f>,f,f,cpp_rref>),
					lit("IMP_std_colon__colon_move" : (ref<'T_0_0,f,f,cpp_rref>) -> ref<'IMP_typename_space_std_colon__colon_remove_reference_lt__Tp_gt__colon__colon_type,f,f,cpp_rref>)
				)(ref_kind_cast(v666, type_lit(cpp_ref)))
			);
		}
	)")
	{
		auto a = done(1);
		auto b = done(2);
		allscale::api::user::add(std::move(a), std::move(b));
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// COMBINATIONS + DEPENDENCIES ////

	#pragma test expect_ir(R"(
		decl struct __any_string__combine;
		decl struct __any_string__combine_variable;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_comma__space_int_rparen_:const __any_string__combine::() -> ptr<(int<4>, int<4>) -> int<4>,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_comma__space_int_rparen_:const __any_string__combine_variable::() -> ptr<(int<4>, int<4>) -> int<4>,t,f>;
		def struct __any_string__combine {
			const function IMP__operator_call_ = (v673 : ref<int<4>,f,f,plain>, v674 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v673+*v674;
			}
		};
		def struct __any_string__combine_variable {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>, v2 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v1-*v2;
			}
		};
		{
			var ref<treeture<int<4>,f>,f,f,plain> v0 = treeture_done(1);
			var ref<treeture<int<4>,f>,f,f,plain> v1 = treeture_done(2);
			var ref<dependencies,f,f,plain> v2 = dependency_after();
			treeture_sequential(*v2, *v0, *v1);
			treeture_parallel(*v2, *v0, *v1);
			treeture_combine(*v2, *v0, *v1, cpp_lambda_to_lambda(*<ref<__any_string__combine,f,f,plain>>(ref_temp(type_lit(__any_string__combine))) {}, type_lit((int<4>, int<4>) -> int<4>)), true);
			var ref<__any_string__combine_variable,f,f,plain> v3 = ref_cast(<ref<__any_string__combine_variable,f,f,plain>>(ref_temp(type_lit(__any_string__combine_variable))) {}, type_lit(f), type_lit(f), type_lit(cpp_rref));
			treeture_combine(*v2, *v0, *v1, cpp_lambda_to_lambda(*v3, type_lit((int<4>, int<4>) -> int<4>)), true);
		}
	)")
	{ // this code is not actually correct, but it is sufficient for testing
		auto a = done(1);
		auto b = done(2);
		auto dep = after();
		sequential(dep, std::move(a), std::move(b));
		parallel(dep, std::move(a), std::move(b));
		combine(dep, std::move(a), std::move(b), [](int m, int n) { return m + n; });

		// check that we also support passing the combine operation lambda from somewhere else instead of specifying it directly in the call
		auto combineOp = [](int m, int n) { return m - n; };
		combine(dep, std::move(a), std::move(b), combineOp);
	}


	return 0;
}
