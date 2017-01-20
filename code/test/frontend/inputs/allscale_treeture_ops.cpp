
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

#include "allscale/api/user/arithmetic.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>, f>,f,f,plain> a = treeture_done(1);
	})")
	{
		auto a = done(1);
	}

	// methods on treetures

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>,t>,f,f,plain> a =  treeture_run(treeture_done(1));
		treeture_wait(*a);
		treeture_get(*a);
		treeture_left(*a);
		treeture_right(*a);
	})")
	{
		impl::reference::treeture<int> a = done(1);
		a.wait();
		a.get();
		a.getLeft();
		a.getRight();
	}

	// methods on unreleased treetures

	#pragma test expect_ir(R"({
		var ref<treeture<real<4>,f>,f,f,plain> a =  treeture_done(1.0E+0f);
		treeture_wait(*a);
		treeture_left(*a);
		treeture_right(*a);
	})")
	{
		impl::reference::unreleased_treeture<float> a = done(1.0f);
		a.wait();
		a.getLeft();
		a.getRight();
	}

	// treeture combination operations

	#pragma test expect_ir(R"(
		decl struct __any_string__combine;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_comma__space_int_rparen_:const __any_string__combine::() -> ptr<(int<4>, int<4>) -> int<4>,t,f>;
		def struct __any_string__combine {
			const function IMP__operator_call_ = (v673 : ref<int<4>,f,f,plain>, v674 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v673+*v674;
			}
		};
		{
			var ref<treeture<int<4>,f>,f,f,plain> a = treeture_done(1);
			var ref<treeture<int<4>,f>,f,f,plain> b = treeture_done(2);
			treeture_sequential(*a, *b);
			treeture_parallel(*a, *b);
			treeture_combine(
				*a,
				*b,
				cpp_lambda_to_lambda(
					<ref<__any_string__combine,f,f,plain>>(ref_temp(type_lit(__any_string__combine))) {},
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
				*v628,
				*v629,
				cpp_lambda_to_lambda(
					<ref<__any_string__adder,f,f,plain>>(ref_temp(
						type_lit(__any_string__adder)
					)) {},
					type_lit((int<4>, int<4>) -> int<4>)
				),
				true
			);
		};
		// Inspire Program
		{
			var ref<treeture<int<4>,f>,f,f,plain> v665 = treeture_done(1);
			var ref<treeture<int<4>,f>,f,f,plain> v666 = treeture_done(2);
			__any_string__user_api_add(
				type_instantiation(
					type_lit((ref<treeture<int<4>,f>,f,f,cpp_ref>) -> ref<treeture<int<4>,f>,f,f,cpp_rref>),
					lit("IMP_std_colon__colon_move" : (ref<'T_0_0,f,f,cpp_rref>) -> ref<'IMP_typename_space_std_colon__colon_remove_reference_lt__Tp_gt__colon__colon_type,f,f,cpp_rref>)
				)(ref_kind_cast(v665, type_lit(cpp_ref))),
				type_instantiation(
					type_lit((ref<treeture<int<4>,f>,f,f,cpp_ref>) -> ref<treeture<int<4>,f>,f,f,cpp_rref>),
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

	return 0;
}
