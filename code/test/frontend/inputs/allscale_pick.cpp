
#include "allscale/api/core/prec.h"

#include <vector>

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"(
		{
			var ref<list<int<4>>,f,f,plain> v0 = [1,2,3];
		}
	)")
	{
		auto res = pick(1, 2, 3);
	}

	// a pick in the base case
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base1;
		decl struct __any_string__base2;
		decl struct __any_string__step;
		decl main : () -> int<4>;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base1::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__base2::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base1::() -> ptr<(int<4>) -> int<4>,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base2::() -> ptr<(int<4>) -> int<4>,t,f>;
		decl __any_string__outer : (ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		decl __any_string__middle : (ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v126 : ref<int<4>,f,f,plain>) -> bool {
				return *v126<2;
			}
		};
		def struct __any_string__base1 {
			const function IMP__operator_call_ = (v173 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v173;
			}
		};
		def struct __any_string__base2 {
			const function IMP__operator_call_ = (v215 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v215+1;
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v265 : ref<int<4>,f,f,plain>, v277 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				recfun_to_fun(
						tuple_member_access(*v277, 0ul, type_lit(recfun<int<4>,int<4>>))
				)(*v265-1);
				return treeture_done(1);
			}
		};
		def __any_string__outer = function (v95 : ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return __any_string__middle((*v95)) materialize ;
		};
		def __any_string__middle = function (v94 : ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return prec(*v94);
		};
		{
			treeture_run(
					precfun_to_fun(
							__any_string__outer(
									build_recfun(
											cpp_lambda_to_closure(
													<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
													type_lit((int<4>) => bool)
											),
											[cpp_lambda_to_closure(
													<ref<__any_string__base1,f,f,plain>>(ref_temp(type_lit(__any_string__base1))) {},
													type_lit((int<4>) => int<4>)
											),cpp_lambda_to_closure(
													<ref<__any_string__base2,f,f,plain>>(ref_temp(type_lit(__any_string__base2))) {},
													type_lit((int<4>) => int<4>)
											)],
											[cpp_lambda_to_closure(
													<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
													type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
											)]
									)
							)
					)(10)
			);
		}
	)")
	{
		prec(fun(
				[](int x)->bool { return x < 2; },
				pick(
						[](int x)->int { return x; },
						[](int x)->int { return x + 1; }
				),
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		)(10);
	}

	// a pick in the step case
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step1;
		decl struct __any_string__step2;
		decl main : () -> int<4>;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step1::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__operator_call_:const __any_string__step2::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl __any_string__outer : (ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		decl __any_string__middle : (ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v126 : ref<int<4>,f,f,plain>) -> bool {
				return *v126<2;
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v173 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v173;
			}
		};
		def struct __any_string__step1 {
			const function IMP__operator_call_ = (v220 : ref<int<4>,f,f,plain>, v275 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				recfun_to_fun(
						tuple_member_access(*v275, 0ul, type_lit(recfun<int<4>,int<4>>))
				)(*v220-1);
				return treeture_done(1);
			}
		};
		def struct __any_string__step2 {
			const function IMP__operator_call_ = (v263 : ref<int<4>,f,f,plain>, v276 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				recfun_to_fun(
						tuple_member_access(*v276, 0ul, type_lit(recfun<int<4>,int<4>>))
				)(*v263-2);
				return treeture_done(2);
			}
		};
		def __any_string__outer = function (v95 : ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return __any_string__middle((*v95)) materialize ;
		};
		def __any_string__middle = function (v94 : ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return prec(*v94);
		};
		{
			treeture_run(
					precfun_to_fun(
							__any_string__outer(
									build_recfun(
											cpp_lambda_to_closure(
													<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
													type_lit((int<4>) => bool)
											),
											[cpp_lambda_to_closure(
													<ref<__any_string__base,f,f,plain>>(ref_temp(type_lit(__any_string__base))) {},
													type_lit((int<4>) => int<4>)
											)],
											[cpp_lambda_to_closure(
													<ref<__any_string__step1,f,f,plain>>(ref_temp(type_lit(__any_string__step1))) {},
													type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
											),cpp_lambda_to_closure(
													<ref<__any_string__step2,f,f,plain>>(ref_temp(type_lit(__any_string__step2))) {},
													type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
											)]
									)
							)
					)(11)
			);
		}
	)")
	{
		prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				pick(
						[](int x, const auto& f) {
							f(x - 1);
							return done(1);
						},
						[](int x, const auto& f) {
							f(x - 2);
							return done(2);
						}
				)
			)
		)(11);
	}

	return 0;
}
