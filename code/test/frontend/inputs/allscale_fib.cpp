
#include "allscale/api/core/prec.h"
#include "allscale/api/user/arithmetic.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// eager implementation of fib
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
		decl __any_string__outer : (ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		decl __any_string__middle : (ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v35 : ref<int<4>,f,f,plain>) -> bool {
				return *v35<2;
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v81 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v81;
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v171 : ref<int<4>,f,f,plain>, v195 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				var ref<treeture<int<4>,t>,f,f,plain> v173 = treeture_run(
					recfun_to_fun(
						tuple_member_access(*v195, 0ul, type_lit(recfun<int<4>,int<4>>))
					)(*v171-1)
				);
				var ref<treeture<int<4>,t>,f,f,plain> v178 = treeture_run(
					recfun_to_fun(
						tuple_member_access(*v195, 0ul, type_lit(recfun<int<4>,int<4>>))
					)(*v171-2)
				);
				return treeture_done(treeture_get(*v173)+treeture_get(*v178));
			}
		};
		def __any_string__outer = function (v95 : ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return __any_string__middle((*v95)) materialize ;
		};
		def __any_string__middle = function (v94 : ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return prec(*v94);
		};
		{
			var ref<precfun<int<4>,int<4>>,f,f,plain> fibEager = __any_string__outer(
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
									<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
									type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
							)]
					)
			) materialize;
			var ref<int<4>,f,f,plain> i = treeture_get(treeture_run(precfun_to_fun(*fibEager)(12)));
		}
	)")
	{
		auto fibEager = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					auto a = run(f(x-1));
					auto b = run(f(x-2));
					return done(a.get() + b.get());
				}
			)
		);

		auto res = fibEager(12).get();
	}

	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl struct __any_string__add_lambda_operator;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__operator_call_:const __any_string__add_lambda_operator::(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>;
		decl __any_string__user_add : (ref<treeture<int<4>,f>,f,f,cpp_rref>, ref<treeture<int<4>,f>,f,f,cpp_rref>) -> treeture<int<4>,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_const_space_int_space__ampersand__comma__space_const_space_int_space__ampersand__rparen_:const __any_string__add_lambda_operator::() -> ptr<(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>,t,f>;
		decl __any_string__outer : (ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		decl __any_string__middle : (ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> bool {
				return *v1<2;
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v1;
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>, v2 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				return __any_string__user_add(recfun_to_fun(tuple_member_access(*v2, 0ul, type_lit(recfun<int<4>,int<4>>)))(*v1-1), recfun_to_fun(tuple_member_access(*v2, 0ul, type_lit(recfun<int<4>,int<4>>)))(*v1-2));
			}
		};
		def __any_string__outer = function (v95 : ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return __any_string__middle((*v95)) materialize ;
		};
		def __any_string__middle = function (v94 : ref<(recfun<int<4>,int<4>>),t,f,cpp_ref>) -> precfun<int<4>,int<4>> {
			return prec(*v94);
		};
		def struct __any_string__add_lambda_operator {
			const function IMP__operator_call_ = (v1 : ref<int<4>,t,f,cpp_ref>, v2 : ref<int<4>,t,f,cpp_ref>) -> int<4> {
				return *v1+*v2;
			}
		};
		def __any_string__user_add = function (v1 : ref<treeture<int<4>,f>,f,f,cpp_rref>, v2 : ref<treeture<int<4>,f>,f,f,cpp_rref>) -> treeture<int<4>,f> {
			return treeture_combine(
					dependency_after(),
					*v1,
					*v2,
					cpp_lambda_to_lambda(
							*<ref<__any_string__add_lambda_operator,f,f,plain>>(ref_temp(
									type_lit(__any_string__add_lambda_operator)
							)) {},
							type_lit((int<4>, int<4>) -> int<4>)
					),
					true
			);
		};
		{
			var ref<precfun<int<4>,int<4>>,f,f,plain> v0 = __any_string__outer(
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
									<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
									type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
							)]
					)
			) materialize;
			var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(precfun_to_fun(*v0)(4));
		}
	)")
	{
		auto fib = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					return allscale::api::user::add(f(x - 1), f(x - 2));
				}
			)
		);

		auto res = fib(4);
	}

	return 0;
}
