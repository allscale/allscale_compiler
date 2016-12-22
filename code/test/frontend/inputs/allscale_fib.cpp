
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
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<ref<int<4>,t,f,cpp_ref>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
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
			const function IMP__operator_call_ = (v171 : ref<int<4>,f,f,plain>, v195 : ref<(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				var ref<treeture<int<4>,t>,f,f,plain> v173 = treeture_run(
					recfun_to_fun(
						tuple_member_access(*v195, 0ul, type_lit(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>))
					)(*v171-1)
				);
				var ref<treeture<int<4>,t>,f,f,plain> v178 = treeture_run(
					recfun_to_fun(
						tuple_member_access(*v195, 0ul, type_lit(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>))
					)(*v171-2)
				);
				return treeture_done(treeture_get(*v173)+treeture_get(*v178));
			}
		};
		{
			var ref<(ref<int<4>,t,f,cpp_ref>) => treeture<int<4>,f>,f,f,plain> fibEager = prec(
					(build_recfun(
							cpp_lambda_to_closure(
									<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
									type_lit((ref<int<4>,t,f,cpp_ref>) => bool)
							),
							[cpp_lambda_to_closure(
									<ref<__any_string__base,f,f,plain>>(ref_temp(type_lit(__any_string__base))) {},
									type_lit((ref<int<4>,t,f,cpp_ref>) => int<4>)
							)],
							[cpp_lambda_to_closure(
									<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
									type_lit((ref<int<4>,t,f,cpp_ref>, (recfun<ref<int<4>,t,f,cpp_ref>,int<4>>)) => treeture<int<4>,f>)
							)]
					))
			);
			var ref<int<4>,f,f,plain> i = treeture_get((*fibEager)(12));
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
		decl struct __any_string__add_lambda;
		decl struct __any_string__step;
		decl struct __any_string__base;
		decl struct __any_string__cutoff;
		decl main : () -> int<4>;
		decl __any_string__add_lambda : (ref<treeture<int<4>,f>,f,f,cpp_rref>, ref<treeture<int<4>,f>,f,f,cpp_rref>) -> treeture<int<4>,f>;
		decl IMP__operator_call_:const __any_string__add_lambda::(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<ref<int<4>,t,f,cpp_ref>,int<4>>)) -> treeture<int<4>,f>;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_const_space_int_space__ampersand__comma__space_const_space_int_space__ampersand__rparen_:const __any_string__add_lambda::() -> ptr<(ref<int<4>,t,f,cpp_ref>, ref<int<4>,t,f,cpp_ref>) -> int<4>,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		def struct __any_string__add_lambda {
			const function IMP__operator_call_ = (v715 : ref<int<4>,t,f,cpp_ref>, v716 : ref<int<4>,t,f,cpp_ref>) -> int<4> {
				return *v715+*v716;
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v871 : ref<int<4>,f,f,plain>, v890 : ref<(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				return __any_string__add_lambda (
						recfun_to_fun(
								tuple_member_access(*v890, 0ul, type_lit(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>))
						)(*v871-1),
						recfun_to_fun(
								tuple_member_access(*v890, 0ul, type_lit(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>))
						)(*v871-2)
				);
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v808 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v808;
			}
		};
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v762 : ref<int<4>,f,f,plain>) -> bool {
				return *v762<2;
			}
		};
		def __any_string__add_lambda = function (v683 : ref<treeture<int<4>,f>,f,f,cpp_rref>, v684 : ref<treeture<int<4>,f>,f,f,cpp_rref>) -> treeture<int<4>,f> {
			return treeture_combine(
					*type_instantiation(
							type_lit((ref<treeture<int<4>,f>,f,f,cpp_ref>) -> ref<treeture<int<4>,f>,f,f,cpp_rref>),
							lit("IMP_std_colon__colon_move" : (ref<'T_0_0,f,f,cpp_rref>) -> ref<'IMP_typename_space_std_colon__colon_remove_reference_lt__Tp_gt__colon__colon_type,f,f,cpp_rref>)
					)(ref_kind_cast(v683, type_lit(cpp_ref))),
					*type_instantiation(
							type_lit((ref<treeture<int<4>,f>,f,f,cpp_ref>) -> ref<treeture<int<4>,f>,f,f,cpp_rref>),
							lit("IMP_std_colon__colon_move" : (ref<'T_0_0,f,f,cpp_rref>) -> ref<'IMP_typename_space_std_colon__colon_remove_reference_lt__Tp_gt__colon__colon_type,f,f,cpp_rref>)
					)(ref_kind_cast(v684, type_lit(cpp_ref))),
					cpp_lambda_to_lambda(
							<ref<__any_string__add_lambda,f,f,plain>>(ref_temp(
									type_lit(__any_string__add_lambda)
							)) {},
							type_lit((int<4>, int<4>) -> int<4>)
					),
					true
			);
		};
		{
			var ref<(ref<int<4>,t,f,cpp_ref>) => treeture<int<4>,f>,f,f,plain> v891 = prec(
					(build_recfun(
							cpp_lambda_to_closure(
									<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
									type_lit((ref<int<4>,t,f,cpp_ref>) => bool)
							),
							[cpp_lambda_to_closure(
									<ref<__any_string__base,f,f,plain>>(ref_temp(type_lit(__any_string__base))) {},
									type_lit((ref<int<4>,t,f,cpp_ref>) => int<4>)
							)],
							[cpp_lambda_to_closure(
									<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
									type_lit((ref<int<4>,t,f,cpp_ref>, (recfun<ref<int<4>,t,f,cpp_ref>,int<4>>)) => treeture<int<4>,f>)
							)]
					))
			);
			var ref<treeture<int<4>,f>,f,f,plain> v897 = (*v891)(4);
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
