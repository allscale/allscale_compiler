

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({ var ref<treeture<int<4>,f>,f,f,plain> v0 = treeture_done(1); })")
	{
		auto a = done(1);
	}

	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> treeture<int<4>,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
		def struct __any_string__step {
			const function IMP__operator_call_ = (v133 : ref<int<4>,f,f,plain>, v134 : ref<recfun<int<4>,int<4>>,t,f,cpp_ref>) -> treeture<int<4>,f> {
				return treeture_done(1);
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v81 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v81;
			}
		};
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v35 : ref<int<4>,f,f,plain>) -> bool {
				return *v35<2;
			}
		};
		{
			var ref<(int<4>) => treeture<int<4>,f>,f,f,plain> prec_result_var = prec(
					(build_recfun(
							lambda_to_closure(
									<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
									type_lit((int<4>) => bool)
							),
							[lambda_to_closure(
									<ref<__any_string__base,f,f,plain>>(ref_temp(type_lit(__any_string__base))) {},
									type_lit((int<4>) => int<4>)
							)],
							[lambda_to_closure(
									<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
									type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
							)]
					))
			);
			(*prec_result_var)(12) materialize ;
		}
	)")
	{
		auto fib = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					return done(1);
				}
			)
		);

		/*auto f = */
		fib(12);

		//fib(10).get();
	}

	return 0;
}
