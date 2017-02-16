
#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// fun construction

	// here we always need to call the result of the prec operation, otherwise the compiler won't generate the necessary operators
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> real<8>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,real<8>>)) -> treeture<real<8>,f>;
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
		decl IMP__conversion_operator_double_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> real<8>,t,f>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> bool {
				return *v1<=2;
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>) -> real<8> {
				return num_cast(*v1, type_lit(real<8>));
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v1 : ref<int<4>,f,f,plain>, v2 : ref<(recfun<int<4>,real<8>>),f,f,plain>) -> treeture<real<8>,f> {
				recfun_to_fun(tuple_member_access(*v2, 0ul, type_lit(recfun<int<4>,real<8>>)))(*v1-2);
				return recfun_to_fun(tuple_member_access(*v2, 0ul, type_lit(recfun<int<4>,real<8>>)))(*v1-1);
			}
		};
		{
			var ref<recfun<int<4>,real<8>>,f,f,plain> v628 = build_recfun(
					cpp_lambda_to_closure(
							<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
							type_lit((int<4>) => bool)
					),
					[cpp_lambda_to_closure(
							<ref<__any_string__base,f,f,plain>>(ref_temp(type_lit(__any_string__base))) {},
							type_lit((int<4>) => real<8>)
					)],
					[cpp_lambda_to_closure(
							<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
							type_lit((int<4>, (recfun<int<4>,real<8>>)) => treeture<real<8>,f>)
					)]
			);
			var ref<recfun<int<4>,real<8>>,f,f,plain> v770 = prec((*v628));
			recfun_to_fun(*v770)(1);
		}
	)")
	{
		auto funVar = fun([](int x) { return x<=2; },
											[](int x) { return (double) x; },
											[](int x, const auto& f) { f(x-2); return f(x-1); });

		auto precRes = prec(funVar);
		precRes(1);
	}

	return 0;
}
