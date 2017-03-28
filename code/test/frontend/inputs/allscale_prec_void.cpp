

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;


#define PREC_IR R"(
	decl struct __any_string__cutoff;
	decl struct __any_string__base;
	decl struct __any_string__step;
	decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
	decl IMP__operator_call_:const __any_string__base::(int<4>) -> unit;
	decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,unit>)) -> treeture<unit,f>;
	decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
	decl IMP__conversion_operator_void_space__lparen__star__rparen__lparen_int_rparen_:const __any_string__base::() -> ptr<(int<4>) -> unit,t,f>;
	def struct __any_string__cutoff {
		const function IMP__operator_call_ = (v124 : ref<int<4>,f,f,plain>) -> bool {
			return *v124<2;
		}
	};
	def struct __any_string__base {
		const function IMP__operator_call_ = (v171 : ref<int<4>,f,f,plain>) -> unit { }
	};
	def struct __any_string__step {
		const function IMP__operator_call_ = (v221 : ref<int<4>,f,f,plain>, v233 : ref<(recfun<int<4>,unit>),f,f,plain>) -> treeture<unit,f> {
			if(*v221==1) {
				return treeture_done(unit);
			}
			return treeture_done(unit);
		}
	};
	{
		treeture_run(
				precfun_to_fun(
						prec(
								(build_recfun(
										cpp_lambda_to_closure(
												<ref<__any_string__cutoff,f,f,plain>>(ref_temp(type_lit(__any_string__cutoff))) {},
												type_lit((int<4>) => bool)
										),
										[cpp_lambda_to_closure(
												<ref<__any_string__base,f,f,plain>>(ref_temp(type_lit(__any_string__base))) {},
												type_lit((int<4>) => unit)
										)],
										[cpp_lambda_to_closure(
												<ref<__any_string__step,f,f,plain>>(ref_temp(type_lit(__any_string__step))) {},
												type_lit((int<4>, (recfun<int<4>,unit>)) => treeture<unit,f>)
										)]
								))
						)
				)(16)
		);
	})"

int main() {
	; // this is required because of the clang compound source location bug

	// prec with void return type. Both locations with a return in the step case should get fixed to return a treeture
	#pragma test expect_ir(PREC_IR)
	{
		prec(
			[](int x) { return x < 2; },
			[](int x) { },
			[](int x, const auto& f) {
				if(x == 1) {
					return;
				}
			}
		)(16);
	}

	return 0;
}
