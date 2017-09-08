

#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

using namespace allscale::api::core;

#define PREC_PREAMBLE_IR R"(
	decl struct __any_string__cutoff;
	decl struct __any_string__base;
	decl struct __any_string__step;
	decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
	decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
	decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
	decl IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_int_rparen__space__minus__gt__space_bool:const __any_string__cutoff::() -> ptr<(int<4>) -> bool,t,f>;
	decl IMP__conversion_operator_auto_space__lparen__star__rparen__lparen_int_rparen__space__minus__gt__space_int:const __any_string__base::() -> ptr<(int<4>) -> int<4>,t,f>;
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
		const function IMP__operator_call_ = (v133 : ref<int<4>,f,f,plain>, v134 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
			var ref<dependencies,f,f,plain> v194 = dependency_after();
			recfun_to_dep_fun(tuple_member_access(*v134, 0ul, type_lit(recfun<int<4>,int<4>>)))(*v194, *v133-1);
			return treeture_done(1);
		}
	};)"
#define PREC_INNER_IR R"(
						prec(
								(build_recfun(
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
								))
						)
					)"

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(PREC_PREAMBLE_IR, "{ treeture_run(precfun_to_fun(", PREC_INNER_IR, ")(13)); }")
	{
		prec(
			[](int x)->bool { return x < 2; },
			[](int x)->int { return x; },
			[](int x, const auto& f) {
				auto dep1 = after();
				f(std::move(dep1), x - 1);
				return done(1);
			}
		)(13);
	}

	#pragma test expect_ir(PREC_PREAMBLE_IR, "{ treeture_run(precfun_to_dep_fun(", PREC_INNER_IR, ")(dependency_after(), 15)); }")
	{
		prec(
			[](int x)->bool { return x < 2; },
			[](int x)->int { return x; },
			[](int x, const auto& f) {
			auto dep1 = after();
			f(std::move(dep1), x - 1);
			return done(1);
		}
		)(after(),15);
	}

	return 0;
}

