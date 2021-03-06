

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

#define SIMPLE_PREC_IR R"(
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
			recfun_to_fun(
					tuple_member_access(*v134, 0ul, type_lit(recfun<int<4>,int<4>>))
			)(*v133-1);
			return treeture_done(1);
		}
	};)"

#define SIMPLE_FUN_IR R"( var ref<precfun<int<4>,int<4>>,f,f,plain> simpleFun = )"

#define SIMPLE_PREC_CALL R"(
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
	))"

auto testFunReturnPrec() {
	return prec(fun(
			[](int x)->bool { return x < 2; },
			[](int x)->int { return x; },
			[](int x, const auto& f) {
				f(x - 1);
				return done(1);
			}
		)
	);
}

auto testFunReturnPrecCallResult(int i) {
	return prec(fun(
			[](int x)->bool { return x < 2; },
			[](int x)->int { return x; },
			[](int x, const auto& f) {
				f(x - 1);
				return done(1);
			}
		)
	)(i);
}

int main() {
	; // this is required because of the clang compound source location bug

	// direct call of prec result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{ treeture_run(precfun_to_fun(", SIMPLE_PREC_CALL, R"(
			)(14));
		}
	)")
	{
		prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		)(14);
	}

	// result of call to prec
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			treeture_run(precfun_to_fun(*simpleFun)(10));
		}
	)")
	{
		auto simpleFun = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		);

		simpleFun(10);
	}

	// result type of call to prec assigned to a variable
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			var ref<treeture<int<4>,t>,f,f,plain> res = treeture_run(precfun_to_fun(*simpleFun)(11));
		}
	)")
	{
		auto simpleFun = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		);

		auto res = simpleFun(11);
	}

	// result type of call to prec assigned to a variable - also calling get on the result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			var ref<treeture<int<4>,t>,f,f,plain> res = treeture_run(precfun_to_fun(*simpleFun)(12));
			treeture_get(*res);
		}
	)")
	{
		auto simpleFun = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		);

		auto res = simpleFun(12);
		res.get();
	}

	// result type of call to prec assigned to a variable - also calling get on the result and directly assigning the result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			var ref<int<4>,f,f,plain> i = *treeture_extract(treeture_run(precfun_to_fun(*simpleFun)(13)));
		}
	)")
	{
		auto simpleFun = prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		);

		auto i = simpleFun(13).get();
	}

	// direct call of prec result - using a variable not a value
	#pragma test expect_ir(SIMPLE_PREC_IR, "{ var ref<int<4>,f,f,plain> v0 = 15; treeture_run(precfun_to_fun(", SIMPLE_PREC_CALL, R"(
			)(*v0));
		}
	)")
	{
		int i = 15;
		prec(fun(
				[](int x)->bool { return x < 2; },
				[](int x)->int { return x; },
				[](int x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		)(i);
	}

	// call to result of function returning prec
	#pragma test expect_ir(SIMPLE_PREC_IR, R"(
		def IMP_testFunReturnPrec = function () -> precfun<int<4>,int<4>> {
			return
	)", SIMPLE_PREC_CALL, R"(;
		};
		{
			var ref<treeture<int<4>,t>,f,f,plain> v0 = treeture_run(precfun_to_fun(IMP_testFunReturnPrec())(4));
		}
	)")
	{
		auto a = testFunReturnPrec()(4);
	}

	// call to function returning the result of prec
	#pragma test expect_ir(SIMPLE_PREC_IR, R"(
		def IMP_testFunReturnPrecCallResult = function (i : ref<int<4>,f,f,plain>) -> treeture<int<4>,t> {
			return treeture_run(precfun_to_fun(
	)", SIMPLE_PREC_CALL, R"( )(*i));
		};
		{
			var ref<treeture<int<4>,t>,f,f,plain> v0 = IMP_testFunReturnPrecCallResult(5);
		}
	)")
	{
		auto a = testFunReturnPrecCallResult(5);
	}

	return 0;
}
