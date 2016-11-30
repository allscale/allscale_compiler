

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

#define SIMPLE_PREC_IR R"(
	decl struct __any_string__cutoff;
	decl struct __any_string__base;
	decl struct __any_string__step;
	decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
	decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
	decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
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
		const function IMP__operator_call_ = (v133 : ref<int<4>,f,f,plain>, v134 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
			recfun_to_fun(
					tuple_member_access(*v134, 0ul, type_lit(recfun<int<4>,int<4>>))
			)(*v133-1);
			return treeture_done(1);
		}
	};)"

#define SIMPLE_FUN_IR R"( var ref<(int<4>) => treeture<int<4>,f>,f,f,plain> simpleFun = )"

#define SIMPLE_PREC_CALL R"(
	prec(
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

	//// simple type tests
	#pragma test expect_ir(R"({ var ref<treeture<int<4>,f>,f,f,plain> a = treeture_done(1); })")
	{
		auto a = done(1);
	}


	//// result type of call to prec assigned to a variable
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			var ref<treeture<int<4>,f>,f,f,plain> res = (*simpleFun)(12);
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
	}

	//// result type of call to prec assigned to a variable - also calling get on the result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			var ref<treeture<int<4>,f>,f,f,plain> res = (*simpleFun)(12);
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

	//// result type of call to prec assigned to a variable - also calling get on the result and directly assigning the result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_FUN_IR, SIMPLE_PREC_CALL, R"(
			;
			var ref<int<4>,f,f,plain> i = treeture_get(*(*simpleFun)(13) materialize);
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

	//// direct call of prec result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{", SIMPLE_PREC_CALL, R"(
			(14);
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

	//// call to function returning prec
	#pragma test expect_ir(SIMPLE_PREC_IR, R"(
		def IMP_testFunReturnPrec = function () -> (int<4>) => treeture<int<4>,f> {
			return
	)", SIMPLE_PREC_CALL, R"(;
		};
		{
			var ref<(int<4>) => treeture<int<4>,f>,f,f,plain> v0 = ref_cast(IMP_testFunReturnPrec() materialize , type_lit(f), type_lit(f), type_lit(cpp_rref));
		}
	)")
	{
		auto a = testFunReturnPrec();
	}

	//// call to function returning the result of prec
	#pragma test expect_ir(SIMPLE_PREC_IR, R"(
		def IMP_testFunReturnPrecCallResult = function (i : ref<int<4>>) -> treeture<int<4>,f> {
			return
	)", SIMPLE_PREC_CALL, R"((ref_kind_cast(i, type_lit(cpp_ref)));
		};
		{
			var ref<treeture<int<4>,f>,f,f,plain> v0 = IMP_testFunReturnPrecCallResult(5);
		}
	)")
	{
		auto a = testFunReturnPrecCallResult(5);
	}


	//// eager implementation of fib
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl IMP__operator_call_:const __any_string__cutoff::(int<4>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(int<4>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(int<4>, (recfun<int<4>,int<4>>)) -> treeture<int<4>,f>;
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
			const function IMP__operator_call_ = (v171 : ref<int<4>,f,f,plain>, v195 : ref<(recfun<int<4>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				var ref<treeture<int<4>,f>,f,f,plain> v173 = treeture_run(
					recfun_to_fun(
						tuple_member_access(*v195, 0ul, type_lit(recfun<int<4>,int<4>>))
					)(*v171-1)
				);
				var ref<treeture<int<4>,f>,f,f,plain> v178 = treeture_run(
					recfun_to_fun(
						tuple_member_access(*v195, 0ul, type_lit(recfun<int<4>,int<4>>))
					)(*v171-2)
				);
				return treeture_done(treeture_get(*v173)+treeture_get(*v178));
			}
		};
		{
			var ref<(int<4>) => treeture<int<4>,f>,f,f,plain> fibEager = prec(
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
			var ref<int<4>,f,f,plain> i = treeture_get(*(*fibEager)(12) materialize );
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

	return 0;
}
