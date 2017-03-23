

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

#define SIMPLE_PREC_IR R"(
	decl struct __any_string__cutoff;
	decl struct __any_string__base;
	decl struct __any_string__step;
	decl IMP__operator_call_:const __any_string__cutoff::(ref<int<4>,t,f,cpp_ref>) -> bool;
	decl IMP__operator_call_:const __any_string__base::(ref<int<4>,t,f,cpp_ref>) -> int<4>;
	decl IMP__operator_call_:const __any_string__step::(ref<int<4>,t,f,cpp_ref>, (recfun<ref<int<4>,t,f,cpp_ref>,int<4>>)) -> treeture<int<4>,f>;
	decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_const_space_int_space__ampersand__rparen_:const __any_string__cutoff::() -> ptr<(ref<int<4>,t,f,cpp_ref>) -> bool,t,f>;
	decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_const_space_int_space__ampersand__rparen_:const __any_string__base::() -> ptr<(ref<int<4>,t,f,cpp_ref>) -> int<4>,t,f>;
	decl __any_string__outer : (ref<recfun<ref<int<4>,t,f,cpp_ref>,int<4>>,t,f,cpp_ref>) -> precfun<ref<int<4>,t,f,cpp_ref>,int<4>>;
	decl __any_string__middle : (ref<(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>),t,f,cpp_ref>) -> precfun<ref<int<4>,t,f,cpp_ref>,int<4>>;
	def struct __any_string__cutoff {
		const function IMP__operator_call_ = (v35 : ref<int<4>,t,f,cpp_ref>) -> bool {
			return *v35<2;
		}
	};
	def struct __any_string__base {
		const function IMP__operator_call_ = (v81 : ref<int<4>,t,f,cpp_ref>) -> int<4> {
			return *v81;
		}
	};
	def struct __any_string__step {
		const function IMP__operator_call_ = (v133 : ref<int<4>,t,f,cpp_ref>, v134 : ref<(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
			recfun_to_fun(
					tuple_member_access(*v134, 0ul, type_lit(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>))
			)(*v133-1);
			return treeture_done(1);
		}
	};
	def __any_string__outer = function (v95 : ref<recfun<ref<int<4>,t,f,cpp_ref>,int<4>>,t,f,cpp_ref>) -> precfun<ref<int<4>,t,f,cpp_ref>,int<4>> {
		return __any_string__middle((*v95)) materialize ;
	};
	def __any_string__middle = function (v94 : ref<(recfun<ref<int<4>,t,f,cpp_ref>,int<4>>),t,f,cpp_ref>) -> precfun<ref<int<4>,t,f,cpp_ref>,int<4>> {
		return prec(*v94);
	};)"

#define SIMPLE_FUN_IR R"( var ref<precfun<int<4>,int<4>>,f,f,plain> simpleFun = )"

#define SIMPLE_PREC_CALL R"(
	__any_string__outer(
			build_recfun(
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
			)
	))"

struct S {
	int i;
	S(int i) : i(i) {}
	virtual ~S() { 42; }
};

int main() {
	; // this is required because of the clang compound source location bug

	// -------- primitives

	// direct call of prec result
	#pragma test expect_ir(SIMPLE_PREC_IR, "{ treeture_run(precfun_to_fun(", SIMPLE_PREC_CALL, R"(
			)(14));
		}
	)")
	{
		prec(fun(
				[](const int& x)->bool { return x < 2; },
				[](const int& x)->int { return x; },
				[](const int& x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		)(14);
	}

	// direct call of prec result with a variable
	#pragma test expect_ir(SIMPLE_PREC_IR, "{ var ref<int<4>,f,f,plain> v0 = 15; treeture_run(precfun_to_fun(", SIMPLE_PREC_CALL, R"(
			)(*v0));
		}
	)")
	{
		int i = 15;
		prec(fun(
				[](const int& x)->bool { return x < 2; },
				[](const int& x)->int { return x; },
				[](const int& x, const auto& f) {
					f(x - 1);
					return done(1);
				}
			)
		)(i);
	}


	// -------- non-trivial types

	// pass by value
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl struct __any_string__S;
		decl main : () -> int<4>;
		decl IMP__operator_call_:const __any_string__cutoff::(__any_string__S) -> bool;
		decl IMP__operator_call_:const __any_string__base::(__any_string__S) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(__any_string__S, (recfun<__any_string__S,int<4>>)) -> treeture<int<4>,f>;
		decl __any_string__S::i : int<4>;
		decl ctor:__any_string__S::(int<4>);
		decl dtor:~__any_string__S::();
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_S_rparen_:const __any_string__cutoff::() -> ptr<(__any_string__S) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_S_rparen_:const __any_string__base::() -> ptr<(__any_string__S) -> int<4>,t,f>;
		decl __any_string__outer : (ref<recfun<__any_string__S,int<4>>,t,f,cpp_ref>) -> precfun<__any_string__S,int<4>>;
		decl __any_string__middle : (ref<(recfun<__any_string__S,int<4>>),t,f,cpp_ref>) -> precfun<__any_string__S,int<4>>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v699 : ref<__any_string__S,f,f,plain>) -> bool {
				return *v699.i<2;
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v745 : ref<__any_string__S,f,f,plain>) -> int<4> {
				return *v745.i;
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v796 : ref<__any_string__S,f,f,plain>, v809 : ref<(recfun<__any_string__S,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				var ref<__any_string__S,f,f,plain> v798 = __any_string__S::(ref_decl(type_lit(ref<__any_string__S,f,f,plain>)), *v796.i-1);
				recfun_to_fun(tuple_member_access(*v809, 0ul, type_lit(recfun<__any_string__S,int<4>>)))(
						ref_cast(v798, type_lit(t), type_lit(f), type_lit(cpp_ref)));
				recfun_to_fun(tuple_member_access(*v809, 0ul, type_lit(recfun<__any_string__S,int<4>>)))(
						ref_cast(__any_string__S::(ref_temp(type_lit(__any_string__S)), *v796.i-2), type_lit(t), type_lit(f), type_lit(cpp_ref)));
				return treeture_done(1);
			}
		};
		def __any_string__outer = function (v95 : ref<recfun<__any_string__S,int<4>>,t,f,cpp_ref>) -> precfun<__any_string__S,int<4>> {
			return __any_string__middle((*v95)) materialize ;
		};
		def __any_string__middle = function (v94 : ref<(recfun<__any_string__S,int<4>>),t,f,cpp_ref>) -> precfun<__any_string__S,int<4>> {
			return prec(*v94);
		};
		def struct __any_string__S {
			i : int<4>;
			ctor function (v653 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).i) {*v653};
			}
			dtor virtual function () {
				42;
			}
		};
		{
			treeture_run(
					precfun_to_fun(
							__any_string__outer(
									build_recfun(
											cpp_lambda_to_closure(
													<ref<__any_string__cutoff,f,f,plain>>(ref_temp(
															type_lit(__any_string__cutoff)
													)) {},
													type_lit((__any_string__S) => bool)
											),
											[cpp_lambda_to_closure(
													<ref<__any_string__base,f,f,plain>>(ref_temp(
															type_lit(__any_string__base)
													)) {},
													type_lit((__any_string__S) => int<4>)
											)],
											[cpp_lambda_to_closure(
													<ref<__any_string__step,f,f,plain>>(ref_temp(
															type_lit(__any_string__step)
													)) {},
													type_lit((__any_string__S, (recfun<__any_string__S,int<4>>)) => treeture<int<4>,f>)
											)]
									)
							)
					)(ref_cast(__any_string__S::(ref_temp(type_lit(__any_string__S)), 16), type_lit(t), type_lit(f), type_lit(cpp_ref)))
			);
		}
	)")
	{
		prec(fun(
				[](S x)->bool { return x.i < 2; },
				[](S x)->int { return x.i; },
				[](S x, const auto& f) {
					S s{x.i - 1};
					f(s);
					f({x.i - 2});
					return done(1);
				}
			)
		)({16});
	}

	// pass by const reference
	#pragma test expect_ir(R"(
		decl struct __any_string__cutoff;
		decl struct __any_string__base;
		decl struct __any_string__step;
		decl struct __any_string__S;
		decl main : () -> int<4>;
		decl IMP__operator_call_:const __any_string__cutoff::(ref<__any_string__S,t,f,cpp_ref>) -> bool;
		decl IMP__operator_call_:const __any_string__base::(ref<__any_string__S,t,f,cpp_ref>) -> int<4>;
		decl IMP__operator_call_:const __any_string__step::(ref<__any_string__S,t,f,cpp_ref>, (recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>)) -> treeture<int<4>,f>;
		decl __any_string__S::i : int<4>;
		decl ctor:__any_string__S::(int<4>);
		decl dtor:~__any_string__S::();
		decl IMP__conversion_operator_bool_space__lparen__star__rparen__lparen_const_space_S_space__ampersand__rparen_:const __any_string__cutoff::() -> ptr<(ref<__any_string__S,t,f,cpp_ref>) -> bool,t,f>;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_const_space_S_space__ampersand__rparen_:const __any_string__base::() -> ptr<(ref<__any_string__S,t,f,cpp_ref>) -> int<4>,t,f>;
		decl __any_string__outer : (ref<recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>,t,f,cpp_ref>) -> precfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>;
		decl __any_string__middle : (ref<(recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>),t,f,cpp_ref>) -> precfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>;
		def struct __any_string__cutoff {
			const function IMP__operator_call_ = (v699 : ref<__any_string__S,t,f,cpp_ref>) -> bool {
				return *v699.i<2;
			}
		};
		def struct __any_string__base {
			const function IMP__operator_call_ = (v745 : ref<__any_string__S,t,f,cpp_ref>) -> int<4> {
				return *v745.i;
			}
		};
		def struct __any_string__step {
			const function IMP__operator_call_ = (v796 : ref<__any_string__S,t,f,cpp_ref>, v809 : ref<(recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>),f,f,plain>) -> treeture<int<4>,f> {
				var ref<__any_string__S,f,f,plain> v798 = __any_string__S::(ref_decl(type_lit(ref<__any_string__S,f,f,plain>)), *v796.i-1);
				recfun_to_fun(tuple_member_access(*v809, 0ul, type_lit(recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>)))(
						ref_cast(v798, type_lit(t), type_lit(f), type_lit(cpp_ref)));
				recfun_to_fun(tuple_member_access(*v809, 0ul, type_lit(recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>)))(
						ref_cast(__any_string__S::(ref_temp(type_lit(__any_string__S)), *v796.i-2), type_lit(t), type_lit(f), type_lit(cpp_ref)));
				return treeture_done(1);
			}
		};
		def __any_string__outer = function (v95 : ref<recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>,t,f,cpp_ref>) -> precfun<ref<__any_string__S,t,f,cpp_ref>,int<4>> {
			return __any_string__middle((*v95)) materialize ;
		};
		def __any_string__middle = function (v94 : ref<(recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>),t,f,cpp_ref>) -> precfun<ref<__any_string__S,t,f,cpp_ref>,int<4>> {
			return prec(*v94);
		};
		def struct __any_string__S {
			i : int<4>;
			ctor function (v653 : ref<int<4>,f,f,plain>) {
				<ref<int<4>,f,f,plain>>((this).i) {*v653};
			}
			dtor virtual function () {
				42;
			}
		};
		{
			treeture_run(
					precfun_to_fun(
							__any_string__outer(
									build_recfun(
											cpp_lambda_to_closure(
													<ref<__any_string__cutoff,f,f,plain>>(ref_temp(
															type_lit(__any_string__cutoff)
													)) {},
													type_lit((ref<__any_string__S,t,f,cpp_ref>) => bool)
											),
											[cpp_lambda_to_closure(
													<ref<__any_string__base,f,f,plain>>(ref_temp(
															type_lit(__any_string__base)
													)) {},
													type_lit((ref<__any_string__S,t,f,cpp_ref>) => int<4>)
											)],
											[cpp_lambda_to_closure(
													<ref<__any_string__step,f,f,plain>>(ref_temp(
															type_lit(__any_string__step)
													)) {},
													type_lit((ref<__any_string__S,t,f,cpp_ref>, (recfun<ref<__any_string__S,t,f,cpp_ref>,int<4>>)) => treeture<int<4>,f>)
											)]
									)
							)
					)(ref_cast(__any_string__S::(ref_temp(type_lit(__any_string__S)), 17), type_lit(t), type_lit(f), type_lit(cpp_ref)))
			);
		}
	)")
	{
		prec(fun(
				[](const S& x)->bool { return x.i < 2; },
				[](const S& x)->int { return x.i; },
				[](const S& x, const auto& f) {
					S s{x.i - 1};
					f(s);
					f({x.i - 2});
					return done(1);
				}
			)
		)({17});
	}

	return 0;
}
