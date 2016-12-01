#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_backend.h"

#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"

#include "allscale/compiler/lang/allscale_ir.h"


#include "../frontend/test_utils.inc"

namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::core;
	namespace fs = boost::filesystem;

	using std::string;


	NodePtr parse(NodeManager& mgr, const string& code) {
		// create the builder
		IRBuilder builder(mgr);

		// get the AllScale language extension
		auto& as = mgr.getLangExtension<lang::AllscaleModule>();

		// parse the given code fragment
		return builder.parse(code,as.getDefinedSymbols());
	}


	bool isCompiling(const insieme::backend::TargetCodePtr& code) {

		// get a temporary file path
		auto tmp = fs::unique_path(fs::temp_directory_path() / "allscale-trg-%%%%%%%%");

		bool res = compileTo(code, tmp, 0, true);

		// delete the temporary
		if (res) fs::remove(tmp);

		// return result
		return res;
	}


	TEST(CodeSnippet, EmptyMain) {

		NodeManager mgr;

		// create an empty code snippet
		auto program = parse(mgr,
				"int<4> main() { return 0; }"
		);
		ASSERT_TRUE(program);

		// convert with allscale backend
		auto code = convert(program);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code);

	}

	TEST(CodeSnippet, SimpleExpression) {

		NodeManager mgr;

		// create an empty code snippet
		auto expr = parse(mgr,"12");
		ASSERT_TRUE(expr);

		// convert with allscale backend
		auto code = convert(expr);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code);

	}

	TEST(CodeSnippet, SimpleLambda) {

		NodeManager mgr;

		// create an empty code snippet
		auto expr = parse(mgr,"()->int<4> { return 1; }");
		ASSERT_TRUE(expr);

		// convert with allscale backend
		auto code = convert(expr);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code);

	}

	TEST(CodeSnippet, FibEager) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					prec((build_recfun(
						  (i : int<4>) -> bool { return i < 2; },
						[ (i : int<4>) -> int<4> { return i; } ],
						[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							auto a = treeture_run(step(i-1));
							auto b = treeture_run(step(i-2));
							return treeture_done(treeture_get(a) + treeture_get(b));
						} ]
					)))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibLazy) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					prec((build_recfun(
						  (i : int<4>) -> bool { return i < 2; },
						[ (i : int<4>) -> int<4> { return i; } ],
						[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
							return treeture_combine(step(i-1),step(i-2),add);
						} ]
					)))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, LambdaToClosure) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(

					def struct __wi__cutoff {
						const function IMP__operator_call_ = (v35 : ref<int<4>,f,f,plain>) -> bool {
							return *v35<2;
						}
					};
					def struct __wi__base {
						const function IMP__operator_call_ = (v81 : ref<int<4>,f,f,plain>) -> int<4> {
							return *v81;
						}
					};
					def struct __wi__step {
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

					{
						var ref<(int<4>) => treeture<int<4>,f>,f,f,plain> fibEager = prec(
								(build_recfun(
										lambda_to_closure(
												<ref<__wi__cutoff,f,f,plain>>(ref_temp(type_lit(__wi__cutoff))) {},
												type_lit((int<4>) => bool)
										),
										[lambda_to_closure(
												<ref<__wi__base,f,f,plain>>(ref_temp(type_lit(__wi__base))) {},
												type_lit((int<4>) => int<4>)
										)],
										[lambda_to_closure(
												<ref<__wi__step,f,f,plain>>(ref_temp(type_lit(__wi__step))) {},
												type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
										)]
								))
						);
						var ref<int<4>,f,f,plain> i = treeture_get(*(*fibEager)(12) materialize );
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;


	}


	TEST(CodeSnippet, CapturedState) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					{
						var ref<int<4>> a;
						var ref<int<4>> b;
						prec((build_recfun(
							  ( i : int<4> ) => i < *a,
							[ ( i : int<4> ) => i + *b ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								return step(i-1);
							} ]
						)))(12);
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(DISABLED_CodeSnippet, FibEagerFull) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					int<4> main() {
						var ref<int<4>> p;
						scan("%d",p);
						auto r = treeture_get(prec((build_recfun(
							  (i : int<4>) -> bool { return i < 2; },
							[ (i : int<4>) -> int<4> { return i; } ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								auto a = treeture_run(step(i-1));
								auto b = treeture_run(step(i-2));
								return treeture_done(treeture_get(a) + treeture_get(b));
							} ]
						)))(*p));
						print("fib(%d)=%d\n",*p,r);
						return 0;
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// compile to an actual binary
		EXPECT_TRUE(backend::compileTo(fib, "fib_eager_art",3));

		// NOTE: run result with "echo N | ./fib_eager_art"
	}

	TEST(DISABLED_CodeSnippet, FibLazyFull) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					int<4> main() {
						var ref<int<4>> p;
						scan("%d",p);
						auto r = treeture_get(prec((build_recfun(
							  (i : int<4>) -> bool { return i < 2; },
							[ (i : int<4>) -> int<4> { return i; } ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
								return treeture_combine(step(i-1),step(i-2),add);
							} ]
						)))(*p));
						print("fib(%d)=%d\n",*p,r);
						return 0;
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// compile to an actual binary
		EXPECT_TRUE(backend::compileTo(fib, "fib_lazy_art",3));

		// NOTE: run result with "echo N | ./fib_lazy_art"
	}


	TEST(CodeSnippet, CppInputEmptyMain) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main() {
					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}


//	TEST(DISABLED_CodeSnippet, CppFib) {
//		NodeManager mgr;
//
//		auto code = R"(
//				#include <iostream>
//				#include "allscale/api/core/prec.h"
//
//				using namespace allscale::api::core;
//
//				int fib(int x) {
//					auto f = prec(fun(
//						[](int x) { return x < 2; },
//						[](int x) { return x; },
//						[](int x, auto& rec) {
//							return done(3);
//						}
//					));
//					return f(x).get();
//				}
//
//				int main() {
//					std::cout << fib(12) << "\n";
//					return 0;
//				}
//			)";
//
//		auto prog = frontend::parseCode(mgr,code);
//		ASSERT_TRUE(prog);
//
//		// convert with allscale backend
//		auto trg = convert(prog);
//		ASSERT_TRUE(trg);
//
//		// check that the resulting source is compiling
//		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
//
//	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
