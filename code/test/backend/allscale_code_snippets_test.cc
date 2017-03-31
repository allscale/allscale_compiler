#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_backend.h"

#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/checks/allscale_checks.h"


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

//		bool res = compileTo(code, tmp, 0, true);
		bool res = compileTo(code, tmp, 0);

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
					precfun_to_fun(prec((build_recfun(
						  (i : int<4>) -> bool { return i < 2; },
						[ (i : int<4>) -> int<4> { return i; } ],
						[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							auto a = treeture_run(step(i-1));
							auto b = treeture_run(step(i-2));
							return treeture_done(treeture_get(a) + treeture_get(b));
						} ]
					))))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibEagerRef) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					precfun_to_fun(prec((build_recfun(
						  (i : cpp_ref<int<4>,t,f>) -> bool { return i < 2; },
						[ (i : cpp_ref<int<4>,t,f>) -> int<4> { return i; } ],
						[ (i : cpp_ref<int<4>,t,f>, steps : (recfun<cpp_ref<int<4>,t,f>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							auto a = treeture_run(step(i-1));
							auto b = treeture_run(step(i-2));
							return treeture_done(treeture_get(a) + treeture_get(b));
						} ]
					))))
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
					precfun_to_fun(prec((build_recfun(
						  (i : int<4>) -> bool { return i < 2; },
						[ (i : int<4>) -> int<4> { return i; } ],
						[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
							return treeture_combine(dependency_after(),step(i-1),step(i-2),add,true);
						} ]
					))))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibLazyRef) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					precfun_to_fun(prec((build_recfun(
						  (i : cpp_ref<int<4>,t,f>) -> bool { return i < 2; },
						[ (i : cpp_ref<int<4>,t,f>) -> int<4> { return i; } ],
						[ (i : cpp_ref<int<4>,t,f>, steps : (recfun<cpp_ref<int<4>,t,f>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
							return treeture_combine(dependency_after(),step(i-1),step(i-2),add,true);
						} ]
					))))
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
						var ref<precfun<int<4>,int<4>>,f,f,plain> fibEager = prec(
								(build_recfun(
										cpp_lambda_to_closure(
												<ref<__wi__cutoff,f,f,plain>>(ref_temp(type_lit(__wi__cutoff))) {},
												type_lit((int<4>) => bool)
										),
										[cpp_lambda_to_closure(
												<ref<__wi__base,f,f,plain>>(ref_temp(type_lit(__wi__base))) {},
												type_lit((int<4>) => int<4>)
										)],
										[cpp_lambda_to_closure(
												<ref<__wi__step,f,f,plain>>(ref_temp(type_lit(__wi__step))) {},
												type_lit((int<4>, (recfun<int<4>,int<4>>)) => treeture<int<4>,f>)
										)]
								))
						);
						var ref<int<4>,f,f,plain> i = treeture_get(*(precfun_to_fun(*fibEager))(12) materialize );
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
						precfun_to_fun(prec((build_recfun(
							  ( i : int<4> ) => i < *a,
							[ ( i : int<4> ) => i + *b ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								return step(i-1);
							} ]
						))))(12);
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

	TEST(CodeSnippet, CapturedArray) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					{
						var ref<array<int<4>,12>> a = <ref<array<int<4>,12>>>(ref_decl(type_lit(ref<array<int<4>,12>>))){};
						precfun_to_fun(prec((build_recfun(
							  ( r : (int<4>,int<4>) ) => r.0 >= r.1,
							[
							  ( r : (int<4>,int<4>) ) => {
								for(int<4> i = r.0 .. r.1 ) {
									a[i] = 12;
								}
								return true;
							  }
							],[
							  ( r : (int<4>,int<4>), steps : (recfun<(int<4>,int<4>),bool>) ) => {
								let step = recfun_to_fun(steps.0);
								auto m = (r.0 + r.1) / 2 ;
								auto a = step(( r.0, m ));
								auto b = step(( m, r.1 ));
								treeture_get(a);
								treeture_get(b);
								return treeture_done(true);
							  }
							]
						))))((0,12));
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


	TEST(CodeSnippet, FibEagerFull) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					int<4> main() {
						var ref<int<4>> p;
						scan("%d",p);
						auto r = treeture_get(precfun_to_fun(prec((build_recfun(
							  (i : int<4>) -> bool { return i < 2; },
							[ (i : int<4>) -> int<4> { return i; } ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								auto a = treeture_run(step(i-1));
								auto b = treeture_run(step(i-2));
								return treeture_done(treeture_get(a) + treeture_get(b));
							} ]
						))))(*p));
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
		EXPECT_TRUE(backend::compileTo(fib, "fib_eager_art",3))
		 	 << "Failed to compile: " << *code;

		// NOTE: run result with "echo N | ./fib_eager_art"
	}

	TEST(CodeSnippet, FibLazyFull) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					int<4> main() {
						var ref<int<4>> p;
						scan("%d",p);
						auto r = treeture_get(precfun_to_fun(prec((build_recfun(
							  (i : int<4>) -> bool { return i < 2; },
							[ (i : int<4>) -> int<4> { return i; } ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
								return treeture_combine(dependency_after(),step(i-1),step(i-2),add,true);
							} ]
						))))(*p));
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
		EXPECT_TRUE(backend::compileTo(fib, "fib_lazy_art",3))
			<< "Failed to compile: " << *code;

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

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}

	// make sure allscale BE does not break standard interception
	TEST(CodeSnippet, CppInputCout) {
		NodeManager mgr;

		auto code = R"(
				#include <iostream>

				int main() {
					std::cout << 1 << std::endl;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);
	}

	TEST(CodeSnippet, CppEmptyMainWithArgs) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				int main(int argc, char** argv) {
					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}

	TEST(CodeSnippet, CppFib) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int fib(int x) {
					auto f = prec(fun(
						[](int x) { return x < 2; },
						[](int x) { return x; },
						[](int x, const auto& rec) {
							auto a = run(rec(x-1));
							auto b = run(rec(x-2));
							return done(a.get() + b.get());
						}
					));
					return f(x).get();
				}

				int main() {
					return fib(12);
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}

	TEST(CodeSnippet, CppFibLazy) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"
				#include "allscale/api/user/arithmetic.h"

				using namespace allscale::api::core;

				int main() {
					auto fib = prec(fun(
							[](int x)->bool { return x < 2; },
							[](int x)->int { return x; },
							[](int x, const auto& f) {
								return allscale::api::user::add(f(x - 1), f(x - 2));
							}
						)
					);

					auto res = fib(4);

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}

	TEST(DISABLED_CodeSnippet, CppFac) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"
				#include "allscale/api/user/arithmetic.h"

				using namespace allscale::api::core;

				int main() {
					auto fac = prec(fun(
							[](int x)->bool { return x < 2; },
							[](int x)->int { return 1; },
							[](int x, const auto& f) {
								return allscale::api::user::mul(done(x), f(x - 1));
							}
						)
					);

					auto res = fac(4);

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}

	TEST(CodeSnippet, CppRange) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;
				
				int main() {
					
					struct range {
						int start;
						int end;
					};
			
					range full;
					full.start = 0;
					full.end = 1000;
					prec(fun(
						[](range r) { return r.start+ 1 >= r.end; },
						[](range r) {
							for(int i=r.start; i<r.end; i++) {
								// nothing
							}
							return true;
						},
						[](range r, const auto& f) {
							int m = r.start + (r.start + r.end) / 2;
							auto a = run(f(range{r.start,m}));
							auto b = run(f(range{m,r.end}));
							a.get();
							b.get();
							return done(true);
						} 
					))(full).get();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}


	TEST(CodeSnippet, CppRangeReference) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;
				
				int main(int argc, char** argv) {
					
					struct range {
						int start;
						int end;
					};
			
					prec(fun(
						[](const range& r) { return r.start + 1 >= r.end; },
						[](const range& r) {
							for(int i=r.start; i<r.end; i++) {
								// nothing
							}
							return true;
						},
						[](const range& r, const auto& f) {
							int m = r.start + (r.start + r.end) / 2;
							auto a = run(f(range{r.start,m}));
							auto b = run(f(range{m,r.end}));
							a.get();
							b.get();
							return done(true);
						} 
					))(range{0,1000}).get();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}

	TEST(CodeSnippet, IntTreeture) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;
				
				int main(int argc, char** argv) {
					
					prec(fun(
						[](int)->bool { return true; },
						[](int) { return 10; },
						[](int,const auto& f) { return done(12); }
					))(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}


	TEST(DISABLED_CodeSnippet, VoidTreeture) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;
				
				int main(int argc, char** argv) {
					
					prec(fun(
						[](int)->bool { return true; },
						[](int) {},
						[](int,const auto& f) { return done(); }
					))(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}


	TEST(DISABLED_CodeSnippet, CppPforEmpty) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"
				#include "allscale/api/user/operator/pfor.h"

				using namespace allscale::api::core;
				using namespace allscale::api::user;
				
				int main(int argc, char** argv) {
					
					pfor(0,10,[](int){
						// nothing to do
					});

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(core::checks::check(prog).empty())
			<< core::printer::dumpErrors(core::checks::check(prog));

		// convert with allscale backend
		auto trg = convert(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

	}


	TEST(DISABLED_CodeSnippet, CppPforArray) {
			NodeManager mgr;

			auto code = R"(
					#include "allscale/api/core/prec.h"
					#include "allscale/api/user/operator/pfor.h"

					using namespace allscale::api::core;
					using namespace allscale::api::user;
					
					int main(int argc, char** argv) {
						
						int A[10];

						pfor(0,10,[&](int x){
							A[x] = 10;
						});

						return 0;
					}
				)";

			auto prog = frontend::parseCode(mgr,code);
			ASSERT_TRUE(prog);

			// check for semantic errors
			ASSERT_TRUE(core::checks::check(prog).empty())
				<< core::printer::dumpErrors(core::checks::check(prog));

			// convert with allscale backend
			auto trg = convert(prog);
			ASSERT_TRUE(trg);

			// check that the resulting source is compiling
			EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

		}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
