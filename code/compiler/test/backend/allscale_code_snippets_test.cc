#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_backend.h"

#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/core/allscale_core.h"

#include "../test_utils.inc"

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

		bool res = compileTo(code, tmp);

		// delete the temporary
		if (res) fs::remove(tmp);

		// return result
		return res;
	}

	insieme::backend::TargetCodePtr convertCode(const insieme::core::NodePtr& code) {
		// pass iput through the core ..
		auto result = allscale::compiler::core::convert(code);

		// check that it was a success
		assert_true(result.result)
			<< "Conversion of input code failed in the core:\n" << result.report;

		// .. and then through the backend
		return convert(result.result);
	}


	TEST(CodeSnippet, EmptyMain) {
		NodeManager mgr;

		// create an empty code snippet
		auto program = parse(mgr,
				"int<4> main() { return 0; }"
		);
		ASSERT_TRUE(program);

		// convert with allscale backend
		auto code = convertCode(program);
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
		auto code = convertCode(expr);
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
		auto code = convertCode(expr);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code);
	}

	TEST(CodeSnippet, FibEager) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					precfun_to_fun(prec((build_recfun(
						  to_closure((i : int<4>) -> bool { return i < 2; }),
						[ to_closure((i : int<4>) -> int<4> { return i; }) ],
						[ to_closure((i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							auto a = treeture_run(step(ref_cast(ref_temp_init(i-1), type_lit(t), type_lit(f), type_lit(cpp_ref))));
							auto b = treeture_run(step(ref_cast(ref_temp_init(i-2), type_lit(t), type_lit(f), type_lit(cpp_ref))));
							return treeture_done(treeture_get(a) + treeture_get(b));
						}) ]
					))))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convertCode(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibEagerRef) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					precfun_to_fun(prec((build_recfun(
						  to_closure((i : cpp_ref<int<4>,t,f>) -> bool { return i < 2; }),
						[ to_closure((i : cpp_ref<int<4>,t,f>) -> int<4> { return i; }) ],
						[ to_closure((i : cpp_ref<int<4>,t,f>, steps : (recfun<cpp_ref<int<4>,t,f>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							auto a = treeture_run(step(ref_cast(ref_temp_init(*i-1), type_lit(t), type_lit(f), type_lit(cpp_ref))));
							auto b = treeture_run(step(ref_cast(ref_temp_init(*i-2), type_lit(t), type_lit(f), type_lit(cpp_ref))));
							return treeture_done(treeture_get(a) + treeture_get(b));
						}) ]
					))))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convertCode(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibLazy) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					precfun_to_fun(prec((build_recfun(
						  to_closure((i : int<4>) -> bool { return i < 2; }),
						[ to_closure((i : int<4>) -> int<4> { return i; }) ],
						[ to_closure((i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
							return treeture_combine(dependency_after(),
									step(ref_cast(ref_temp_init(i-1), type_lit(t), type_lit(f), type_lit(cpp_ref))),
									step(ref_cast(ref_temp_init(i-2), type_lit(t), type_lit(f), type_lit(cpp_ref))),
									add,true);
						}) ]
					))))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convertCode(fib);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code) << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibLazyRef) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					precfun_to_fun(prec((build_recfun(
						  to_closure((i : cpp_ref<int<4>,t,f>) -> bool { return i < 2; }),
						[ to_closure((i : cpp_ref<int<4>,t,f>) -> int<4> { return i; }) ],
						[ to_closure((i : cpp_ref<int<4>,t,f>, steps : (recfun<cpp_ref<int<4>,t,f>,int<4>>)) -> treeture<int<4>,f> {
							let step = recfun_to_fun(steps.0);
							let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
							return treeture_combine(dependency_after(),
									step(ref_cast(ref_temp_init(*i-1), type_lit(t), type_lit(f), type_lit(cpp_ref))),
									step(ref_cast(ref_temp_init(*i-2), type_lit(t), type_lit(f), type_lit(cpp_ref))),
									add,true);
						}) ]
					))))
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convertCode(fib);
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
		auto code = convertCode(fib);
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
							  to_closure((i : int<4>) -> bool { return i < 2; }),
							[ to_closure((i : int<4>) -> int<4> { return i; }) ],
							[ to_closure((i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								auto a = treeture_run(step(ref_cast(ref_temp_init(i-1), type_lit(t), type_lit(f), type_lit(cpp_ref))));
								auto b = treeture_run(step(ref_cast(ref_temp_init(i-2), type_lit(t), type_lit(f), type_lit(cpp_ref))));
								return treeture_done(treeture_get(a) + treeture_get(b));
							}) ]
						))))(*p));
						print("fib(%d)=%d\n",*p,r);
						return 0;
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto trg = convertCode(fib);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

		// compile to an actual binary
		// NOTE: run result with "echo N | ./fib_eager_art"
//		EXPECT_TRUE(backend::compileTo(fib, "fib_eager_art",3))
//		 	 << "Failed to compile: " << *code;
	}

	TEST(CodeSnippet, FibLazyFull) {
		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					int<4> main() {
						var ref<int<4>> p;
						scan("%d",p);
						auto r = treeture_get(precfun_to_fun(prec((build_recfun(
							  to_closure((i : int<4>) -> bool { return i < 2; }),
							[ to_closure((i : int<4>) -> int<4> { return i; }) ],
							[ to_closure((i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								let add = ( a : int<4> , b : int<4> ) -> int<4> { return a + b; };
								return treeture_combine(dependency_after(),
									step(ref_cast(ref_temp_init(i-1), type_lit(t), type_lit(f), type_lit(cpp_ref))),
									step(ref_cast(ref_temp_init(i-2), type_lit(t), type_lit(f), type_lit(cpp_ref))),
									add,true);
							}) ]
						))))(*p));
						print("fib(%d)=%d\n",*p,r);
						return 0;
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto trg = convertCode(fib);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;

		// compile to an actual binary
		// NOTE: run result with "echo N | ./fib_lazy_art"
//		EXPECT_TRUE(backend::compileTo(fib, "fib_lazy_art",3))
//			<< "Failed to compile: " << *code;
	}


	TEST(CodeSnippet, DependencyOperations) {
		NodeManager mgr;

		auto prog = parse(mgr,
				R"(
					int<4> main() {
						var ref<dependencies,f,f,plain> v0 = dependency_after();
						var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(treeture_done(1));
						var ref<dependencies,f,f,plain> v2 = dependency_after(treeture_to_task_ref(*v1));
						var ref<treeture<int<4>,t>,f,f,plain> v3 = treeture_run(treeture_done(1));
						var ref<dependencies,f,f,plain> v4 = dependency_after(treeture_to_task_ref(*v1), treeture_to_task_ref(*v3));
						return 0;
					}
				)"
		);
		ASSERT_TRUE(prog);

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);
		auto trgCode = toString(*trg);

		// check that the resulting source is compiling
		EXPECT_PRED2(containsSubString, trgCode, "allscale::runtime::dependencies v0 = allscale::runtime::after();");
		EXPECT_PRED2(containsSubString, trgCode, "allscale::treeture<int32_t > v1 = allscale::treeture<int32_t >(1);");
		EXPECT_PRED2(containsSubString, trgCode, "allscale::runtime::dependencies v2 = allscale::runtime::after(v1);");
		EXPECT_PRED2(containsSubString, trgCode, "allscale::treeture<int32_t > v3 = allscale::treeture<int32_t >(1);");
		EXPECT_PRED2(containsSubString, trgCode, "allscale::runtime::dependencies v4 = allscale::runtime::after(v1, v3);");
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}


	TEST(CodeSnippet, CppEmptyMain) {
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	// make sure allscale BE does not break standard interception
	TEST(CodeSnippet, CppCout) {
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}


	TEST(CodeSnippet, VoidTreeture) {
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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCaptureValue) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {

						int var;
						prec(
								[](int)->bool { return true; },
								[var](int p) { var + p; },
								[](int,const auto& f) { return f(12); }
						)(1).wait();

						return 0;
				}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
		<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCaptureReference) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {

					int var;
					prec(
						[](int)->bool { return true; },
						[&var](int p) { var + p; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCapturePointerValue) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {
					int x;
					int* var = &x;
					prec(
						[](int)->bool { return true; },
						[var](int p) { *var + p; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCapturePointerReference) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {
					int x;
					int* var = &x;
					prec(
						[](int)->bool { return true; },
						[&var](int p) { *var + p; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCaptureStructValue) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				struct Data {
					int value;
				};

				int main(int argc, char** argv) {
					Data var;
					prec(
						[](int)->bool { return true; },
						[var](int p) { var.value + p; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCaptureStructReference) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				struct Data {
					int value;
				};

				int main(int argc, char** argv) {
					Data var;
					prec(
						[](int)->bool { return true; },
						[&var](int p) { var.value + p; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCaptureLambdaValue) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {
					auto fun = []() {
						return 3;
					};
					prec(
						[](int)->bool { return true; },
						[fun](int p) { fun() + 4; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(CodeSnippet, CppCaptureLambdaReference) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {
					auto fun = []() {
						return 3;
					};
					prec(
						[](int)->bool { return true; },
						[&fun](int p) { fun() + 4; },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}


	TEST(CodeSnippet, CppCaptureLambdaWithClosureValue) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {
					int x; int y;
					auto fun = [x,&y]() {};
					prec(
						[](int)->bool { return true; },
						[fun](int p) { fun(); },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

    TEST(CodeSnippet, CppCaptureLambdaWithClosureReference) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"

				using namespace allscale::api::core;

				int main(int argc, char** argv) {
					int x; int y;
					auto fun = [x,&y]() {};
					prec(
						[](int)->bool { return true; },
						[&fun](int p) { fun(); },
						[](int,const auto& f) { return f(12); }
					)(1).wait();

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}

	TEST(DISABLED_CodeSnippet, CppPforWrapper) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"
				#include "allscale/api/user/algorithm/pfor.h"

				using namespace allscale::api::core;
				using namespace allscale::api::user;

				template<typename Iter, typename Body>
				allscale::api::user::detail::loop_reference<Iter> my_pfor(const allscale::api::user::detail::range<Iter>& r, const Body& body) {
					allscale::api::user::detail::loop_reference<Iter> res(r,allscale::api::core::done());
					return res;
//					return { r , allscale::api::core::done() };
				}

				int main() {

					my_pfor(allscale::api::user::detail::range<int>(0,10),[](int){});

					return 0;
				}
			)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);

		// check for semantic errors
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}



	TEST(CodeSnippet, CppPforEmpty) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"
				#include "allscale/api/user/algorithm/pfor.h"

				using namespace allscale::api::core;
				using namespace allscale::api::user::algorithm;

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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}


	TEST(CodeSnippet, CppPforArray) {
		NodeManager mgr;

		auto code = R"(
				#include "allscale/api/core/prec.h"
				#include "allscale/api/user/algorithm/pfor.h"

				using namespace allscale::api::core;
				using namespace allscale::api::user::algorithm;

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
		ASSERT_TRUE(checks::check(prog).empty())
			<< printer::dumpErrors(checks::check(prog));

		// convert with allscale backend
		auto trg = convertCode(prog);
		ASSERT_TRUE(trg);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, trg) << "Failed to compile: " << *trg;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
