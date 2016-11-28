#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_backend.h"

#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"

#include "allscale/compiler/lang/allscale_ir.h"

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

	TEST(CodeSnippet, Fib) {

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

	TEST(DISABLED_CodeSnippet, FibFull) {

		NodeManager mgr;

		auto fib = parse(mgr,
				R"(
					int<4> main() {
						auto p = 44;
						auto r = treeture_get(prec((build_recfun(
							  (i : int<4>) -> bool { return i < 2; },
							[ (i : int<4>) -> int<4> { return i; } ],
							[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
								let step = recfun_to_fun(steps.0);
								auto a = treeture_run(step(i-1));
								auto b = treeture_run(step(i-2));
								return treeture_done(treeture_get(a) + treeture_get(b));
							} ]
						)))(p));
						print("fib(%d)=%d\n",p,r);
						return 0;
					}
				)"
		);
		ASSERT_TRUE(fib);

		// convert with allscale backend
		auto code = convert(fib);
		ASSERT_TRUE(code);

		// compile to an actual binary
		EXPECT_TRUE(backend::compileTo(fib, "fib_art",3));
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
