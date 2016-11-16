
#include <gtest/gtest.h>

#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/error_printer.h"

namespace allscale {
namespace compiler {
namespace lang {

	TEST(AllscaleModule, Fib) {
		ic::NodeManager nm;
		ic::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto fibIr = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i < 2; },
				[ (i : int<4>) -> int<4> { return i; } ],
				[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
					auto step = (j : int<4>) => recfun_call(steps.0, j);
					auto a = treeture_run(step(i-1));
					auto b = treeture_run(step(i-2));
					return treeture_done(treeture_get(a) + treeture_get(b));
				} ]
			)))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(fibIr);

		auto errs = ic::checks::check(fibIr);
		ASSERT_EQ(errs.getAll().size(), 0) << ic::printer::dumpErrors(errs);
	}

	TEST(AllscaleModule, Mutual) {
		ic::NodeManager nm;
		ic::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto evenIr = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i == 0; },
				[ (i : int<4>) -> bool { return true; }],
				[ (i : int<4>, steps : (recfun<int<4>,bool>, recfun<int<4>,bool>)) -> treeture<bool,f> {
					auto odd = (j : int<4>) => recfun_call(steps.1, j);
					return odd(i-1);
				} ] ),
				build_recfun(
				  (i : int<4>) -> bool { return i == 0; },
				[ (i : int<4>) -> bool { return false; }],
				[ (i : int<4>, steps : (recfun<int<4>,bool>, recfun<int<4>,bool>)) -> treeture<bool,f> {
					auto even = (j : int<4>) => recfun_call(steps.0, j);
					return even(i-1);
				} ] )
			))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(evenIr);

		auto errs = ic::checks::check(evenIr);
		ASSERT_EQ(errs.getAll().size(), 0) << ic::printer::dumpErrors(errs);
	}

}
}
}
