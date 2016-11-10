
#include <gtest/gtest.h>

#include "allscale/core/allscale_ir.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_builder.h"

namespace allscale {
namespace compiler {

	TEST(AllscaleModule, Fib) {
		ic::NodeManager nm;
		ic::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto fibIr = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i < 2; },
				[ (i : int<4>) -> int<4> { return i; } ],
				[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>> {
					auto step = (j : int<4>) => recfun_call(steps.0, j);
					auto a = step(i-1);
					auto b = step(i-2);
					return treeture_done(treeture_get(a) + treeture_get(b));
				} ]
			)))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(fibIr);

		ASSERT_EQ(ic::checks::check(fibIr).getAll().size(), 0);
	}

	TEST(AllscaleModule, Mutual) {
		ic::NodeManager nm;
		ic::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto evenIr = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i == 0; },
				[ (i : int<4>) -> bool { return true; }],
				[ (i : int<4>, steps : (recfun<int<4>,bool>, recfun<int<4>,bool>)) -> treeture<bool> {
					auto odd = (j : int<4>) => recfun_call(steps.1, j);
					return odd(i-1);
				} ] ),
				build_recfun(
				  (i : int<4>) -> bool { return i == 0; },
				[ (i : int<4>) -> bool { return false; }],
				[ (i : int<4>, steps : (recfun<int<4>,bool>, recfun<int<4>,bool>)) -> treeture<bool> {
					auto even = (j : int<4>) => recfun_call(steps.0, j);
					return even(i-1);
				} ] )
			))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(evenIr);

		ASSERT_EQ(ic::checks::check(evenIr).getAll().size(), 0);
	}

}
}