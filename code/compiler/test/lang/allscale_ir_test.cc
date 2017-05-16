
#include <gtest/gtest.h>

#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/error_printer.h"

namespace allscale {
namespace compiler {
namespace lang {

	TEST(AllscaleModule, Fib) {
		core::NodeManager nm;
		core::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto fibIr = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i < 2; },
				[ (i : int<4>) -> int<4> { return i; } ],
				[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
					auto step = recfun_to_fun(steps.0);
					auto a = treeture_run(step(i-1));
					auto b = treeture_run(step(i-2));
					return treeture_done(treeture_get(a) + treeture_get(b));
				} ]
			)))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(fibIr);

		auto errs = core::checks::check(fibIr);
		ASSERT_EQ(errs.getAll().size(), 0) << core::printer::dumpErrors(errs);
	}

	TEST(AllscaleModule, Mutual) {
		core::NodeManager nm;
		core::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto evenIr = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i == 0; },
				[ (i : int<4>) -> bool { return true; }],
				[ (i : int<4>, steps : (recfun<int<4>,bool>, recfun<int<4>,bool>)) -> treeture<bool,f> {
					auto odd = recfun_to_fun(steps.1);
					return odd(i-1);
				} ] ),
				build_recfun(
				  (i : int<4>) -> bool { return i == 0; },
				[ (i : int<4>) -> bool { return false; }],
				[ (i : int<4>, steps : (recfun<int<4>,bool>, recfun<int<4>,bool>)) -> treeture<bool,f> {
					auto even = recfun_to_fun(steps.0);
					return even(i-1);
				} ] )
			))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(evenIr);

		auto errs = core::checks::check(evenIr);
		ASSERT_EQ(errs.getAll().size(), 0) << core::printer::dumpErrors(errs);
	}


	TEST(PrecOperation, Fib) {
		core::NodeManager nm;
		core::IRBuilder builder(nm);
		auto& as = nm.getLangExtension<AllscaleModule>();

		auto fib = builder.parseExpr(R"I(
			prec((build_recfun(
				  (i : int<4>) -> bool { return i < 2; },
				[ (i : int<4>) -> int<4> { return i; } ],
				[ (i : int<4>, steps : (recfun<int<4>,int<4>>)) -> treeture<int<4>,f> {
					auto step = recfun_to_fun(steps.0);
					auto a = treeture_run(step(i-1));
					auto b = treeture_run(step(i-2));
					return treeture_done(treeture_get(a) + treeture_get(b));
				} ]
			)))
		)I", as.getDefinedSymbols());

		EXPECT_TRUE(fib);

		ASSERT_TRUE(PrecOperation::isEncoding(fib));

		// try to 'parse'
		PrecOperation op = PrecOperation::fromIR(fib);

		EXPECT_EQ("int<4>", 						toString(*op.getResultType()));
		EXPECT_EQ("int<4>", 						toString(*op.getParameterType()));
		EXPECT_EQ("treeture<int<4>,f>", 			toString(*op.getTreetureType().toIRType()));

		EXPECT_EQ(1, op.getFunctions().size());
		EXPECT_EQ("((int<4>)->bool)", 				toString(*op.getFunction().getBaseCaseTestType()));
		EXPECT_EQ("((int<4>)->int<4>)", 			toString(*op.getFunction().getBaseCaseType()));
		EXPECT_EQ("((int<4>,(recfun<int<4>,int<4>>))->treeture<int<4>,f>)", toString(*op.getFunction().getStepCaseType()));

		EXPECT_EQ(fib, op.toIR(nm));

	}

	TEST(TreetureType, Basic) {
		core::NodeManager nm;
		core::IRBuilder builder(nm);

		auto t1 = builder.parseType("treeture<int<4>,f>");
		EXPECT_TRUE(t1);
		EXPECT_PRED1(isTreeture, t1);

		// check value type
		EXPECT_EQ(builder.parseType("int<4>"),TreetureType(builder.parseType("treeture<int<4>,f>")).getValueType());
		EXPECT_EQ(builder.parseType("bool"),TreetureType(builder.parseType("treeture<bool,f>")).getValueType());

		// check release state
		EXPECT_FALSE(TreetureType(builder.parseType("treeture<int<4>,f>")).isReleased());
		EXPECT_TRUE (TreetureType(builder.parseType("treeture<int<4>,t>")).isReleased());
		EXPECT_FALSE(TreetureType(builder.parseType("treeture<int<4>,'a>")).isReleased());
		EXPECT_FALSE(TreetureType(builder.parseType("treeture<int<4>,'b>")).isReleased());

		// check back-conversion
		EXPECT_EQ(builder.parseType("treeture<int<4>,f>"),TreetureType(builder.parseType("treeture<int<4>,f>")).toIRType());
		EXPECT_EQ(builder.parseType("treeture<int<4>,t>"),TreetureType(builder.parseType("treeture<int<4>,t>")).toIRType());
		EXPECT_EQ(builder.parseType("treeture<int<4>,'a>"),TreetureType(builder.parseType("treeture<int<4>,'a>")).toIRType());
		EXPECT_EQ(builder.parseType("treeture<int<4>,'b>"),TreetureType(builder.parseType("treeture<int<4>,'b>")).toIRType());
	}

	TEST(TreetureType, IsTreeture) {
		core::NodeManager nm;
		core::IRBuilder builder(nm);

		// correct treeture types
		auto t1 = builder.parseType("treeture<int<4>,f>");
		ASSERT_TRUE(t1);
		EXPECT_TRUE(isTreeture(t1));

		auto t2 = builder.parseType("treeture<bool,f>");
		ASSERT_TRUE(t2);
		EXPECT_TRUE(isTreeture(t2));

		// incorrect treeture types
		auto it1 = builder.parseType("treeture<int<4>>");
		ASSERT_TRUE(it1);
		EXPECT_FALSE(isTreeture(it1));

		auto it2 = builder.parseType("treeture<int<4>,f,f>");
		ASSERT_TRUE(it2);
		EXPECT_FALSE(isTreeture(it2));

		auto it3 = builder.parseType("treeture<int<4>,x>");
		ASSERT_TRUE(it3);
		EXPECT_FALSE(isTreeture(it3));

		auto it4 = builder.parseType("tree<int<4>,f>");
		ASSERT_TRUE(it4);
		EXPECT_FALSE(isTreeture(it4));

		// check type of expression
		auto et1 = builder.parseExpr("decl foo  : () -> treeture<int<4>,f>; foo()");
		ASSERT_TRUE(et1);
		EXPECT_TRUE(isTreeture(et1));

		auto et2 = builder.parseExpr("decl bar : () -> treeture<int<4>,x>; bar()");
		ASSERT_TRUE(et2);
		EXPECT_FALSE(isTreeture(et2));
	}

	TEST(RecFunType, IsRecFun) {
		core::NodeManager nm;
		core::IRBuilder builder(nm);

		// correct recfun types
		auto rf1 = builder.parseType("recfun<bool,bool>");
		ASSERT_TRUE(rf1);
		EXPECT_TRUE(isRecFun(rf1));

		auto rf2 = builder.parseType("recfun<int<4>,real<8>>");
		ASSERT_TRUE(rf2);
		EXPECT_TRUE(isRecFun(rf2));

		// incorrect recfun types
		auto irf1 = builder.parseType("recfun<bool>");
		ASSERT_TRUE(irf1);
		EXPECT_FALSE(isRecFun(irf1));

		auto irf2 = builder.parseType("recfun<bool,bool,bool>");
		ASSERT_TRUE(irf2);
		EXPECT_FALSE(isRecFun(irf2));

		auto irf3 = builder.parseType("rec<bool,bool>");
		ASSERT_TRUE(irf3);
		EXPECT_FALSE(isRecFun(irf3));

		// check type of expression
		auto erf1 = builder.parseExpr("decl foo : () -> recfun<bool,bool>; foo()");
		ASSERT_TRUE(erf1);
		EXPECT_TRUE(isRecFun(erf1));

		// check type of expression
		auto erf2 = builder.parseExpr("decl bar : () -> recfun<bool>; bar()");
		ASSERT_TRUE(erf2);
		EXPECT_FALSE(isRecFun(erf2));
	}

} // end namespace lang
} // end namespace compiler
} // end namespace allscale
