#include <gtest/gtest.h>

#include "allscale/compiler/analysis/out_of_bounds_analysis.h"

#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/dump/json_dump.h"

#include "insieme/utils/assert.h"

#include "insieme/driver/integration/tests.h"

namespace allscale {
namespace compiler {
namespace analysis {

	using namespace insieme::core;
	using namespace insieme::analysis::cba::haskell;

	TEST(OutOfBounds, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_decl(type_lit(ref<array<int<4>, 21u>>));"
			"	*ptr_subscript(ptr_from_array(a), 42);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, BasicNoOutOfBounds) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_decl(type_lit(ref<array<int<4>, 21u>>));"
			"	*ptr_subscript(ptr_from_array(a), 20);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsNotOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, Sliced) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_decl(type_lit(ref<array<int<4>, 21u>>));"
			"	var ref<ptr<int<4>>> ap = ptr_add(ptr_from_array(a), 10);"
			"	var ref<int<8>> i = ref_decl(type_lit(ref<int<8>>));"
			"   *ptr_subscript(*ap, 20);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[3].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, UsingNew) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<ptr<int<4>>> a = ptr_from_array(<ref<array<int<4>,21u>>>(ref_new(type_lit(array<int<4>,21u>))) {});"
			"	*ptr_subscript(*a, 42);"
			"	ref_delete(ptr_to_array(*a));"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, UsingUninitializedIndex) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_decl(type_lit(ref<array<int<4>, 21u>>));"
			"	var ref<int<8>> b = ref_decl(type_lit(ref<int<8>>));"
			"	*ptr_subscript(ptr_from_array(a), *b);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[2].as<CallExprAddress>();

		EXPECT_EQ(OutOfBoundsResult::MayBeOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, Assignment) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_decl(type_lit(ref<array<int<4>, 21u>>));"
			"	var ref<int<4>> b = *ptr_subscript(ptr_from_array(a), 42);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<DeclarationStmtAddress>().getDeclaration().getChildAddresses()[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, AssignmentNotOutOfBounds) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_decl(type_lit(ref<array<int<4>, 21u>>));"
			"	var ref<int<4>> b = *ptr_subscript(ptr_from_array(a), 20);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<DeclarationStmtAddress>().getDeclaration().getChildAddresses()[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsNotOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(DISABLED_OutOfBounds, NullPointer) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<array<int<4>, 21u>> a = ref_null(type_lit(array<int<4>, 21u>), type_lit(f), type_lit(f));"
			"	*ptr_subscript(ptr_from_array(a), 42);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<CallExprAddress>();

		EXPECT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, Scalar) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<int<4>> a = ref_decl(type_lit(ref<int<4>>));"
			"	var ref<ptr<int<4>>> ap = ptr_from_ref(a);"
			"	*ptr_subscript(*ap, 42);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[2].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, ScalarNotOutOfBounds) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"{"
			"	var ref<int<4>> a = ref_decl(type_lit(ref<int<4>>));"
			"	var ref<ptr<int<4>>> ap = ptr_from_ref(a);"
			"	*ptr_subscript(*ap, 0);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[2].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsNotOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, DISABLED_InsideStruct) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"def struct container {"
			"    array : array<int<4>,21u>;"
			"};"

			"{"
			"	var ref<container> c = ref_decl(type_lit(ref<container>));"
			"	*ptr_subscript(ptr_from_array(c.array), 42);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, DISABLED_InsideStructNotOutOfBounds) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"def struct container {"
			"    array : array<int<4>,21u>;"
			"};"

			"{"
			"	var ref<container> c = ref_decl(type_lit(ref<container>));"
			"	*ptr_subscript(ptr_from_array(c.array), 10);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[1].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsNotOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, AcrossFunctionCall) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"def fun = function (arr : ref<ptr<int<4>>>) -> ptr<int<4>> {"
			"	return ptr_add(*arr, 20);"
			"};"

			"{"
			"	var ref<array<int<4>,21u>> arr = ref_decl(type_lit(ref<array<int<4>,21u>>));"
			"	var ref<ptr<int<4>>> ap = fun(ptr_from_array(arr));"
			"	*ptr_subscript(*ap, 10);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[2].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, AcrossFunctionCallNotOutOfBounds) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctx;

		auto stmt = builder.parseStmt(
			"def fun = function (arr : ref<ptr<int<4>>>) -> ptr<int<4>> {"
			"	return ptr_add(*arr, 10);"
			"};"

			"{"
			"	var ref<array<int<4>,21u>> arr = ref_decl(type_lit(ref<array<int<4>,21u>>));"
			"	var ref<ptr<int<4>>> ap = fun(ptr_from_array(arr));"
			"	*ptr_subscript(*ap, 10);"
			"}"
		).as<CompoundStmtPtr>();
		EXPECT_EQ(0, checks::check(stmt).size()) << printer::dumpErrors(checks::check(stmt));

		auto call = CompoundStmtAddress(stmt)[2].as<CallExprAddress>();

		ASSERT_EQ(OutOfBoundsResult::IsNotOutOfBounds, getOutOfBounds(ctx, call));
	}

	TEST(OutOfBounds, InputCode) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		Context ctxt;

		// load some input code
		auto testCase = insieme::driver::integration::getCase("seq/c/pendulum");
		ASSERT_TRUE(testCase);
		ProgramPtr prog = testCase->load(mgr);
		ASSERT_TRUE(prog);

		// collect all reads
		std::vector<CallExprAddress> reads;
		auto& refExt = mgr.getLangExtension<lang::ReferenceExtension>();
		visitDepthFirst(ProgramAddress(prog),[&](const CallExprAddress& call){
			if (refExt.isCallOfRefDeref(call)) {
				reads.push_back(call);
			}
		});

//		std::cout << "Total number of reads: " << reads.size() << "\n";

		// check all those for out-of-bound violations
		std::map<OutOfBoundsResult,int> counter;
		for(const auto& read : reads) {
			auto cur = getOutOfBounds(ctxt,read);
			counter[cur]++;
		}

//		std::cout << "Not out-of-bound: " << counter[OutOfBoundsResult::IsNotOutOfBounds] << "\n";
//		std::cout << " Is out-of-bound: " << counter[OutOfBoundsResult::IsOutOfBounds] << "\n";
//		std::cout << "May out-of-bound: " << counter[OutOfBoundsResult::MayBeOutOfBounds] << "\n";

		EXPECT_LE(2155,counter[OutOfBoundsResult::IsNotOutOfBounds]);
		EXPECT_GE(   0,counter[OutOfBoundsResult::IsOutOfBounds]);
		EXPECT_GE(  39,counter[OutOfBoundsResult::MayBeOutOfBounds]);

	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
