#include <gtest/gtest.h>

#include "allscale/compiler/analysis/out_of_bounds_analysis.h"

#include <fstream>

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

	namespace {

		struct summary {
			std::vector<CallExprAddress> notOutOfBounds;
			std::vector<CallExprAddress> mayOutOfBounds;
			std::vector<CallExprAddress> isOutOfBounds;

			std::size_t numAccesses() const {
				return notOutOfBounds.size() + mayOutOfBounds.size() + isOutOfBounds.size();
			}
		};

		summary eval(const NodePtr& code, bool debug = false) {
			NodeManager& mgr = code.getNodeManager();

			summary res;

			// collect all reads
			std::vector<CallExprAddress> reads;
			auto& refExt = mgr.getLangExtension<lang::ReferenceExtension>();
			visitDepthFirst(NodeAddress(code),[&](const CallExprAddress& call){
				if (refExt.isCallOfRefDeref(call) ) {
					reads.push_back(call);
				}
			});


			// check all those for out-of-bound violations
			Context ctxt;
			std::map<OutOfBoundsResult,int> counter;
			for(const auto& read : reads) {
				switch (getOutOfBounds(ctxt,read)) {
					case OutOfBoundsResult::IsNotOutOfBounds: res.notOutOfBounds.push_back(read); break;
					case OutOfBoundsResult::MayBeOutOfBounds: res.mayOutOfBounds.push_back(read); break;
					case OutOfBoundsResult::IsOutOfBounds:    res.isOutOfBounds.push_back(read); break;
				}
			}

			// for debugging, print report
			if (debug) {

				std::cout << "MayOutOfBounds:\n";
				for(const auto& cur : res.mayOutOfBounds) {
					std::cout << cur << "\n";
				}

				std::cout << "IsOutOfBounds:\n";
				for(const auto& cur : res.isOutOfBounds) {
					std::cout << cur << "\n";
				}

				dump::json::dumpIR("code.json",code);
				ctxt.dumpSolution();

			}

			// done
			return res;
		}

		summary eval(const std::string& caseName, bool debug = false) {
			NodeManager mgr;

			// parse test case configuration
			auto testCase = insieme::driver::integration::getCase(caseName);
			EXPECT_TRUE(testCase) << "No such test case: " << caseName;
			if (!testCase) exit(1);

			// load the input program
			ProgramPtr prog = testCase->load(mgr);
			EXPECT_TRUE(prog);
			if (!prog) exit(1);

			// evaluate the out-of-bound query
			return eval(prog,debug);
		}


	}


	TEST(OutOfBounds, InputCode) {

		// evaluate out-of-bounds for pendulum
		auto res = eval("seq/c/pendulum");

//		std::cout << "Total number of accesses: " << res.numAccesses() << "\n";
//		std::cout << "Not out-of-bound: " << res.notOutOfBounds.size() << "\n";
//		std::cout << " Is out-of-bound: " << res. isOutOfBounds.size() << "\n";
//		std::cout << "May out-of-bound: " << res.mayOutOfBounds.size() << "\n";

		EXPECT_LE(2155,res.notOutOfBounds.size());
		EXPECT_GE(   0,res. isOutOfBounds.size());
		EXPECT_GE(  39,res.mayOutOfBounds.size());

	}

	TEST(DISABLED_OutOfBounds, Survey) {

		// load list of programs
		std::vector<std::string> programs;
		{
			std::ifstream file("integration_tests_actual.txt");
			std::string str;
			while (std::getline(file, str)) {
				programs.push_back(str);
			}
		}

		std::cout << "Number of test cases: " << programs.size() << "\n";

		std::map<std::string,std::tuple<std::size_t,std::size_t,std::size_t>> data;

		auto printData = [&]() {
			std::cout << "Name,NotOutOfBound,MayOutOfBound,IsOutOfBound\n";
			for(const auto& cur : data) {
				std::cout << cur.first
						<< "," << std::get<0>(cur.second)
						<< "," << std::get<1>(cur.second)
						<< "," << std::get<2>(cur.second)
						<< "\n";
			}
			std::cout << "\n";
		};


		for(const auto& name : programs) {
			std::cout << "Evaluating: " << name << " (" << (data.size() + 1) << "/" << programs.size() << ")\n";

			auto res = eval(name);
			std::cout << "Total number of accesses: " << res.numAccesses() << "\n";
			std::cout << "Not out-of-bound: " << res.notOutOfBounds.size() << "\n";
			std::cout << " Is out-of-bound: " << res. isOutOfBounds.size() << "\n";
			std::cout << "May out-of-bound: " << res.mayOutOfBounds.size() << "\n";
			std::cout << "\n";

			// record data
			data[name] = std::make_tuple(
					res.notOutOfBounds.size(),
					res.mayOutOfBounds.size(),
					res.isOutOfBounds.size()
			);

			// print all data
			printData();
		}

	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
