#include <gtest/gtest.h>

#include "allscale/compiler/analysis/data_item_access.h"
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"
#include "insieme/utils/string_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/json_dump.h"


namespace allscale {
namespace compiler {
namespace analysis {

	using namespace insieme::core;

	namespace {

		DataItemAccesses getDataItemAccesses(NodeManager& mgr, const std::string& str, bool debug = false) {
			IRBuilder builder(mgr);

			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// parse the given statement
			auto stmt = builder.parseStmt(str,ext.getDefinedSymbols());
			EXPECT_TRUE(stmt) << "Failed to parse " << str << "\n";
			if (!stmt) assert_fail();

			// check for semantic errors
			assert_correct_ir(stmt);

			// compute data item accesses
			insieme::analysis::cba::haskell::Context ctxt;
			auto res = allscale::compiler::analysis::getDataItemAccesses(ctxt,stmt);

			// ensure that the analysis was successful
			EXPECT_TRUE(res.is_initialized()) << "Invalid solution obtained!";

			if (debug) {
				insieme::core::dump::json::dumpIR("ir.json",stmt);
				ctxt.dumpStatistics();
				ctxt.dumpSolution("solution",false);
			}

			// exception on timeout
			return res.value_or(DataItemAccesses());
		}

		bool isEmpty(const DataItemAccesses& accesses) {
			return accesses.empty();
		}

	}


	TEST(DataItemAccessesAnalysis, NoRequirements) {
		NodeManager mgr;

		// some literals => no data requirements
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"12"));
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"\"hello\""));

		// try some compound statements
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"{}"));
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"{ 12; }"));
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"{ 12; 14; }"));

		// also, there should be no dependency if only the reference of a element is obtained, but not accessed
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"data_item_element_access(lit(\"A\":ref<A>),12,type_lit(ref<int<4>>))"));

		// also a local variable operation should not cause a data requirement
		EXPECT_PRED1(isEmpty,getDataItemAccesses(mgr,"{ var ref<int<4>> x; x = 14; }"));
	}

	TEST(DataItemAccessesAnalysis, ReadRequirement) {
		NodeManager mgr;

		// acquire a reference and read from it
		EXPECT_EQ(
			"{0-1}",
			toString(getDataItemAccesses(mgr,
				R"(
					{
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(lit("A":ref<A>),12,type_lit(ref<int<4>>));
						// read the reference -- this creates a data requirement
						*ref;
					}
				)"
			))
		);

		// acquire a reference and read from it (using a literal to address element)
		EXPECT_EQ(
			"{0-1}",
			toString(getDataItemAccesses(mgr,
				R"(
					{
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(lit("A":ref<A>),lit("x":bla),type_lit(ref<int<4>>));
						// read the reference -- this creates a data requirement
						*ref;
					}
				)"
			))
		);

	}

	TEST(DataItemAccessesAnalysis, WriteRequirement) {
		NodeManager mgr;

		// acquire a reference and write to it
		EXPECT_EQ(
			"{0-1}",
			toString(getDataItemAccesses(mgr,
				R"(
					{
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(lit("A":ref<A>),12,type_lit(ref<int<4>>));
						// write to the reference -- this creates a data requirement
						ref = 14;
					}
				)"
			))
		);

	}


	TEST(DataItemAccessesAnalysis, ReadWriteRequirement) {
		NodeManager mgr;

		// acquire a reference and read from and write to it
		EXPECT_EQ(
			"{0-1,0-1-3-1}",
			toString(getDataItemAccesses(mgr,
				R"(
					{
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(lit("A":ref<A>),12,type_lit(ref<int<4>>));
						// read the reference -- this creates a data requirement
						ref = *ref;
					}
				)"
			))
		);

	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
