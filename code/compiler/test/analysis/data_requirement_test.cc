#include <gtest/gtest.h>

#include "allscale/compiler/analysis/data_requirement.h"
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"
#include "insieme/utils/string_utils.h"
#include "insieme/core/checks/full_check.h"

namespace allscale {
namespace compiler {
namespace analysis {

	using namespace insieme::core;

	TEST(AccessMode, Basic) {

		EXPECT_EQ("RO",toString(AccessMode::ReadOnly));
		EXPECT_EQ("RW",toString(AccessMode::ReadWrite));

	}

	TEST(DataRequirement, Basics) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto item  = builder.parseExpr("lit(\"A\":ref<Data>)");
		auto range = builder.parseExpr("12");


		DataRequirement readAccess(item,range,AccessMode::ReadOnly);
		DataRequirement writeAccess(item,range,AccessMode::ReadWrite);

		// test equality / inequality support
		EXPECT_EQ(readAccess,readAccess);
		EXPECT_EQ(writeAccess,writeAccess);

		EXPECT_EQ(readAccess,DataRequirement(item,range,AccessMode::ReadOnly));
		EXPECT_EQ(writeAccess,DataRequirement(item,range,AccessMode::ReadWrite));

		EXPECT_NE(readAccess,writeAccess);
		EXPECT_NE(writeAccess,readAccess);


		// test less-than comparable
		EXPECT_LT(readAccess,writeAccess);

		// test that they are printable
		EXPECT_EQ("Requirement { A[12] RO }",toString(readAccess));
		EXPECT_EQ("Requirement { A[12] RW }",toString(writeAccess));

	}

	TEST(DataRequirements, Basics) {

		DataRequirements requirements;

	}

	namespace {

		DataRequirements getDataRequirements(NodeManager& mgr, const std::string& str) {
			IRBuilder builder(mgr);

			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// parse the given statement
			auto stmt = builder.parseStmt(str,ext.getDefinedSymbols());
			EXPECT_TRUE(stmt) << "Failed to parse " << str << "\n";
			if (!stmt) assert_fail();

			// check for semantic errors
			assert_correct_ir(stmt);

			// compute data requirements
			return allscale::compiler::analysis::getDataRequirements(stmt);
		}

		bool isEmpty(const DataRequirements& req) {
			return req.empty();
		}

	}


	TEST(DataRequirementAnalysis, NoDependencies) {
		NodeManager mgr;

		// some literals => no data dependencies
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"12"));
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"\"hello\""));

		// try some compound statements
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{}"));
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{ 12; }"));
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{ 12; 14; }"));

		// also, there should be no dependency if only the reference of a element is obtained
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"data_item_element_access(lit(\"A\":ref<A>),12,type_lit(int<4>))"));
	}

//	TEST(DataRequirementAnalysis, ReadDependency) {
//		NodeManager mgr;
//
//		// aquire a reference and read from it
//		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,
//				R"(
//					{
//						// get a reference -- this does not cause a dependency
//						auto ref = data_item_element_access(item,12,type_lit(int<4>));
//						// read the reference -- this creates a data dependency
//						*ref;
//					}
//				)"
//		));
//
//
//	}


} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
