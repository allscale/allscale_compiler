#include <gtest/gtest.h>

#include "allscale/compiler/analysis/data_requirement.h"
#include "allscale/compiler/lang/allscale_ir.h"

#include "insieme/core/ir_builder.h"
#include "insieme/utils/string_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/json_dump.h"

namespace allscale {
namespace compiler {
namespace analysis {

	using namespace insieme::core;

	TEST(AccessMode, Basic) {

		EXPECT_EQ("RO",toString(AccessMode::ReadOnly));
		EXPECT_EQ("RW",toString(AccessMode::ReadWrite));

	}

	TEST(DataRange,Basic) {

		// check the default constructor
		DataRange range;

		EXPECT_TRUE(range.isUnknown());
		EXPECT_EQ("unknown",toString(range));

		EXPECT_EQ("empty",toString(DataRange::empty()));
		EXPECT_EQ("empty",toString(DataRange::empty()));

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto e2 = builder.parseExpr("2");
		auto e4 = builder.parseExpr("4");
		auto e8 = builder.parseExpr("8");


		// -- unknown --

		auto u = range;
		EXPECT_EQ("unknown",toString(u));
		EXPECT_TRUE(u.isUnknown());
		EXPECT_FALSE(u.isEmpty());


		// -- empty --

		auto e = DataRange::empty();
		EXPECT_EQ("empty",toString(e));
		EXPECT_FALSE(e.isUnknown());
		EXPECT_TRUE(e.isEmpty());


		// -- terms --

		auto r2 = DataRange::term(e2);
		auto r4 = DataRange::term(e4);
		auto r8 = DataRange::term(e8);

		EXPECT_EQ("2",toString(r2));
		EXPECT_EQ("4",toString(r4));
		EXPECT_EQ("8",toString(r8));

		// -- unions --

		auto u24 = DataRange::merge(r2,r4);
		auto u28 = DataRange::merge(r2,r8);
		auto u48 = DataRange::merge(r4,r8);

		EXPECT_EQ("union{2,4}",toString(u24));
		EXPECT_EQ("union{2,8}",toString(u28));
		EXPECT_EQ("union{4,8}",toString(u48));

		// -- spans --

		auto s24 = DataRange::span(r2,r4);
		auto s28 = DataRange::span(r2,r8);
		auto s48 = DataRange::span(r4,r8);

		EXPECT_EQ("span(2,4)",toString(s24));
		EXPECT_EQ("span(2,8)",toString(s28));
		EXPECT_EQ("span(4,8)",toString(s48));



		// -- combinations --

		EXPECT_EQ("empty",       toString(DataRange::merge()));
		EXPECT_EQ("2",           toString(DataRange::merge(r2)));
		EXPECT_EQ("union{2,4}",  toString(DataRange::merge(u24)));
		EXPECT_EQ("union{2,4,8}",toString(DataRange::merge(u24,u48)));


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

		DataRequirements getDataRequirements(NodeManager& mgr, const std::string& str, bool debug = false) {
			IRBuilder builder(mgr);

			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// parse the given statement
			auto stmt = builder.parseStmt(str,ext.getDefinedSymbols());
			EXPECT_TRUE(stmt) << "Failed to parse " << str << "\n";
			if (!stmt) assert_fail();

			// check for semantic errors
			assert_correct_ir(stmt);

			// compute data requirements
			insieme::analysis::cba::haskell::Context ctxt;
			auto res = allscale::compiler::analysis::getDataRequirements(ctxt,stmt);

			if (debug) {
				insieme::core::dump::json::dumpIR("ir.json",stmt);
				ctxt.dumpStatistics();
				ctxt.dumpSolution();
			}

			// exception on timeout
			return *res;
		}

		bool isEmpty(const DataRequirements& req) {
			return req.empty();
		}

	}


	TEST(DataRequirementAnalysis, NoRequirements) {
		NodeManager mgr;

		// some literals => no data requirements
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"12"));
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"\"hello\""));

		// try some compound statements
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{}"));
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{ 12; }"));
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{ 12; 14; }"));

		// also, there should be no dependency if only the reference of a element is obtained, but not accessed
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"data_item_element_access(lit(\"A\":ref<A>),12,type_lit(ref<int<4>>))"));

		// also a local variable operation should not cause a data requirement
		EXPECT_PRED1(isEmpty,getDataRequirements(mgr,"{ var ref<int<4>> x; x = 14; }"));
	}

	TEST(DataRequirementAnalysis, ReadRequirement) {
		NodeManager mgr;

		// acquire a reference and read from it
		EXPECT_EQ(
			"{Requirement { A[12] RO }}",
			toString(getDataRequirements(mgr,
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
			"{Requirement { A[x] RO }}",
			toString(getDataRequirements(mgr,
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

	TEST(DataRequirementAnalysis, WriteRequirement) {
		NodeManager mgr;

		// acquire a reference and write to it
		EXPECT_EQ(
			"{Requirement { A[12] RW }}",
			toString(getDataRequirements(mgr,
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


	TEST(DataRequirementAnalysis, ReadWriteRequirement) {
		NodeManager mgr;

		// acquire a reference and read from and write to it
		EXPECT_EQ(
			"{Requirement { A[12] RO },Requirement { A[12] RW }}",
			toString(getDataRequirements(mgr,
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

	TEST(DataRequirementAnalysis, WrappedRange) {
		NodeManager mgr;

		// acquire a reference and read from it
		EXPECT_EQ(
			"{Requirement { A[point(12)] RO }}",
			toString(getDataRequirements(mgr,
				R"(
					{
						let point = lit("point" : (int<4>)->point);
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(lit("A":ref<A>),point(12),type_lit(ref<int<4>>));
						// read the reference -- this creates a data requirement
						*ref;
					}
				)"
			))
		);

	}

	TEST(DataRequirementAnalysis, AccessInFunction) {
		NodeManager mgr;

		// acquire a reference and read from it
		EXPECT_EQ(
			"{Requirement { A[12] RO },Requirement { A[12] RW }}",
			toString(getDataRequirements(mgr,
				R"(
					def inc = ( r : ref<int<4>> ) -> unit {
						r = *r + 1;
					};

					{
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(lit("A":ref<A>),12,type_lit(ref<int<4>>));

						// update value in function -- should add data requirements
						inc(ref);
					}
				)"
			))
		);

		// obtain the reference in a nested scope
		EXPECT_EQ(
			"{Requirement { A[12] RO },Requirement { A[12] RW }}",
			toString(getDataRequirements(mgr,
				R"(
					def inc = ( r : ref<'a>, i : 'b ) -> unit {
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(r,i,type_lit(ref<int<4>>));
						// read/write the reference
						ref = *ref + 1;
					};

					{
						inc(lit("A":ref<A>),12);
					}
				)"

			))
		);

		// obtain the reference in a nested scope with modification
		EXPECT_EQ(
			"{Requirement { A[12+2] RO },Requirement { A[12+2] RW }}",
			toString(getDataRequirements(mgr,
				R"(
					def inc = ( r : ref<'a>, i : int<4> ) -> unit {
						// get a reference -- this does not cause a requirement
						auto ref = data_item_element_access(r,i+2,type_lit(ref<int<4>>));
						// read/write the reference
						ref = *ref + 1;
					};

					{
						inc(lit("A":ref<A>),12);
					}
				)"

			))
		);

	}

	TEST(DataRequirementAnalysis, ForLoop) {
		NodeManager mgr;

		// read from a data structure inside a for loop
		EXPECT_EQ(
			"{Requirement { A[span(0,10-1u)] RO }}",
			toString(getDataRequirements(mgr,
				R"(
					{
						for(int<4> i = 0 .. 10) {
							*data_item_element_access(lit("A":ref<A>),i,type_lit(ref<int<4>>));
						}
					}
				)"
			))
		);

		// read from a data structure inside a for loop
		EXPECT_EQ(
			"{Requirement { A[span(point(0),point(10-1u))] RO }}",
			toString(getDataRequirements(mgr,
				R"(
					{
						let point = lit("point" : (int<4>)->point);
						for(int<4> i = 0 .. 10) {
							*data_item_element_access(lit("A":ref<A>),point(i),type_lit(ref<int<4>>));
						}
					}
				)"
			))
		);

	}

	TEST(DataRequirementAnalysis, ForLoop2D) {
		NodeManager mgr;

		// read from a data structure inside a for loop
		EXPECT_EQ(
			"{Requirement { A[span(point(0, 4),point(10-1u, 12-1u))] RO }}",
			toString(getDataRequirements(mgr,
				R"(

					{
						let point = lit("point" : (int<4>,int<4>)->point);
						for(int<4> i = 0 .. 10) {
							for(int<4> j = 4 .. 12) {
								*data_item_element_access(lit("A":ref<A>),point(i,j),type_lit(ref<int<4>>));
							}
						}
					}
				)"
			))
		);

	}



	TEST(DataRequirementAnalysis, ForLoop1DStencil) {
		NodeManager mgr;

		// compute stencil with fixed boundary
		EXPECT_EQ(
			"{Requirement { A[span(10+1,20-1u+1)] RO },Requirement { A[span(10,20-1u)] RO },Requirement { A[span(10-1,20-1u-1)] RO },Requirement { B[span(10,20-1u)] RW }}",
			toString(getDataRequirements(mgr,
				R"(
					def stencil = ( A : ref<'a,f,f,cpp_ref>, B : ref<'a,f,f,cpp_ref>, a : int<4>, b : int<4> ) -> unit {
						let point = lit("point" : (int<4>,int<4>)->point);
						for(int<4> i = a .. b) {
							auto ref1 = data_item_element_access(A,i-1,type_lit(ref<int<4>>));
							auto ref2 = data_item_element_access(A, i ,type_lit(ref<int<4>>));
							auto ref3 = data_item_element_access(A,i+1,type_lit(ref<int<4>>));

							auto ref = data_item_element_access(B,i,type_lit(ref<int<4>>));

							ref = *ref1 + *ref2 + *ref3;
						}
					};

					{
						stencil(lit("A":ref<int<4>,f,f,cpp_ref>),lit("B":ref<int<4>,f,f,cpp_ref>),10,20);
					}
				)"
			))
		);


		// compute stencil with literal boundary
		auto requirement = toString(getDataRequirements(mgr,
			R"(
				def stencil = ( A : ref<'a,f,f,cpp_ref>, B : ref<'a,f,f,cpp_ref>, a : int<4>, b : int<4> ) -> unit {
					let point = lit("point" : (int<4>,int<4>)->point);
					for(int<4> i = a .. b) {
						auto ref1 = data_item_element_access(A,i-1,type_lit(ref<int<4>>));
						auto ref2 = data_item_element_access(A, i ,type_lit(ref<int<4>>));
						auto ref3 = data_item_element_access(A,i+1,type_lit(ref<int<4>>));

						auto ref = data_item_element_access(B,i,type_lit(ref<int<4>>));

						ref = *ref1 + *ref2 + *ref3;
					}
				};

				{
					stencil(lit("A":ref<int<4>,f,f,cpp_ref>),lit("B":ref<int<4>,f,f,cpp_ref>),*lit("x":ref<int<4>>),*lit("y":ref<int<4>>));
				}
			)"
		));

		EXPECT_PRED2(containsSubString, requirement, "Requirement { A[span(*x,*y-1u)] RO }");
		EXPECT_PRED2(containsSubString, requirement, "Requirement { A[span(*x-1,*y-1u-1)] RO }");
		EXPECT_PRED2(containsSubString, requirement, "Requirement { A[span(*x+1,*y-1u+1)] RO }");
		EXPECT_PRED2(containsSubString, requirement, "Requirement { B[span(*x,*y-1u)] RW }");

		// compute stencil with literal boundary and local variables
		requirement = toString(getDataRequirements(mgr,
			R"(
				def stencil = ( A : ref<'a,f,f,cpp_ref>, B : ref<'a,f,f,cpp_ref>, a : int<4>, b : int<4> ) -> unit {
					let point = lit("point" : (int<4>,int<4>)->point);
					for(int<4> i = a .. b) {
						auto ref1 = data_item_element_access(A,i-1,type_lit(ref<int<4>>));
						auto ref2 = data_item_element_access(A, i ,type_lit(ref<int<4>>));
						auto ref3 = data_item_element_access(A,i+1,type_lit(ref<int<4>>));

						var ref<int<4>> sum = 0; 
						sum = *ref1 + *ref2 + *ref3;

						auto ref = data_item_element_access(B,i,type_lit(ref<int<4>>));
						ref = *sum / 3;
					}
				};

				{
					stencil(lit("A":ref<int<4>,f,f,cpp_ref>),lit("B":ref<int<4>,f,f,cpp_ref>),*lit("x":ref<int<4>>),*lit("y":ref<int<4>>));
				}
			)"
		));

		EXPECT_PRED2(containsSubString, requirement, "Requirement { A[span(*x,*y-1u)] RO }");
		EXPECT_PRED2(containsSubString, requirement, "Requirement { A[span(*x-1,*y-1u-1)] RO }");
		EXPECT_PRED2(containsSubString, requirement, "Requirement { A[span(*x+1,*y-1u+1)] RO }");
		EXPECT_PRED2(containsSubString, requirement, "Requirement { B[span(*x,*y-1u)] RW }");

	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
