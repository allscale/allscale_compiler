#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_extension.h"

#include "insieme/core/ir_builder.h"


namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::core;

	using std::string;

	TEST(AllScaleExtension, Basic) {
		NodeManager mgr;
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

		EXPECT_TRUE(ext.getDataItemRefGen());
	}

	TEST(AllScaleExtension, IsDataItemReference) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

		// some positive cases
		EXPECT_PRED1(isDataItemReference, ext.getDataItemRefGen());
		EXPECT_PRED1(isDataItemReference, builder.parseType("art_data_item_ref<int<4>>"));
		EXPECT_PRED1(isDataItemReference, getDataItemReferenceType(builder.parseType("int<4>")));
		EXPECT_PRED1(isDataItemReference, builder.parseExpr("lit(\"A\":art_data_item_ref<int<4>>)"));


		// some negative cases
		EXPECT_FALSE(isDataItemReference(builder.parseType("dummy")));
		EXPECT_FALSE(isDataItemReference(builder.parseType("art_data_item_ref")));
	}

	TEST(AllScaleExtension, GetReferencedDataItemType) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some positive cases
		EXPECT_EQ(builder.parseType("int<4>"),getReferencedDataItemType(builder.parseType("art_data_item_ref<int<4>>")));
		EXPECT_EQ(builder.parseType("bla"),getReferencedDataItemType(builder.parseType("art_data_item_ref<bla>")));
		EXPECT_EQ(builder.parseType("int<4>"), getReferencedDataItemType(getDataItemReferenceType(builder.parseType("int<4>"))));

		// some negative cases
		EXPECT_FALSE(getReferencedDataItemType(builder.parseType("dummy")));
	}


	TEST(AllScaleExtension, IsDataItemRequirement) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

		// some positive cases
		EXPECT_PRED1(isDataItemRequirement, ext.getDataItemRequirementGen());
		EXPECT_PRED1(isDataItemRequirement, builder.parseType("art_data_item_requirement<int<4>>"));
		EXPECT_PRED1(isDataItemRequirement, getDataItemRequirementType(builder.parseType("int<4>")));
		EXPECT_PRED1(isDataItemRequirement, builder.parseExpr("lit(\"A\":art_data_item_requirement<int<4>>)"));

		// some negative cases
		EXPECT_FALSE(isDataItemRequirement(builder.parseType("dummy")));
		EXPECT_FALSE(isDataItemRequirement(builder.parseType("art_data_item_requirement")));
	}

	TEST(AllScaleExtension, GetRequiredDataItemType) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// some positive cases
		EXPECT_EQ(builder.parseType("int<4>"), getRequiredDataItemType(builder.parseType("art_data_item_requirement<int<4>>")));
		EXPECT_EQ(builder.parseType("bla"),    getRequiredDataItemType(builder.parseType("art_data_item_requirement<bla>")));
		EXPECT_EQ(builder.parseType("int<4>"), getRequiredDataItemType(getDataItemRequirementType(builder.parseType("int<4>"))));

		// some negative cases
		EXPECT_FALSE(getRequiredDataItemType(builder.parseType("dummy")));
	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
