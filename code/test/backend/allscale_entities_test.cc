#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_runtime_entities.h"

#include "insieme/core/ir_builder.h"


namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::core;

	using std::string;


	TEST(RuntimeEntities, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto t_int  = builder.parseType("int<4>");
		auto t_bool = builder.parseType("bool");
		auto t_cls  = builder.tupleType({ t_int });


		// create a work item variants

		WorkItemVariant versionA(
				builder.parseExpr(
						"( p : cpp_ref<(int<4>),t,f> )->treeture<bool,t> { return p.0 > 0; }"
				).as<LambdaExprPtr>()
		);

		WorkItemVariant versionB(
				builder.parseExpr(
						"( p : cpp_ref<(int<4>),t,f> )->treeture<bool,t> { return p.0 + 1 > 1; }"
				).as<LambdaExprPtr>()
		);

		// check proper access to closure and result types
		EXPECT_EQ(t_cls, versionA.getClosureType());
		EXPECT_EQ(t_cls, versionB.getClosureType());

		EXPECT_EQ(t_bool, versionA.getResultType());
		EXPECT_EQ(t_bool, versionB.getResultType());


		// create a work item description
		WorkItemDescription desc(
				"test", versionA, versionB
		);

		EXPECT_EQ(t_cls, desc.getClosureType());
		EXPECT_EQ(t_bool, desc.getResultType());

	}

	TEST(RuntimeEntities, WorkItemDescriptionEncoding) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto t_int  = builder.parseType("int<4>");
		auto t_bool = builder.parseType("bool");
		auto t_cls  = builder.tupleType({ t_int });

		// create a work item variant
		WorkItemVariant variant (
				builder.parseExpr(
						"( p : cpp_ref<(int<4>),t,f> )->treeture<bool,t> { return p.0 > 0; }"
				).as<LambdaExprPtr>()
		);

		// create a work item description
		WorkItemDescription desc(
				"test", variant, variant
		);

		// convert work item description into IR
		ExpressionPtr encoded = encoder::toIR(mgr, desc);
		ASSERT_TRUE(encoded);

		// see whether it can be properly converted back
		auto decoded = encoder::toValue<WorkItemDescription>(encoded);
		EXPECT_EQ(desc,decoded);

		// check the type of the encoded version
		EXPECT_EQ("art_wi_desc<(int<4>),bool>",toString(*encoded->getType()));

	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
