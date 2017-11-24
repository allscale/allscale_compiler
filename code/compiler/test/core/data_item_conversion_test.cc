#include <gtest/gtest.h>

#include <algorithm>
#include <iostream>

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/dump/json_dump.h"

#include "allscale/compiler/core/data_item_annotation.h"
#include "allscale/compiler/core/data_item_conversion.h"
#include "allscale/compiler/backend/allscale_extension.h"

#include "../test_utils.inc"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	namespace {

		bool isReferenceOnDataItem(const TypePtr& type) {
			if (!lang::isReference(type)) return false;
			return isDataItem(core::analysis::getReferencedType(type));
		}

		bool isReferenceOnDataItem(const ExpressionPtr& expr) {
			return isReferenceOnDataItem(expr->getType());
		}

		// --- Data Item Variables ---

		bool checkDataItemVariables(const NodePtr& code, bool expect) {
			bool res = false;
			visitDepthFirstOnce(code,[&](const VariablePtr& var){
				// check that the variable is not of a data item type
				if (isReferenceOnDataItem(var)) {
					EXPECT_TRUE(expect) << "Variable of data item type: " << *var << " of type " << dumpOneLine(var->getType());
					res = true;
				}
			},true,true);
			return res;
		}

		bool hasDataItemVariables(const NodePtr& code) {
			return checkDataItemVariables(code,true);
		}

		bool hasNoDataItemVariables(const NodePtr& code) {
			return !checkDataItemVariables(code,false);
		}


		// --- Data Item Fields ---

		bool checkDataItemFields(const NodePtr& code, bool expect) {
			bool res = false;
			visitDepthFirstOnce(code,[&](const RecordPtr& record){

				for(const auto& cur : record->getFields()) {
					if (isDataItem(cur->getType()) || isReferenceOnDataItem(cur->getType())) {
						EXPECT_TRUE(expect) << "Field of data item type: " << *cur << " of type " << dumpOneLine(cur->getType());
						res = true;
					}
				}

			},true,true);
			return res;
		}

		bool hasDataItemFields(const NodePtr& code) {
			return checkDataItemFields(code,true);
		}

		bool hasNoDataItemFields(const NodePtr& code) {
			return !checkDataItemFields(code,false);
		}


		// --- Captured Data Item References ---

		bool containsDataItemReference(const TypePtr& type) {
			bool res = false;
			visitDepthFirstOncePrunable(type,[&](const NodePtr& node){
				// check whether it can be pruned here
				if (res || node.isa<ExpressionPtr>()) return core::Action::Prune;

				// check whether the current type is a data item reference
				if (auto type = node.isa<TypePtr>()) {
					if (backend::isDataItemReference(type)) {
						res = true;
						return core::Action::Prune;
					}
				}

				// continue
				return core::Action::Descent;
			});
			return res;
		}

		bool checkCapturedDataItemReferenceReferences(const NodePtr& code, bool expect) {
			bool res = false;
			visitDepthFirstOnce(code,[&](const FieldPtr& field){

				auto type = field->getType();
				if (core::lang::isReference(type) && containsDataItemReference(core::analysis::getReferencedType(type))) {
					EXPECT_TRUE(expect) << "Field storing reference on DataItemReference encountered: " << *field->getName() << " of type " << dumpOneLine(type);
					res = true;
				}

			},true,true);
			return res;
		}

		bool hasCapturedDataItemReferenceReferences(const NodePtr& code) {
			return checkCapturedDataItemReferenceReferences(code,true);
		}

		bool hasNoCapturedDataItemReferenceReferences(const NodePtr& code) {
			return !checkCapturedDataItemReferenceReferences(code,false);
		}
	}


	TEST(DataItemConversionTest, VariableScalar) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/user/data/scalar.h"

			using namespace allscale::api::user::data;

			int main()
			{
				Scalar<int> x;
				x.get();
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		// the input code should have variables of a data item type
		EXPECT_TRUE(hasDataItemVariables(prog));

		// apply conversion to be tested
		auto res = convertDataItemReferences(prog);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item variables any more
		EXPECT_TRUE(hasNoDataItemVariables(res));
	}

	TEST(DataItemConversionTest, VariableGrid) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/user/data/grid.h"

			using namespace allscale::api::user::data;

			int main()
			{
				Grid<int,2> x({10,20});
				x[{1,2}];
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		// the input code should have variables of a data item type
		EXPECT_TRUE(hasDataItemVariables(prog));

		// apply conversion to be tested
		auto res = convertDataItemReferences(prog);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item variables any more
		EXPECT_TRUE(hasNoDataItemVariables(res));
	}


	TEST(DataItemConversionTest, StructMemberScalar) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/user/data/static_grid.h"

			using namespace allscale::api::user::data;

			struct Universe {
				StaticGrid<int,10,20> prop;
			};

			int main()
			{
				Universe u;
				u.prop[{1,2}];
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		// the input code should have data item fields
		EXPECT_TRUE(hasNoDataItemVariables(prog));
		EXPECT_TRUE(hasDataItemFields(prog));

		// apply conversion to be tested
		auto res = convertDataItemReferences(prog);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item fields any more
		EXPECT_TRUE(hasNoDataItemVariables(res));
		EXPECT_TRUE(hasNoDataItemFields(res));
	}



	TEST(DataItemConversionTest, CaptureByReferenceScalar) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/user/algorithm/async.h"
			#include "allscale/api/user/data/scalar.h"

			using namespace allscale::api::user::algorithm;
			using namespace allscale::api::user::data;

			int main()
			{
				Scalar<int> x;
				async([&](){
					x.get();
				});
			}
		)";

		auto prog = frontend::parseCode(mgr,code);

		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		// the input code should have data item fields
		EXPECT_TRUE(hasDataItemVariables(prog));
		EXPECT_TRUE(hasDataItemFields(prog));
		EXPECT_TRUE(hasNoCapturedDataItemReferenceReferences(prog));

		// apply conversion to be tested
		auto res = convertDataItemReferences(prog);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item fields any more
		EXPECT_TRUE(hasNoDataItemVariables(res));
		EXPECT_TRUE(hasNoDataItemFields(res));
		EXPECT_TRUE(hasCapturedDataItemReferenceReferences(res));

		// perform second transformation
		res = convertCapturedDataItemReferences(res);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item fields any more
		EXPECT_TRUE(hasNoDataItemVariables(res));
		EXPECT_TRUE(hasNoDataItemFields(res));
		EXPECT_TRUE(hasNoCapturedDataItemReferenceReferences(res));

	}




	TEST(DataItemConversionTest, CaptureByReferenceUniverse) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/user/algorithm/async.h"
			#include "allscale/api/user/data/static_grid.h"

			using namespace allscale::api::user::algorithm;
			using namespace allscale::api::user::data;

			struct Universe {
				StaticGrid<int,10,20> prop;
			};

			int main()
			{
				Universe u;
				async([&](){
					u.prop[{1,2}];
				});
			}
		)";

		auto prog = frontend::parseCode(mgr,code);

		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		// the input code should have data item fields
		EXPECT_TRUE(hasDataItemFields(prog));
		EXPECT_TRUE(hasNoCapturedDataItemReferenceReferences(prog));

		// apply conversion to be tested
		auto res = convertDataItemReferences(prog);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item fields any more
		EXPECT_TRUE(hasNoDataItemFields(res));
		EXPECT_TRUE(hasCapturedDataItemReferenceReferences(res));

		// perform second transformation
		res = convertCapturedDataItemReferences(res);

		// check that it does not contain errors
		EXPECT_TRUE(checks::check(res).empty()) << printer::dumpErrors(checks::check(res));

		// the result should not have any data item fields any more
		EXPECT_TRUE(hasNoDataItemFields(res));
		EXPECT_TRUE(hasNoCapturedDataItemReferenceReferences(res));

	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
