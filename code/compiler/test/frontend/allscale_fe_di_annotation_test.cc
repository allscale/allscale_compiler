
#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"

#include "allscale/compiler/core/data_item_annotation.h"
#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/frontend/allscale_frontend.h"

#include "allscale_fe_test_utils.h"

namespace allscale {
namespace compiler {
namespace frontend {

	TEST(AllScaleDiSemaConversionTest, Attachment) {
		runAllscaleTestOn("/inputs/allscale_di_sema_simplest.cpp", true, [](const insieme::core::NodePtr& node) {
			insieme::core::visitDepthFirstOnceInterruptible(node, [](const insieme::core::GenericTypePtr& t) {
				//std::cout << t->getName()->getValue() << std::endl;
				if(t->getName()->getValue() == "IMP_SimplestDI") {
					EXPECT_TRUE(core::isDataItem(t));
				}
				else {
					EXPECT_FALSE(core::isDataItem(t));
				}
				return false;
			}, true, true);
		});
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
