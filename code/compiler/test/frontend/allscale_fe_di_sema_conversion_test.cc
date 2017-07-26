
#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"

#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/frontend/allscale_frontend.h"

#include "allscale_fe_test_utils.h"

namespace allscale {
namespace compiler {
namespace frontend {

	TEST(AllScaleDiSemaConversionTest, Simplest) {
		runAllscaleTestOn("/inputs/allscale_di_sema_simplest.cpp", true);
	}

	TEST(AllScaleDiSemaConversionTest, NonBaseRange) {
		runAllscaleTestOn("/inputs/allscale_di_sema_nonbaserange.cpp", true);
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
