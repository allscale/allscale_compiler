
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

	TEST(AllScaleDiSemaConversionTest, Templated) {
		runAllscaleTestOn("/inputs/allscale_di_sema_templated.cpp", true);
	}

	TEST(AllScaleDiSemaConversionTest, WithCleanups) {
		runAllscaleTestOn("/inputs/allscale_di_sema_with_cleanups.cpp", true);
	}

	TEST(AllScaleDiSemaConversionTest, ApiStaticGrid) {
		runAllscaleTestOn("/inputs/allscale_di_sema_api_static_grid.cpp", true);
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
