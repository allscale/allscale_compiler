
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

	TEST(AllScaleConversionTest, Basic) {
		runAllscaleTestOn("/inputs/allscale_basic.cpp");
	}

	TEST(AllScaleConversionTest, BasicNonTrivial) {
		runAllscaleTestOn("/inputs/allscale_basic_non_trivial.cpp");
	}

	TEST(AllScaleConversionTest, CtorInitExprs) {
		runAllscaleTestOn("/inputs/allscale_ctor_init_exprs.cpp");
	}

	TEST(AllScaleConversionTest, DependencyOps) {
		runAllscaleTestOn("/inputs/allscale_dependency_ops.cpp");
	}

	TEST(AllScaleConversionTest, Fib) {
		runAllscaleTestOn("/inputs/allscale_fib.cpp");
	}

	TEST(AllScaleConversionTest, FunOps) {
		runAllscaleTestOn("/inputs/allscale_fun_ops.cpp");
	}

	TEST(AllScaleConversionTest, Members) {
		runAllscaleTestOn("/inputs/allscale_members.cpp");
	}

	TEST(AllScaleConversionTest, Pfor) {
		runAllscaleTestOn("/inputs/allscale_pfor.cpp");
	}

	TEST(AllScaleConversionTest, Pick) {
		runAllscaleTestOn("/inputs/allscale_pick.cpp");
	}

	TEST(AllScaleConversionTest, PrecDeps) {
		runAllscaleTestOn("/inputs/allscale_prec_deps.cpp");
	}

	TEST(AllScaleConversionTest, PrecNonTreetureStep) {
		runAllscaleTestOn("/inputs/allscale_prec_non_treeture_step.cpp");
	}

	TEST(AllScaleConversionTest, PrecVariants) {
		runAllscaleTestOn("/inputs/allscale_prec_variants.cpp");
	}

	TEST(AllScaleConversionTest, PrecVoid) {
		runAllscaleTestOn("/inputs/allscale_prec_void.cpp");
	}

	TEST(AllScaleConversionTest, TaskRefOps) {
		runAllscaleTestOn("/inputs/allscale_task_ref_ops.cpp");
	}

	TEST(AllScaleConversionTest, TreetureOps) {
		runAllscaleTestOn("/inputs/allscale_treeture_ops.cpp");
	}

	TEST(AllScaleConversionTest, Types) {
		runAllscaleTestOn("/inputs/allscale_types.cpp");
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
