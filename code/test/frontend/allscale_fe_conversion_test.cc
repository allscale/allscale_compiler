
#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"

#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/frontend/allscale_frontend.h"

namespace allscale {
namespace compiler {
namespace frontend {

#define ALLSCALE_ROOT_DIR insieme::utils::getInsiemeSourceRootDir() + "../../"
#define ALLSCALE_FRONTEND_TEST_DIR ALLSCALE_ROOT_DIR + "code/test/frontend/"

	namespace {
		void runAllscaleTestOn(const string& filename) {
			insieme::frontend::utils::runConversionTestOn(filename, [](insieme::frontend::ConversionJob& job) {
				configureConversionJob(job);
			}, [](insieme::core::NodeManager& mgr, insieme::core::lang::symbol_map& symbols) {
				auto allscaleSymbols = mgr.getLangExtension<compiler::lang::AllscaleModule>().getSymbols();
				symbols.insert(allscaleSymbols.begin(), allscaleSymbols.end());
			}, [](const insieme::core::NodePtr& node) { return allscale::compiler::checks::check(node); });
		}
	}

	TEST(AllScaleConversionTest, Types) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_types.cpp");
	}

	TEST(AllScaleConversionTest, Members) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_members.cpp");
	}

	TEST(AllScaleConversionTest, TreetureOps) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_treeture_ops.cpp");
	}

	TEST(AllScaleConversionTest, TaskRefOps) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_task_ref_ops.cpp");
	}

	TEST(AllScaleConversionTest, DependencyOps) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_dependency_ops.cpp");
	}

	TEST(AllScaleConversionTest, FunOps) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_fun_ops.cpp");
	}

	TEST(AllScaleConversionTest, Basic) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_basic.cpp");
	}

	TEST(AllScaleConversionTest, BasicNonTrivial) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_basic_non_trivial.cpp");
	}

	TEST(AllScaleConversionTest, PrecVariants) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_prec_variants.cpp");
	}

	TEST(AllScaleConversionTest, PrecVoid) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_prec_void.cpp");
	}

	TEST(AllScaleConversionTest, Fib) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_fib.cpp");
	}

	TEST(AllScaleConversionTest, Pick) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_pick.cpp");
	}

	TEST(AllScaleConversionTest, DISABLED_Pfor) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_pfor.cpp");
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
