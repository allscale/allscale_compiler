
#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"

#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"

#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/frontend/allscale_fe_extension.h"

namespace allscale {
namespace compiler {
namespace frontend {

#define ALLSCALE_ROOT_DIR insieme::utils::getInsiemeSourceRootDir() + "../../"
#define ALLSCALE_FRONTEND_TEST_DIR ALLSCALE_ROOT_DIR + "code/test/frontend/"

	namespace {
		void runAllscaleTestOn(const string& filename) {
			insieme::frontend::utils::runConversionTestOn(filename, [](insieme::frontend::ConversionJob& job) {
				auto apiIncludeDir = std::string(ALLSCALE_ROOT_DIR) + "api/code/include/";
				job.addIncludeDirectory(apiIncludeDir);
				job.addInterceptedHeaderDir(apiIncludeDir + "allscale/api/core/"); // we are intercepting the core library only
				job.setStandard(insieme::frontend::ConversionSetup::Standard::Cxx14);
				job.registerFrontendExtension<insieme::frontend::extensions::InterceptorExtension, insieme::frontend::extensions::TestPragmaExtension>();
				job.registerFrontendExtension<AllscaleExtension, insieme::frontend::extensions::InterceptorExtension>();
			}, [](insieme::core::NodeManager& mgr, insieme::core::lang::symbol_map& symbols) {
				auto allscaleSymbols = mgr.getLangExtension<compiler::lang::AllscaleModule>().getSymbols();
				symbols.insert(allscaleSymbols.begin(), allscaleSymbols.end());
			}, [](const insieme::core::NodePtr& node) { return allscale::compiler::checks::check(node); });
		}
	}

	TEST(AllScaleConversionTest, Basic) {
		runAllscaleTestOn(ALLSCALE_FRONTEND_TEST_DIR + "/inputs/allscale_basic.cpp");
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
