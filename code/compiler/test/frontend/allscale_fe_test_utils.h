#pragma once

namespace allscale {
namespace compiler {
namespace frontend {

#define ALLSCALE_ROOT_DIR (insieme::utils::getInsiemeSourceRootDir() + "../../")
#define ALLSCALE_FRONTEND_TEST_DIR (ALLSCALE_ROOT_DIR + "code/compiler/test/frontend/")

	static inline void runAllscaleTestOn(const string& filename, bool intercept = false) {
		insieme::frontend::utils::runConversionTestOn(ALLSCALE_FRONTEND_TEST_DIR + filename, [intercept](insieme::frontend::ConversionJob& job) {
			configureConversionJob(job);
			if(intercept) {
				job.addIncludeDirectory(ALLSCALE_FRONTEND_TEST_DIR + "inputs/");
				job.addInterceptedHeaderDir(ALLSCALE_FRONTEND_TEST_DIR + "inputs/intercepted/");
			}
		}, [](insieme::core::NodeManager& mgr, insieme::core::lang::symbol_map& symbols) {
			auto allscaleSymbols = mgr.getLangExtension<compiler::lang::AllscaleModule>().getSymbols();
			symbols.insert(allscaleSymbols.begin(), allscaleSymbols.end());
		}, [](const insieme::core::NodePtr& node) { return allscale::compiler::checks::check(node); });
	}

#undef ALLSCALE_ROOT_DIR
#undef ALLSCALE_FRONTEND_TEST_DIR

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
