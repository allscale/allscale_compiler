#include <boost/filesystem.hpp>

#include "insieme/driver/integration/integration_tests_handler.h"
#include "insieme/driver/integration/tests.h"
#include "allscale/compiler/config.h"

int main(int argc, char** argv) {
	// we simply forward the parameters to the integration test handler, using an allscale-specific configuration
	insieme::driver::integration::IntegrationTestPaths testPaths = {
			allscale::compiler::getAllscaleTestRootDir(),
			allscale::compiler::getAllscaleBuildRootDir(),
			"allscale_integration_test_config",
			boost::filesystem::path(allscale::compiler::getAllscaleBuildRootDir()) / "integration-testdirs",
	};
	return insieme::driver::integration::handleIntegrationTests(argc, argv, "AllScale Integration Test Driver", allscale::compiler::getVersion(),
	                                                            testPaths);
}
