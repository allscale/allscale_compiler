#include <cstdlib>
#include <iostream>

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/utils/object_file_utils.h"
#include "insieme/frontend/extensions/interceptor_extension.h"

#include "allscale/compiler/frontend/allscale_fe_extension.h"
#include "allscale/compiler/backend/allscale_backend.h"

namespace driver = insieme::driver;
namespace core = insieme::core;

int main(int argc, char** argv) {
	std::cout << "Allscale compiler - Version: epsilon\n";

	// the target file
	std::string target;
	int opt_level;

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	parser.addParameter<std::string>("o",target,"a.out","the target file");
	parser.addParameter<int>("O",opt_level,0,"optimization level");
	auto options = parser.parse(argc, argv);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }
	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	// Step 2: filter input files
	core::NodeManager mgr;
	if(!driver::utils::filterInputFiles(mgr, options.job)) {
		return 1;
	}

	// Step 3: load input code
	std::cout << "Extracting executable ...\n";
	options.job.registerFrontendExtension<allscale::compiler::frontend::AllscaleExtension, insieme::frontend::extensions::InterceptorExtension>();

	// convert src file to target code
	auto program = options.job.execute(mgr);

	// Step 4: produce target code and build binary
	auto ok = allscale::compiler::backend::compileTo(program, target, opt_level < 3);

	// return result
	return (ok)?0:1;
}
