#include <cstdlib>
#include <iostream>
#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/utils/timer.h"

#include "insieme/core/checks/ir_checks.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/cmd/common_options.h"
#include "insieme/driver/utils/object_file_utils.h"
#include "insieme/driver/utils/driver_utils.h"

#include "allscale/compiler/config.h"
#include "allscale/compiler/env_vars.h"
#include "allscale/compiler/frontend/allscale_frontend.h"
#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/analysis/diagnostics.h"
#include "allscale/compiler/backend/allscale_backend.h"

namespace driver = insieme::driver;
namespace core = insieme::core;

int main(int argc, char** argv) {
	std::cout << "Allscale compiler - Version: " << allscale::compiler::getVersion() << std::endl;

	// -------------- options ---------------

	// object holding common command line options
	driver::cmd::CommonOptions commonOptions;

	// the optimization level
	unsigned opt_level;

	// This option is here only for compatibility reasons, because insiemecc accepts this flag and the integration test driver sets it when executing
	// insiemecc and/or allscalecc. We simply ignore this flag here and print a warning.
	std::string backendString;

	// -------------- processing ---------------

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	// register common options and flags needed by more then one driver
	commonOptions.addFlagsAndParameters(parser);

	// register allscalecc specific flags and parameters
	parser.addParameter(",O",       opt_level,     0u,              "optimization level");
	parser.addParameter("backend", backendString, std::string(""), "backend selection (for compatibility reasons - ignored)");
	auto options = parser.parse(argc, argv);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }

	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	if(!backendString.empty()) {
		std::cout << "WARNING: The --backend option has been specified. this option is supported only for compatibility reasons and will be ignored." << std::endl;
	}


	// Step 2: filter input files
	core::NodeManager mgr;
	if(!driver::utils::filterInputFiles(mgr, options.job)) {
		return 1;
	}

	// configure for AllScale
	allscale::compiler::frontend::configureConversionJob(options.job);

	// add appropriate macros depending on optimization level 
	// TODO: invoke backend compiler and retrieve macro definitions?
	if(opt_level != 0) {
		options.job.setDefinition("__OPTIMIZE__", "1");
	}

	// update file name extension of building a lib file
	bool createSharedObject = boost::filesystem::extension(commonOptions.outFile) == ".so";

	// if it is compile only or if it should become an object file => save it
	if(commonOptions.compileOnly || createSharedObject) {
		auto res = options.job.toIRTranslationUnit(mgr);
		std::cout << "Saving object file ...\n";
		driver::utils::saveLib(res, commonOptions.outFile);
		return driver::utils::isInsiemeLib(commonOptions.outFile) ? 0 : 1;
	}

	// Step 3: load input code
	insieme::utils::Timer timer;
	std::cout << "Parsing input files ... " << std::flush;
	auto program = options.job.execute(mgr);
	std::cout << timer.step() << "s\n";

	// dump IR code
	if(!commonOptions.dumpIR.empty()) {
		std::cout << "Dumping intermediate representation ... " << std::flush;
		std::ofstream out(commonOptions.dumpIR.string());
		out << core::printer::PrettyPrinter(program, core::printer::PrettyPrinter::PRINT_DEREFS);
		std::cout << timer.step() << "s\n";
	}

	// perform semantic checks - also including the AllScale specific checks
	if(commonOptions.checkSema || commonOptions.checkSemaOnly) {

		// run semantic checks
		std::cout << "Running semantic checks ... " << std::flush;
		auto errors = insieme::core::checks::check(program, allscale::compiler::checks::getFullCheck());
		std::cout << timer.step() << "s\n";

		// print errors if some have been found
		if (!errors.empty()) {
			// print errors
			for(const auto& cur : errors.getAll()) {
				std::cout << cur << "\n";
			}
			// print summary
			std::cout << "\n";
			std::cout << "Total number of errors:   " << errors.getErrors().size() << "\n";
			std::cout << "Total number of warnings: " << errors.getWarnings().size() << "\n";

			// fail compilation
			std::cout << "\nErrors encountered, compilation process terminated.\n";
			return 1;
		}

		// end program if this is all that is requested
		if(commonOptions.checkSemaOnly) { return errors.empty() ? 0 : 1; }
	}

	// run diagnostics analysis
	/*{
		std::cout << "Running diagnostics ... " << std::flush;
		auto issues = allscale::compiler::analysis::runDiagnostics(core::NodeAddress(program));
		if(issues.empty()) {
			std::cout << "done\n";
		} else {
			std::cout << "\n";
			bool printNodeAddresses = std::getenv(ALLSCALE_DIAG_NODE_ADDRESSES);
			for(const auto& issue : issues) {
				allscale::compiler::analysis::prettyPrintIssue(std::cout, issue, options.settings.noColor, printNodeAddresses);
			}
		}
	}*/

	// Step 4: convert src file to target code
	std::cout << "Producing target code ... " << std::flush;
	auto code = allscale::compiler::backend::convert(program);
	std::cout << timer.step() << "s\n";

	// dump source file if requested, exit if requested
	insieme::frontend::path filePath = commonOptions.dumpTRG;
	if(!commonOptions.dumpTRGOnly.empty()) { filePath = commonOptions.dumpTRGOnly; }
	if(!filePath.empty()) {
		std::cout << "Dumping target code ... " << std::flush;
		std::ofstream out(filePath.string());
		out << *code;
		std::cout << timer.step() << "s\n";
		if(!commonOptions.dumpTRGOnly.empty()) { return 0; }
	}

	// Step 5: built the resulting binary
	std::cout << "Compiling target code (-O" << opt_level << ") ... " << std::flush;
	auto ok = allscale::compiler::backend::compileTo(code, commonOptions.outFile.string(), opt_level);
	std::cout << timer.step() << "s\n";

	// return result
	return (ok)?0:1;
}
