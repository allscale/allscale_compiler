#include <cstdlib>
#include <iostream>
#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/utils/timer.h"

#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/dump/json_dump.h"
#include "insieme/core/dump/binary_haskell.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/cmd/common_options.h"
#include "insieme/driver/utils/object_file_utils.h"
#include "insieme/driver/utils/driver_utils.h"

#include "allscale/compiler/config.h"
#include "allscale/compiler/env_vars.h"

#include "allscale/compiler/frontend/allscale_frontend.h"
#include "allscale/compiler/core/allscale_core.h"
#include "allscale/compiler/backend/allscale_backend.h"

#include "allscale/compiler/checks/allscale_checks.h"
//#include "allscale/compiler/analysis/diagnostics.h"

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

	// Allows the AllScale driver to dump an input code as JSON/binary IR.
	insieme::frontend::path dumpJSON, dumpBinaryIR;

	allscale::compiler::core::ConversionConfig conversionConfig;

	// If set, analysis failures will be ignored and target code will be anyway created
	bool ignoreAnalysisFailures = false;

	// -------------- processing ---------------

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	// register common options and flags needed by more then one driver
	commonOptions.addFlagsAndParameters(parser);

	// register allscalecc specific flags and parameters
	parser.addParameter(",O",        opt_level,     0u,                                     "optimization level");
	parser.addParameter("dump-json", dumpJSON,      insieme::frontend::path(),              "dump intermediate representation (JSON)");
	parser.addParameter("dump-binary-ir", dumpBinaryIR, insieme::frontend::path(),          "dump intermediate representation (binary)");
	parser.addParameter("backend",   backendString, std::string(""),                        "backend selection (for compatibility reasons - ignored)");
	parser.addFlag("check-data-item-accesses",      conversionConfig.checkDataItemAccesses, "enables data item access instrumentation (debugging)");
	parser.addFlag("ignore-analysis-failure",       ignoreAnalysisFailures,                 "ignore analysis failures and generate code anyway");
	parser.addFlag("allow-sm-only",                 conversionConfig.allowSharedMemoryOnly, "only create shared memory conversion variant");
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

	if(!dumpBinaryIR.empty()) {
		vector<core::NodeAddress> targets;
		core::NodePtr ref_deref = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefDeref();
		core::NodePtr ref_assign = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefAssign();

		core::visitDepthFirst(core::NodeAddress(program), [&](const core::CallExprAddress& call) {
				auto fun = call->getFunctionExpr();
				if(*fun == *ref_deref || *fun == *ref_assign) {
					targets.push_back(call[0]);
				}
			});

		std::ofstream out(dumpBinaryIR.string());
		core::dump::binary::haskell::dumpAddresses(out, targets);
	}

	// dump JSON IR representation
	if(!dumpJSON.empty()) {
		std::cout << "Dumping JSON representation ...\n";
		std::ofstream out(dumpJSON.string());
		core::dump::json::dumpIR(out, program);
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


	// Step 4: apply source-to-source conversions
	std::cout << "Converting to AllScale Runtime code ... \n" << std::flush;
	auto summary = allscale::compiler::core::convert(conversionConfig,program,[&](const allscale::compiler::core::ProgressUpdate& update){
		std::cout << "\t" << update.msg;
		if (update.totalSteps > 0) {
			std::cout << " ("<< update.completedSteps << "/" << update.totalSteps << ")";
		}
		std::cout << std::endl;
	});

	// print brief reporting
	std::cout << summary.report << std::endl;

	// create HTML report
	summary.report.toHTML("report.html");

	// abort if not successful so far
	if (!summary.successful()) {
		if (!ignoreAnalysisFailures) {
			std::cout << "Conversion aborted due to errors (see report, use --ignore-analysis-failure to force code generation).\n";
			return EXIT_FAILURE;
		}
		std::cout << "Errors encountered, yet code generation enforced (--ignore-analysis-failure).\n";
	}

	// extract the converted program
	assert(summary.result);
	program = summary.result.as<insieme::core::ProgramPtr>();

	// report time usage
	std::cout << "Converted to AllScale Runtime code in " << timer.step() << "s\n";

	// Step 5: convert transformed IR to target code
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

	// Step 6: built the resulting binary
	std::cout << "Compiling target code (-O" << opt_level << ") ... " << std::flush;
	allscale::compiler::backend::CompilerConfig compilerConfig;
	compilerConfig.optimization_level = opt_level;
	compilerConfig.checkDataItemAccesses = conversionConfig.checkDataItemAccesses;
	auto ok = allscale::compiler::backend::compileTo(code, commonOptions.outFile.string(), compilerConfig);
	std::cout << timer.step() << "s\n";

	// return result
	return (ok)?0:1;
}
