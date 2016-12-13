#include <cstdlib>
#include <iostream>
#include <fstream>

#include <boost/filesystem.hpp>

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/utils/object_file_utils.h"

#include "allscale/compiler/frontend/allscale_frontend.h"
#include "allscale/compiler/backend/allscale_backend.h"

namespace driver = insieme::driver;
namespace core = insieme::core;

int main(int argc, char** argv) {
	std::cout << "Allscale compiler - Version: epsilon\n";

	// -------------- options ---------------

	// the target file
	std::string target;

	// the target code file (if requested)
	std::string targetCode;

	// the optimization level
	unsigned opt_level;

	// a flag to indicate that it should only compile, but not link the resulting code
	bool compileOnly = false;


	// -------------- processing ---------------

	// Step 1: parse input parameters
	auto parser = driver::cmd::Options::getParser();
	parser.addFlag("compile,c", compileOnly, "compilation only");
	parser.addParameter<std::string>("o",target,"a.out","the binary target file");
	parser.addParameter<std::string>("targetcode,t", targetCode,"","file to dump target code to");
	parser.addParameter<unsigned>("O",opt_level,0,"optimization level");
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

	// configure for AllScale
	allscale::compiler::frontend::configureConversionJob(options.job);

	// update file name extension of building a lib file
	bool createSharedObject = boost::filesystem::extension(target) == ".so";

	// if it is compile only or if it should become an object file => save it
	if(compileOnly || createSharedObject) {
		auto res = options.job.toIRTranslationUnit(mgr);
		std::cout << "Saving object file ...\n";
		driver::utils::saveLib(res, target);
		return driver::utils::isInsiemeLib(target) ? 0 : 1;
	}

	// Step 3: load input code
	std::cout << "Parsing input files ...\n";
	auto program = options.job.execute(mgr);

	// Step 4: convert src file to target code
	std::cout << "Producing target code ... \n";
	auto code = allscale::compiler::backend::convert(program);

	// check whether target code is requested
	if (!targetCode.empty()) {
		std::ofstream out(targetCode);
		out << *code;
	}

	// Step 5: built the resulting binary
	std::cout << "Compiling target code (-O" << opt_level << ") ... \n";
	auto ok = allscale::compiler::backend::compileTo(code, target, opt_level);

	// return result
	return (ok)?0:1;
}
