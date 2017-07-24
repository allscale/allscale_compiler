#include <gtest/gtest.h>

#include <vector>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/gtest_utils.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/dump/json_dump.h"

#include "allscale/compiler/analysis/data_requirement.h"
#include "allscale/compiler/frontend/allscale_frontend.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/config.h"

namespace allscale {
namespace compiler {
namespace analysis {

	using namespace std;
	using namespace insieme::core;

	namespace fs = boost::filesystem;

	// the directory to load input files from
	const auto ROOT_DIR = getAllscaleSourceRootDir() + "compiler/test/analysis/input_tests/";

	// the type definition (specifying the parameter type)
	class DataRequirementInputTest : public ::testing::TestWithParam<std::string> {};

	// define the test case pattern
	TEST_P(DataRequirementInputTest, OverallTest) {
		string filename = GetParam();
		string file = ROOT_DIR + filename;

		SCOPED_TRACE(file);

		// check whether file is present
		EXPECT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		ASSERT_TRUE(fs::exists(file));
		std::cout << "Loading: " << file << "... " << std::flush;

		// set up converson job
		insieme::frontend::ConversionJob job(file);
		frontend::configureConversionJob(job);

		job.addIncludeDirectory(ROOT_DIR);
		job.addInterceptedHeaderDir(ROOT_DIR + "intercepted/");

		// load file using the frontend
		NodeManager mgr;
		auto prog = job.execute(mgr);
		std::cout << "done" << std::endl;

		// running semantic checks
		auto res = core::checks::check(prog);
		EXPECT_TRUE(res.empty()) << res << "\n------\n" << printer::dumpErrors(res);

		// create a CBA analysis context
		insieme::analysis::cba::haskell::Context ctxt;

		// a utility to retrieve an instructions context
		auto getScope = [](const NodeAddress& addr) {
			auto compound = addr.getParentNode().isa<core::CompoundStmtPtr>();
			EXPECT_TRUE(compound) << "Context has to be a compound statement!\n";
			return compound;
		};

		// run CBA analysis
		int testCount = 0;
		visitDepthFirst(NodeAddress(prog), [&](const CallExprAddress& call) {

			// only interested in literal calls
			auto fun = call->getFunctionExpr();
			if (!fun.isa<LiteralPtr>() && !fun.isa<LambdaExprPtr>()) return;

			const string& name = (fun.isa<LiteralPtr>()) ?
					insieme::utils::demangle(fun.as<LiteralPtr>()->getStringValue()) :
					insieme::utils::demangle(fun.as<LambdaExprPtr>()->getReference()->getNameAsString()) ;

			// check prefix of literal
			if (!boost::starts_with(name, "cba_")) return;

			// check the predicate
			testCount++;

			// data requirement analysis
			if (name == "cba_expect_data_requirements") {
				auto compound = getScope(call);

				// obtain expected value form user code
				auto arg = call->getArgument(0).isa<CallExprPtr>();
				assert_true(arg) << "Expected value is not a string literal!" << std::endl
					<< *core::annotations::getLocation(call) << std::endl;

				auto lit = arg->getArgument(0).isa<LiteralPtr>();
				assert_true(arg) << "Expected value is not a string literal!" << std::endl
					<< *core::annotations::getLocation(call) << std::endl;

				auto expected = lit->getStringValue();
				expected = expected.substr(1,expected.size()-2);

				// for now, we have to create a temporary context
				ctxt = std::move(insieme::analysis::cba::haskell::Context());
				auto requirements = getDataRequirements(ctxt,compound);

				// TODO: check the actual value
				std::cout << "Expected requirements:   " << expected << "\n";
				std::cout << "Identified requirements: " << requirements << "\n";
//				EXPECT_EQ(expected,toString(requirements))
//					<< *core::annotations::getLocation(call) << std::endl;

				EXPECT_FALSE(requirements.isUniverse())
					<< *core::annotations::getLocation(call) << std::endl;

			// debugging
			} else if (name == "cba_print_code") {
				// just dump the code
				dumpReadable(getScope(call));

			} else if (name == "cba_dump_json") {
				// dump the code as a json file
				core::dump::json::dumpIR("code.json", getScope(call));

			} else if (name == "cba_dump_statistic") {
				// dump the current statistic
				ctxt.dumpStatistics();

			} else if (name == "cba_dump_solution") {
				// dump the current solution
				ctxt.dumpSolution();

				// dump the code as a json file (as it is required by inspyer)
				core::dump::json::dumpIR("code.json", getScope(call));

			// the rest
			} else {
				FAIL() << "Unsupported CBA expectation predicate: " << name << " - " << *core::annotations::getLocation(call);
			}
		});

		EXPECT_TRUE(testCount > 0) << "No tests encountered within file " << file;

	}



	// ------- utilities -----------

	namespace {

		void collectFiles(const fs::path& dir, const std::string& prefix, std::vector<string>& res) {

			fs::path root(dir);
			assert_true(fs::is_directory(root));

			for(auto it = fs::directory_iterator(root); it != fs::directory_iterator(); ++it) {
				fs::path file = it->path();
				// collect c files
				auto ext = file.extension().string();
				if (ext == ".c" || ext == ".cpp") {
					res.push_back(prefix + file.filename().string());
				}
				// collect files recursively
				if (fs::is_directory(file)) {
					const auto& name = file.filename().string();
					if (name != "_disabled") {
						collectFiles(file, prefix + name + "/", res);
					}
				}
			}

		}

	}

	/*
	 * Generate a list of configurations for the tests.
	 * This is a cross-product of the cba_tests files and the Datalog/Haskell backends
	 */
	vector<std::string> getFilenames() {
		vector<string> filenames;

		// collect input files
		collectFiles(fs::path(ROOT_DIR), "", filenames);

		// sort files
		std::sort(filenames.begin(), filenames.end());

		// done
		return filenames;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, DataRequirementInputTest, ::testing::ValuesIn(getFilenames()), insieme::utils::TestCaseNamePrinter());

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
