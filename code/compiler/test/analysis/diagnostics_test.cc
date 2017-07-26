#include <gtest/gtest.h>

#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>

#include "allscale/compiler/analysis/diagnostics.h"
#include "allscale/compiler/checks/allscale_checks.h"
#include "allscale/compiler/config.h"
#include "allscale/compiler/frontend/allscale_frontend.h"

#include "insieme/core/printer/error_printer.h"

#include "insieme/driver/cmd/commandline_options.h"

#include "insieme/utils/gtest_utils.h"
#include "insieme/utils/string_utils.h"

using namespace insieme::analysis::cba::haskell;
using namespace insieme::core;
using namespace insieme::utils;

namespace fs = boost::filesystem;

namespace allscale {
namespace compiler {
namespace analysis {

	const auto INPUT_TESTS_DIR = getAllscaleSourceRootDir() + "compiler/test/analysis/diagnostics_input_tests/";

	class DiagnosticsInputTest : public ::testing::TestWithParam<std::string> {};

	TEST_P(DiagnosticsInputTest, Test) {
		auto input_filepath = INPUT_TESTS_DIR + GetParam();
		auto input_diag_filepath = input_filepath + ".diag";
		SCOPED_TRACE(input_filepath);

		ASSERT_TRUE(fs::exists(input_filepath));

		std::vector<std::string> argv = {"compiler", input_filepath};
		if(boost::ends_with(input_filepath, ".cpp")) {
			argv.push_back("--std=c++14");
		}

		auto options = insieme::driver::cmd::Options::parse(argv);
		options.job.addIncludeDirectory(INPUT_TESTS_DIR);

		// configure for AllScale
		frontend::configureConversionJob(options.job);

		NodeManager mgr;
		auto program = options.job.execute(mgr);

		// semantic checks
		auto errors = insieme::core::checks::check(program, allscale::compiler::checks::getFullCheck());
		ASSERT_TRUE(errors.empty()) << errors << "\n------\n" << printer::dumpErrors(errors);

		auto issues = runDiagnostics(NodeAddress(program));

		std::stringstream diag_output;
		for(const auto& issue : issues) {
			prettyPrintIssue(diag_output, issue, true);
		}

		// PRINT DIAGNOSTICS OUTPUT
		//{
		//	auto out = diag_output.str();
		//	boost::replace_all(out, input_filepath, "<input_filepath>");
		//	std::cout << out << std::flush;
		//}

		std::ifstream expected_diag_output(input_diag_filepath);
		ASSERT_TRUE(expected_diag_output.good()) << "could not open file containing expected diagnostics output";

		std::size_t line_counter = 1;
		while(!expected_diag_output.eof()) {
			std::string line, expected_line;
			std::getline(diag_output, line);
			std::getline(expected_diag_output, expected_line);

			// strip cr
			boost::replace_all(line,          "\r", "");
			boost::replace_all(expected_line, "\r", "");

			// expand tabs
			boost::replace_all(line,          "\t", "        ");
			boost::replace_all(expected_line, "\t", "        ");

			// expand input_filepath placeholder
			boost::replace_all(expected_line, "<input_filepath>", input_filepath);

			ASSERT_EQ(expected_line, line) << "line " << line_counter;
			line_counter++;
		}
		ASSERT_TRUE(diag_output.eof()) << "double check expected diag output for trailing newline";
	}

	std::vector<std::string> getFilenames() {
		fs::path root(INPUT_TESTS_DIR);
		assert_true(fs::is_directory(root));

		std::vector<std::string> input_files;
		for(const auto& entry : boost::make_iterator_range(fs::recursive_directory_iterator(root), {})) {
			if(containsSubString(entry.path().string(), "disabled_")) {
				continue;
			}

			auto ext = entry.path().extension().string();
			if(ext != ".c" && ext != ".cpp") {
				continue;
			}

			auto path = entry.path().string().substr(INPUT_TESTS_DIR.size());
			input_files.push_back(path);
		}

		std::sort(input_files.begin(), input_files.end());
		return input_files;
	}

	INSTANTIATE_TEST_CASE_P(InputFileChecks, DiagnosticsInputTest, ::testing::ValuesIn(getFilenames()), TestCaseNamePrinter());

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
