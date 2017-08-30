#include "allscale/compiler/core/allscale_core.h"

#include <fstream>

#include <boost/algorithm/string/replace.hpp>
#include <boost/property_tree/json_parser.hpp>

#include "insieme/core/annotations/source_location.h"

#include "allscale/compiler/config.h"
#include "allscale/compiler/core/cpp_lambda_to_ir_conversion.h"
#include "allscale/compiler/core/data_item_conversion.h"
#include "allscale/compiler/core/prec_to_work_item_conversion.h"

namespace allscale {
namespace compiler {
namespace core {

	boost::property_tree::ptree toPropertyTree(const ConversionReport& report) {
		boost::property_tree::ptree ret;

		for(const auto& p : report.issues) {
			boost::property_tree::ptree entry;
			auto target = p.first;

			entry.push_back(make_pair("issues", toPropertyTree(p.second)));

			if(auto location = insieme::core::annotations::getLocation(target)) {
				entry.put<string>("loc_short", toString(*location));

				std::stringstream ss;
				insieme::core::annotations::prettyPrintLocation(ss, *location, true);
				entry.put<string>("loc_pretty", ss.str());
			}

			ret.push_back(make_pair(toString(target), entry));
		}

		return ret;
	}

	void toJSON(const std::string& filename, const ConversionReport& report) {
		write_json(filename, toPropertyTree(report));
	}

	void toJSON(std::ostream& out, const ConversionReport& report) {
		write_json(out, toPropertyTree(report));
	}

	void toHTML(const std::string& filename, const ConversionReport& report) {
		auto report_filename = getAllscaleSourceRootDir() + "/compiler/resources/report.html";
		std::ifstream in(report_filename);
		assert_true(in)  << "could not open template for conversion report";
		in >> std::noskipws;

		std::ofstream out(filename);
		assert_true(out) << "could not open output file for conversion report";

		std::stringstream report_buffer;
		write_json(report_buffer, toPropertyTree(report));

		while(in) {
			std::string line;
			std::getline(in, line);

			boost::replace_all(line, "%REPORT%", report_buffer.str());

			out << line << "\n";
		}
	}

	std::ostream& operator<<(std::ostream& out, const ConversionReport& report) {

		out << "\n";
		out << " ------ AllScale Code Generation Report ------\n";

		out << "  Number of processed parallel regions: " << report.issues.size() << "\n";

		out << " ---------------------------------------------\n";

		// print issues, one after another
		int counter = 0;
		for(const auto& cur : report.issues) {
			const auto& precCall = cur.first;
			const auto& issues = cur.second;

			// if there are no issues to report => skip this one
			if (issues.empty()) continue;

			out << "Processed parallel region #" << (++counter) << ":\n";
			reporting::prettyPrintLocation(out,precCall);
			std::cout << "  Messages:\n";
			for(const auto& cur : issues) {
				out << "\t" << cur << "\n";
			}
			out << "\n";
		}

		out << " ---------------------------------------------\n";

		// done
		return out;
	}

	ConversionResult convert(const insieme::core::NodePtr& code, const ProgressCallback& callback) {

		// Step 1: convert C++ lambdas to IR
		callback(ProgressUpdate("Pre-processing C++ lambdas ..."));
		auto res = convertCppLambdaToIR(code);

		// Step 2: introduce data item references
		res = convertDataItemReferences(res, callback);

		// Step 3: convert prec calls
		auto precConversionResult = convertPrecToWorkItem(res, callback);
		res = precConversionResult.result;

		// Step 4: convert the entry point into a work item
		// TODO: move this step from the backend to the core

		// Step 5: add default constructors to all closure types
		// TODO: move this step from the backend to the core

		return { precConversionResult.report, res };
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
