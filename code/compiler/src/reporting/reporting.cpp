#include "allscale/compiler/reporting/reporting.h"

#include <iomanip>

#include <boost/algorithm/string/replace.hpp>
#include <boost/property_tree/json_parser.hpp>

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/lang/lang.h"

#include "allscale/compiler/config.h"

using namespace insieme::core;

namespace allscale {
namespace compiler {
namespace reporting {

	namespace {

		boost::property_tree::ptree locationToPropertyTree(const NodeAddress& target) {
			boost::property_tree::ptree loc;
			loc.put<string>("address", toString(target));

			if(auto binding = target.isa<LambdaBindingAddress>()) {
				loc.put<string>("name", insieme::utils::demangle(binding->getReference()->getName()->getValue()));
			}
			else if(auto lambda = target.isa<LambdaExprAddress>()) {
				loc.put<string>("name", insieme::utils::demangle(lambda->getReference()->getName()->getValue()));
			}

			if(auto location = annotations::getLocation(target)) {
				loc.put<string>("location", toString(*location));

				if(auto source_file = std::fstream(location->getFile())) {
					std::stringstream ss;

					// goto first line
					for(unsigned i = 1; i < location->getStart().getLine(); i++) {
						source_file.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
					}

					// collect
					std::string line;
					for(unsigned i = location->getStart().getLine(); i <= location->getEnd().getLine(); i++) {
						std::getline(source_file, line);
						ss << line << "\n";
					}

					loc.put<string>("source", ss.str());
				}
			}

			if(lang::isBuiltIn(target.getAddressedNode())) {
				loc.put<bool>("is_builtin", true);
			}

			return loc;
		}

		boost::property_tree::ptree toPropertyTree(const Issue & issue) {
			boost::property_tree::ptree ret;
			ret.put<string>("error_code", toString(issue.getErrorCode()));
			ret.put<string>("target", toString(issue.getTarget()));
			ret.put<string>("severity", toString(issue.getSeverity()));
			ret.put<string>("category", toString(issue.getCategory()));
			ret.put<string>("message", issue.getMessage());

			if(const auto& details = issue.getDetail()) {
				ret.push_back(make_pair("details", details->toPropertyTree()));
			}

			// tags
			boost::property_tree::ptree tags;
			for(const auto& tag : issue.getTags()) {
				tags.push_back(make_pair("", boost::property_tree::ptree(toString(tag))));
			}
			if(!tags.empty()) {
				ret.push_back(make_pair("tags", tags));
			}

			// loc
			ret.push_back(make_pair("loc", locationToPropertyTree(issue.getTarget())));

			// backtrace
			{
				boost::property_tree::ptree backtrace;

				auto target = issue.getTarget();
				auto binding = target.getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
				while(binding) {
					auto lambdaexpr = binding.getFirstParentOfType(NodeType::NT_LambdaExpr);
					backtrace.push_back(make_pair("", locationToPropertyTree(lambdaexpr)));

					binding = binding.getParentAddress().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
				}

				ret.push_back(make_pair("backtrace", backtrace));
			}

			return ret;
		}

		boost::property_tree::ptree toPropertyTree(const Issues & issues) {
			boost::property_tree::ptree ret;
			for(const auto& issue : issues) {
				ret.push_back(make_pair("", toPropertyTree(issue)));
			}
			return ret;
		}

		boost::property_tree::ptree toPropertyTree(const ConversionReport& report) {

			// conversions + issues
			int index = 1;
			boost::property_tree::ptree conversions;
			for(const auto& p : report.issues) {
				boost::property_tree::ptree entry;

				// original target
				auto target = p.first;
				entry.put<string>("target", toString(target));

				entry.put<int>("index", index++);

				entry.push_back(make_pair("loc", locationToPropertyTree(target)));

				// locating user code
				{
					using namespace insieme::core;

					auto binding = target.getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
					while(binding) {
						auto lambdaexpr = binding.getFirstParentOfType(NodeType::NT_LambdaExpr);

						if(auto location = annotations::getLocation(lambdaexpr)) {
							if(!containsSubString(location->getFile(), "include/allscale/api")) {
								// found user code
								entry.push_back(make_pair("loc_user", locationToPropertyTree(lambdaexpr)));
								break;
							}
						}

						binding = binding.getParentAddress().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
					}
				}

				entry.push_back(make_pair("issues", toPropertyTree(p.second.first)));

				boost::property_tree::ptree variant_issues;
				{
					for(const auto& pp : p.second.second) {
						variant_issues.push_back(make_pair(pp.first, toPropertyTree(pp.second)));
					}
				}
				entry.push_back(make_pair("variant_issues", variant_issues));

				conversions.push_back(make_pair("", entry));
			}

			// collect help messages
			boost::property_tree::ptree help_messages;
			{
				std::set<ErrorCode> errors;
				for(const auto& p : report.issues) {
					for (const auto& issue : p.second.first) {
						errors.insert(issue.getErrorCode());
					}
					for(const auto& pp : p.second.second) {
						for(const auto& issue : pp.second) {
							errors.insert(issue.getErrorCode());
						}
					}
				}

				for (const auto& err : errors) {
					if (auto msg = lookupHelpMessage(err)) {
						help_messages.put<string>(toString(err), *msg);
					}
				}
			}

			boost::property_tree::ptree ret;
			ret.push_back(make_pair("conversions", conversions));
			ret.push_back(make_pair("help_messages", help_messages));
			return ret;
		}

	}

	std::ostream& operator<<(std::ostream& out, const ConversionReport& report) {

		out << "\n";
		out << " ------ AllScale Code Generation Report ------\n";

		out << "  Number of processed parallel regions: " << report.issues.size() << "\n";

		out << " ---------------------------------------------\n";

		int region_counter = 0;
		for(const auto& p : report.issues) {
			const auto& issues = p.second.first;
			const auto& variants = p.second.second;

			out << "Processed parallel region #" << (++region_counter) << ":\n";

			// output issues not associated with a specific variant
			for(const auto& issue : issues) {
				out << "\t" << issue << "\n";
			}

			// output issues associated with a specific variant
			for(const auto& pp : variants) {
				const auto& issues = pp.second;

				out << "\tVariant #" << pp.first << "\n";
				for(const auto& issue : issues) {
					out << "\t\t" << issue << "\n";
				}
			}

		}

		out << " ---------------------------------------------\n";

		return out;
	}

	namespace {

		template<Severity Level>
		bool has(const Issues& issues) {
			for(const auto& cur : issues) {
				if (cur.getSeverity() == Level) return true;
			}
			return false;
		}

		template<Severity Level, typename Issues>
		bool has(const Issues& issues) {
			for(const auto& cur : issues) {
				if (has<Level>(cur.second.first)) return true;
				for(const auto& variant : cur.second.second) {
					if (has<Level>(variant.second)) return true;
				}
			}
			return false;
		}

	}

	bool ConversionReport::containsErrors() const {
		return has<Severity::Error>(issues);
	}

	bool ConversionReport::containsWarnings() const {
		return has<Severity::Warning>(issues);
	}


	void ConversionReport::toJSON(const std::string& filename) const {
		write_json(filename, toPropertyTree(*this));
	}

	void ConversionReport::toJSON(std::ostream& out) const {
		write_json(out, toPropertyTree(*this));
	}

	void ConversionReport::toHTML(const std::string& filename) const {
		auto report_template = getAllscaleBuildRootDir() + "/report.out.html"; // generated by CMake
		std::ifstream in(report_template);
		assert_true(in)  << "could not open template for conversion report";
		in >> std::noskipws;

		std::ofstream out(filename);
		assert_true(out) << "could not open output file for conversion report";

		std::stringstream report_buffer;
		write_json(report_buffer, toPropertyTree(*this));

		std::stringstream timestamp;
		{
			std::time_t t = std::time(nullptr);
			timestamp << std::asctime(std::localtime(&t));
		}

		while(in) {
			std::string line;
			std::getline(in, line);

			boost::replace_all(line, "%REPORT%",   report_buffer.str());
			boost::replace_all(line, "%DATETIME%", timestamp.str());

			out << line << "\n";
		}
	}

	std::ostream& operator<<(std::ostream& out, ErrorCode err) {
		auto flags = out.flags();
		out << toString(lookupDetails(err).severity)[0]
		    << std::setfill('0') << std::setw(3) << static_cast<int>(err);
		out.flags(flags);
		return out;
	}

	std::ostream& operator<<(std::ostream& out, Severity severity) {
		switch(severity) {
		case Severity::Warning: return out << "Warning";
		case Severity::Error:   return out << "Error";
		case Severity::Info:    return out << "Info";
		}
		return out;
	}

	std::ostream& operator<<(std::ostream& out, Category category) {
		switch(category) {
		case Category::Basic:             return out << "Basic";
		case Category::DistributedMemory: return out << "Distributed Memory";
		}
		return out;
	}

	std::ostream& operator<<(std::ostream& out, Tag tag) {
		switch(tag) {
		case Tag::Timeout: return out << "Timeout";
		case Tag::Read:    return out << "Read";
		case Tag::Write:   return out << "Write";
		case Tag::Global:  return out << "Global";
		}
		return out;
	}

	ErrorDetails lookupDetails(ErrorCode err) {
		switch(err) {
		case ErrorCode::Timeout:                                           return {Severity::Warning, {},                        Category::Basic,             "Timeout"};
		case ErrorCode::CallToUnknownFunction:                             return {Severity::Warning, {},                        Category::Basic,             "Call to unknown function"};
		case ErrorCode::ReadAccessToUnknownLocation:                       return {Severity::Warning, {Tag::Read},               Category::Basic,             "Compiler has been unable to reliably determine targeted memory location of read access"};
		case ErrorCode::WriteAccessToUnknownLocation:                      return {Severity::Warning, {Tag::Write},              Category::Basic,             "Compiler has been unable to reliably determine targeted memory location of write access"};
		case ErrorCode::ReadAccessToGlobal:                                return {Severity::Error,   {Tag::Read,  Tag::Global}, Category::Basic,             "Invalid read access to global memory location"};
		case ErrorCode::WriteAccessToGlobal:                               return {Severity::Error,   {Tag::Write, Tag::Global}, Category::Basic,             "Invalid write access to global memory location"};
		case ErrorCode::ReadAccessToPotentialDataItemElementReference:     return {Severity::Error,   {Tag::Read},               Category::Basic,             "Unable to determine data item element reference targeted by read operation"};
		case ErrorCode::WriteAccessToPotentialDataItemElementReference:    return {Severity::Error,   {Tag::Write},              Category::Basic,             "Unable to determine data item element reference targeted by write operation"};
		case ErrorCode::UnobtainableDataRequirement:                       return {Severity::Error,   {},                        Category::Basic,             "Unable to obtain data requirements for given code variant"};
		case ErrorCode::ObtainedDataRequirement:                           return {Severity::Info,    {},                        Category::Basic,             "Successfully obtained data requirements for given code variant"};
		case ErrorCode::ConvertParRegionToSharedMemoryParRuntimeCode:      return {Severity::Info,    {},                        Category::Basic,             "Converted parallel region into shared-memory parallel runtime code."};
		case ErrorCode::ConvertParRegionToDistributedMemoryParRuntimeCode: return {Severity::Info,    {},                        Category::Basic,             "Converted parallel region into distributed-memory parallel runtime code."};
		case ErrorCode::UnableToInstrumentVariantForDataItemAccessCheck:   return {Severity::Error,   {},                        Category::Basic,             "Unable to instrument data item accesses as requested due to unknown targets of read/write operations."};
		case ErrorCode::InstrumentedVariantForDataItemAccessCheck:         return {Severity::Info,    {},                        Category::Basic,             "Successfully instrumented data item accesses."};
		case ErrorCode::CallToInvalidFunctionForDistributedMemory:         return {Severity::Warning, {},                        Category::DistributedMemory, "Call to blacklisted function prevents generation of distributed memory code."};
		case ErrorCode::InvalidUseOfGlobalForDistributedMemory:            return {Severity::Warning, {Tag::Global},             Category::DistributedMemory, "Use of global variable prevents generation of distributed memory code."};
		case ErrorCode::ValidForDistributedMemory:                         return {Severity::Info,    {},                        Category::DistributedMemory, "Given code variant can be converted to distributed memory target code."};
		case ErrorCode::InvalidForDistributedMemory:                       return {Severity::Error,   {},                        Category::DistributedMemory, "Given code variant can not be converted to distributed memory target code."};
		case ErrorCode::RefOrPtrFoundInCaptureList:                        return {Severity::Warning, {},                        Category::DistributedMemory, "Use of captured reference / pointer prevents generation of distributed memory code."};
		};
		assert_true(false) << "Unknown ErrorCode " << static_cast<int>(err);
		return {};
	}

	boost::optional<std::string> lookupHelpMessage(ErrorCode err) {
		switch (err) {
		case ErrorCode::ConvertParRegionToSharedMemoryParRuntimeCode: return {{"dummy help text"}};
		default: return {};
		}
	}

	bool Issue::operator==(const Issue& other) const {
		return error_code == other.error_code
			&& target == other.target
			&& message == other.message;
	}

	bool Issue::operator<(const Issue& other) const {
		auto thisDetails = lookupDetails(error_code);
		auto thatDetails = lookupDetails(other.error_code);

		// first by severity
		if (thisDetails.severity < thatDetails.severity) return true;
		if (!(thisDetails.severity == thatDetails.severity)) return false;

		// than by target
		if (target < other.target) return true;
		if (!(target == other.target)) return false;

		// finally by issue code
		if (error_code < other.error_code) return true;
		if (!(error_code == other.error_code)) return false;

		// and if this is all the same, than by message (quite unlikely)
		return message < other.message;
	}

	std::ostream& operator<<(std::ostream& out, const Issue& issue) {
		out << toString(issue.error_details.severity) << ": "
		    << "[" << toString(issue.error_details.category) << "] ";

		auto msg = issue.getMessage();
		if(msg.size() > 80) {
			out << msg.substr(0, 77) << "...";
		} else {
			out << msg;
		}

		return out ;
	}

	Issue Issue::timeout(const NodeAddress& node) {
		return Issue(node, ErrorCode::Timeout);
	}

	void prettyPrintLocation(std::ostream& out, const NodeAddress& target, bool disableColorization, bool printNodeAddress) {

		// print target nesting information
		auto binding = target.getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
		while (binding) {
			auto lambdaexpr = binding.getFirstParentOfType(NodeType::NT_LambdaExpr);

			out << "\t\tfrom: ";

			// name
			out << "\"" << insieme::utils::demangle(binding->getReference()->getName()->getValue()) << "\"";

			// location
			if(lang::isBuiltIn(lambdaexpr.getAddressedNode())) {
				out << " (builtin)";
			} else if(auto location = annotations::getLocation(lambdaexpr)) {
				out << " (" << *location << ")";
			}

			if (printNodeAddress) {
				out << " at " << toString(lambdaexpr);
			}

			out << "\n";

			binding = binding.getParentAddress().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
		}

		// and the final location
		if(auto location = annotations::getLocation(target)) {
			annotations::prettyPrintLocation(out, *location, disableColorization);
		}
	}

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization /* = false */, bool printNodeAddress /* = false */) {
		out << issue << "\n";

		if(printNodeAddress) {
			out << "at address " << toString(issue.getTarget()) << "\n";
		}

		// print target nesting information
		prettyPrintLocation(out, issue.getTarget(),disableColorization,printNodeAddress);
	}

} // end namespace reporting
} // end namespace compiler
} // end namespace allscale
