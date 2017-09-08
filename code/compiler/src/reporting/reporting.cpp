#include "allscale/compiler/reporting/reporting.h"

#include <iomanip>

#include <boost/property_tree/json_parser.hpp>

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/lang/lang.h"

using namespace insieme::core;

namespace allscale {
namespace compiler {
namespace reporting {

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
		case Category::Basic: return out << "Basic";
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
		case ErrorCode::Timeout:                                        return {Severity::Warning, {},                        Category::Basic, "Timeout"};
		case ErrorCode::CallToUnknownFunction:                          return {Severity::Warning, {},                        Category::Basic, "Call to unknown function"};
		case ErrorCode::ReadAccessToUnknownLocation:                    return {Severity::Error,   {Tag::Read},               Category::Basic, "Read access to unknown location"};
		case ErrorCode::WriteAccessToUnknownLocation:                   return {Severity::Error,   {Tag::Write},              Category::Basic, "Write access to unknown location"};
		case ErrorCode::ReadAccessToGlobal:                             return {Severity::Error,   {Tag::Read,  Tag::Global}, Category::Basic, "Read access to global"};
		case ErrorCode::WriteAccessToGlobal:                            return {Severity::Error,   {Tag::Write, Tag::Global}, Category::Basic, "Write access to global"};
		case ErrorCode::ReadAccessToPotentialDataItemElementReference:  return {Severity::Error,   {Tag::Read},               Category::Basic, "Unable to determine data item element reference targeted by read operation"};
		case ErrorCode::WriteAccessToPotentialDataItemElementReference: return {Severity::Error,   {Tag::Write},              Category::Basic, "Unable to determine data item element reference targeted by write operation"};
		case ErrorCode::UnobtainableDataRequirement:                    return {Severity::Error,   {},                        Category::Basic, "Unable to obtain data requirement"};
		case ErrorCode::ObtainedDataRequirement:                        return {Severity::Info,    {},                        Category::Basic, "Obtained data requirement"};
		case ErrorCode::ConvertParRegionToSharedMemoryParRuntimeCode:   return {Severity::Info,    {},                        Category::Basic, "Converted parallel region into shared-memory parallel runtime code."};
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
		if (target < other.target) return true;
		if (!(target == other.target)) return false;

		if (error_code < other.error_code) return true;
		if (!(error_code == other.error_code)) return false;

		return message < other.message;
	}

	std::ostream& operator<<(std::ostream& out, const Issue& issue) {
		return out << toString(issue.error_details.severity) << ": "
		           << "[" << toString(issue.error_details.category) << "] "
		           << issue.getMessage();
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

} // end namespace reporting
} // end namespace compiler
} // end namespace allscale
