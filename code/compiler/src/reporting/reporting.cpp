#include "allscale/compiler/reporting/reporting.h"

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

	namespace detail {

		IssueDetails lookupDetails(ErrorCode error_code) {
			switch(error_code) {
			case ErrorCode::Timeout:                                        return {Severity::Warning, Category::Basic, "Timeout"};
			case ErrorCode::CallToUnknownFunction:                          return {Severity::Warning, Category::Basic, "Call to unknown function"};
			case ErrorCode::ReadAccessToUnknownLocation:                    return {Severity::Error,   Category::Basic, "Read access to unknown location"};
			case ErrorCode::WriteAccessToUnknownLocation:                   return {Severity::Error,   Category::Basic, "Write access to unknown location"};
			case ErrorCode::ReadAccessToGlobal:                             return {Severity::Error,   Category::Basic, "Read access to global"};
			case ErrorCode::WriteAccessToGlobal:                            return {Severity::Error,   Category::Basic, "Write access to global"};
			case ErrorCode::ReadAccessToPotentialDataItemElementReference:  return {Severity::Error,   Category::Basic, "Unable to determine data item element reference targeted by read operation"};
			case ErrorCode::WriteAccessToPotentialDataItemElementReference: return {Severity::Error,   Category::Basic, "Unable to determine data item element reference targeted by write operation"};
			case ErrorCode::ConvertParRegionToSharedMemoryParRuntimeCode:   return {Severity::Info,    Category::Basic, "Converted parallel region into shared-memory parallel runtime code."};
			default:                                                        return {Severity::Error,   Category::Basic, "Unknown error code"};
			};
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
		return out << toString(issue.details.severity) << ": "
				   << "[" << toString(issue.details.category) << "] "
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

	boost::property_tree::ptree toPropertyTree(const Issue & issue) {
		boost::property_tree::ptree ret;
		ret.put<string>("target", toString(issue.getTarget()));
		ret.put<string>("severity", toString(issue.getSeverity()));
		ret.put<string>("category", toString(issue.getCategory()));
		ret.put<string>("message", issue.getMessage());

		if(auto location = annotations::getLocation(issue.getTarget())) {
			ret.put<string>("loc_short", toString(*location));

			std::stringstream ss;
			annotations::prettyPrintLocation(ss, *location, true);
			ret.put<string>("loc_pretty", ss.str());
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
