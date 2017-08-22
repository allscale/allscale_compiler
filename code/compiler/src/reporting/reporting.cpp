#include "allscale/compiler/reporting/reporting.h"

#include "insieme/utils/name_mangling.h"

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
		case Severity::Info:   return out << "Info";
		}
		return out;
	}

	std::ostream& operator<<(std::ostream& out, Category category) {
		switch(category) {
		case Category::Basic: return out << "Basic";
		}
		return out;
	}

	bool Issue::operator==(const Issue& other) const {
		return category == other.category
			&& severity == other.severity
			&& target == other.target
			&& message == other.message;
	}

	bool Issue::operator<(const Issue& other) const {
		if (target < other.target) return true;
		if (!(target == other.target)) return false;

		if (severity < other.severity) return true;
		if (!(severity == other.severity)) return false;

		if (category < other.category) return true;
		if (!(category == other.category)) return false;

		return message < other.message;
	}

	std::ostream& operator<<(std::ostream& out, const Issue& issue) {
		return out << toString(issue.severity) << ": "
				   << "[" << toString(issue.category) << "] "
				   << issue.message;
	}

	Issue Issue::timeout(const NodeAddress& node) {
		return Issue(node, Severity::Warning, Category::Basic, "Timeout");
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
