#include "allscale/compiler/analysis/diagnostics.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/lang/lang.h"

#include "insieme/utils/name_mangling.h"

using namespace insieme::core;
using namespace insieme::analysis::cba::haskell;

extern "C" {

	using namespace allscale::compiler::analysis;

	Issues* hat_hs_diagnostics(StablePtr ctx, const HaskellNodeAddress node_hs, unsigned long flags);

}

namespace allscale {
namespace compiler {
namespace analysis {

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

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization /* = false */, bool printNodeAddresse /* = false */) {
		out << issue << "\n";

		if(printNodeAddresse) {
			out << "at address " << toString(issue.getTarget()) << "\n";
		}

		// print target nesting information
		auto binding = issue.getTarget().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
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

			if (printNodeAddresse) {
				out << " at " << toString(lambdaexpr);
			}

			out << "\n";

			binding = binding.getParentAddress().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
		}

		if(auto location = annotations::getLocation(issue.getTarget())) {
			annotations::prettyPrintLocation(out, *location, disableColorization);
		}
	}

	Issues runDiagnostics(const insieme::core::NodeAddress& node, DiagnosisFlags flags /* = DiagnosisFlags::All */) {
		AnalysisContext context;
		return runDiagnostics(context, node, flags);
	}

	Issues runDiagnostics(AnalysisContext& ctx, const insieme::core::NodeAddress& node, DiagnosisFlags flags /* = DiagnosisFlags::All */) {
		auto node_hs = ctx.resolveNodeAddress(node);
		Issues* res_ptr = hat_hs_diagnostics(ctx.getHaskellContext(), node_hs, static_cast<unsigned long>(flags));
		assert_true(res_ptr);

		Issues res = std::move(*res_ptr);
		delete res_ptr;

		return res;
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale


extern "C" {

	using namespace allscale::compiler::analysis;

	Issue* hat_c_mk_issue(NodeAddress* target, Severity severity, Category category, const char* message) {
		return new Issue(*target, static_cast<Severity>(severity), category, message);
	}

	Issues* hat_c_mk_issues(Issue* issues[], size_t size) {
		auto ret = new Issues;
		for(size_t i = 0; i < size; i++) {
			ret->emplace(std::move(*issues[i]));
			delete issues[i];
		}
		return ret;
	}

}