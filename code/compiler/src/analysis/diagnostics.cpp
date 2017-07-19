#include "allscale/compiler/analysis/diagnostics.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/lang/lang.h"

#include "insieme/utils/name_mangling.h"

using namespace insieme::core;
using namespace insieme::analysis::cba::haskell;

extern "C" {

	using namespace allscale::compiler::analysis;

	Issues* hat_hs_diagnostics(StablePtr ctx, const HaskellNodeAddress node_hs);

}

namespace allscale {
namespace compiler {
namespace analysis {

	std::ostream& operator<<(std::ostream& out, const Issue& issue) {
		return out << toString(issue.severity) << ": "
		           << issue.message << "\n"
		           << "at address " << toString(issue.target);
	}

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization /* = false */) {
		out << issue << "\n";

		// print target nesting information
		auto binding = issue.getTarget().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
		while (binding) {
			auto lambdaexpr = binding.getFirstParentOfType(NodeType::NT_LambdaExpr);

			out << "\t\tfrom: ";

			// name
			out << "\"" << insieme::utils::demangle(binding->getReference()->getName()->getValue()) << "\" ";

			// location
			if(lang::isBuiltIn(lambdaexpr.getAddressedNode())) {
				out << "(builtin) ";
			} else if(auto location = annotations::getLocation(lambdaexpr)) {
				out << "(" << *location << ") ";
			}

			// address
			out << "at " << toString(lambdaexpr) << "\n";

			binding = binding.getParentAddress().getFirstParentOfType(NodeType::NT_LambdaBinding).as<LambdaBindingAddress>();
		}

		if(auto location = annotations::getLocation(issue.getTarget())) {
			annotations::prettyPrintLocation(out, *location, disableColorization);
		}
	}

	Issues runDiagnostics(const insieme::core::NodeAddress& node) {
		AnalysisContext context;
		return runDiagnostics(context, node);
	}

	Issues runDiagnostics(AnalysisContext& ctx, const insieme::core::NodeAddress& node) {
		auto node_hs = ctx.resolveNodeAddress(node);
		Issues* res_ptr = hat_hs_diagnostics(ctx.getHaskellContext(), node_hs);
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

	Issue* hat_c_mk_issue(NodeAddress* target, Severity severity, const char* message) {
		return new Issue(*target, static_cast<Severity>(severity), message);
	}

	Issues* hat_c_mk_issues(Issue* issues[], size_t size) {
		auto ret = new Issues;
		ret->reserve(size);
		for(size_t i = 0; i < size; i++) {
			ret->emplace_back(std::move(*issues[i]));
			delete issues[i];
		}
		return ret;
	}

}