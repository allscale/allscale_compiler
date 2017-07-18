#include "allscale/compiler/analysis/diagnostics.h"

using namespace insieme::core;
using namespace insieme::analysis::cba::haskell;

extern "C" {

	using namespace allscale::compiler::analysis;

	Issues* hat_hs_diagnostics(StablePtr ctx, const HaskellNodeAddress node_hs);

}

namespace allscale {
namespace compiler {
namespace analysis {

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