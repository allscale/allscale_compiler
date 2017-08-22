#include "allscale/compiler/analysis/diagnostics.h"


using namespace insieme::core;
using namespace insieme::analysis::cba::haskell;

extern "C" {

	using namespace allscale::compiler::analysis;
	using namespace allscale::compiler::reporting;

	AnalysisResult<Issues*>* hat_hs_diagnostics(StablePtr ctx, const HaskellNodeAddress node_hs, unsigned long flags);

}

namespace allscale {
namespace compiler {
namespace analysis {

	Issues runDiagnostics(const insieme::core::NodeAddress& node, DiagnosisFlags flags /* = DiagnosisFlagsAll */) {
		AnalysisContext context;
		return runDiagnostics(context, node, flags);
	}

	Issues runDiagnostics(AnalysisContext& ctx, const insieme::core::NodeAddress& node, DiagnosisFlags flags /* = DiagnosisFlagsAll */) {
		auto node_hs = ctx.resolveNodeAddress(node);
		auto result = hat_hs_diagnostics(ctx.getHaskellContext(), node_hs, flags);
		auto value = ctx.unwrapResult(result);

		if(!value) {
			return {Issue::timeout(node)};
		}

		return *value;
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale


extern "C" {

	Issue* hat_c_mk_issue(NodeAddress* target, Severity severity, Category category, const char* message) {
		return new Issue(*target, static_cast<Severity>(severity), category, message);
	}

	void hat_c_del_issue(Issue* i) {
		delete i;
	}

	Issues* hat_c_mk_issues(Issue* issues[], size_t size) {
		auto ret = new Issues;
		for(size_t i = 0; i < size; i++) {
			ret->emplace(std::move(*issues[i]));
		}
		return ret;
	}

}
