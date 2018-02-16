#include "allscale/compiler/analysis/out_of_bounds_analysis.h"

#include "insieme/analysis/cba/common/failure.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/inspyer/inspyer.h"

extern "C" {

	using namespace allscale::compiler::analysis;

	namespace hat = insieme::analysis::cba::haskell;

	// Analysis
	hat::AnalysisResult<OutOfBoundsResult>* hat_out_of_bounds(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

using namespace insieme;
using namespace insieme::analysis::cba;
using namespace insieme::analysis::cba::haskell;

namespace allscale {
namespace compiler {
namespace analysis {

	OutOfBoundsResult getOutOfBounds(Context& ctxt, const core::CallExprAddress& call) {
		const auto& refext = call.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		assert_true(refext.isCallOfRefDeref(call));

		auto call_hs = ctxt.resolveNodeAddress(call);
		auto result = ctxt.runAnalysis<OutOfBoundsResult>(hat_out_of_bounds, call_hs);
		if(!result) throw AnalysisFailure("Timeout in Out-Of-Bounds Analysis");
		return *result;
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
