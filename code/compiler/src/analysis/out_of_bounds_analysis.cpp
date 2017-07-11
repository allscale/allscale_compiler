#include "allscale/compiler/analysis/out_of_bounds_analysis.h"

#include "insieme/core/lang/reference.h"
#include "insieme/core/inspyer/inspyer.h"

extern "C" {

	namespace hat = insieme::analysis::cba::haskell;

	// Analysis
	int hat_out_of_bounds(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

using namespace insieme;
using namespace insieme::analysis::cba::haskell;

namespace allscale {
namespace compiler {
namespace analysis {

	OutOfBoundsResult getOutOfBounds(Context& ctxt, const core::CallExprAddress& call) {
		const auto& refext = call.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		assert_true(refext.isCallOfRefDeref(call));

		auto call_hs = ctxt.resolveNodeAddress(call);
		int res = hat_out_of_bounds(ctxt.getHaskellContext(), call_hs);

		return static_cast<OutOfBoundsResult>(res);
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
