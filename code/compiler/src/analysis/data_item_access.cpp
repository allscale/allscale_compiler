#include "allscale/compiler/analysis/data_item_access.h"

extern "C" {

	using namespace insieme::analysis::cba::haskell;
	using namespace allscale::compiler::analysis;

	AnalysisResult<DataItemAccesses*>* hat_hs_data_item_accesses(StablePtr ctx, const HaskellNodeAddress node_hs);

}

namespace allscale {
namespace compiler {
namespace analysis {

	using namespace insieme::core;

	// -- Data Item Accesses Analysis --

	DataItemAccessesOpt getDataItemAccesses(AnalysisContext& ctx, const StatementPtr& stmt) {
		auto node_hs = ctx.resolveNodeAddress(NodeAddress(stmt));
		return ctx.runAnalysis<DataItemAccesses*>(hat_hs_data_item_accesses, node_hs);
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
