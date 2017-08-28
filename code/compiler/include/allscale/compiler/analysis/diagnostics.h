#pragma once

#include <ostream>
#include <set>
#include <string>

#include "insieme/core/ir_address.h"

#include "insieme/analysis/cba/haskell/context.h"

#include "allscale/compiler/reporting/reporting.h"

namespace allscale {
namespace compiler {
namespace analysis {

	enum DiagnosisFlags : unsigned long {
		DiagnosisFlagsAll              = ~0ul,
		DiagnosisFlagsUnknownReference = (1ul << 0),
		DiagnosisFlagsGlobalVariable   = (1ul << 1),
		DiagnosisFlagsUncertainAccess  = (1ul << 2),
	};

	// a context object for re-using partial results of analysis calls
	using AnalysisContext = insieme::analysis::cba::haskell::Context;

	/**
	 * The main entry point of the diagnostics analysis.
	 *
	 * @param context a context for the analysis to reuse partial results of previous analysis steps.
	 * @param node the node to be analyzed
	 * @param flags the combination of diagnosis to run on the target node
	 * @return the list of diagnostic issues obtained for the given node
	 */
	reporting::Issues runDiagnostics(AnalysisContext& context, const insieme::core::NodeAddress& node, DiagnosisFlags flags = DiagnosisFlagsAll);

	/**
	 * A convenience entry for the diagnostics analysis, producing a temporary analysis context.
	 *
	 * @param node the node to be analyzed
	 * @param flags the combination of diagnosis to run on the target node
	 * @return the list of diagnostic issues obtained for the given node
	 */
	reporting::Issues runDiagnostics(const insieme::core::NodeAddress& node, DiagnosisFlags flags = DiagnosisFlagsAll);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
