#pragma once

#include <vector>

#include <boost/optional.hpp>

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/analysis/cba/haskell/context.h"
#include "insieme/analysis/cba/common/set.h"

namespace allscale {
namespace compiler {
namespace analysis {

	// a context object for re-using partial results of analysis calls
	using AnalysisContext = insieme::analysis::cba::haskell::Context;

	// a type-def for a list of addresses referencing data item accesses
	using DataItemAccesses = insieme::analysis::cba::Set<insieme::core::NodeAddress>;

	// a type-def for the result type produced by the analysis defined by this header
	using DataItemAccessesOpt = boost::optional<DataItemAccesses>;

	/**
	 * A function collecting all data item accesses in the given statement.
	 *
	 * @param context a analysis context for result-reuse
	 * @param stmt the statement to be searched
	 * @return a list of addresses of data item accesses
	 */
	DataItemAccessesOpt getDataItemAccesses(AnalysisContext& context, const insieme::core::StatementPtr& stmt);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
