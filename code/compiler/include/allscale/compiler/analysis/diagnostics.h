#pragma once

#include <ostream>
#include <vector>
#include <string>

#include "insieme/core/ir_address.h"

#include "insieme/analysis/cba/haskell/context.h"

namespace allscale {
namespace compiler {
namespace analysis {

	/**
	 * Severity of the Diagnostics Message
	 */
	enum class Severity : int {
		Warning = 0,
		Error = 1,
	};

	std::ostream& operator<<(std::ostream& out, Severity severity) {
		switch(severity) {
		case Severity::Warning: return out << "Warning";
		case Severity::Error:   return out << "Error";
		}
		return out;
	}

	class Issue {

	  private:

		  insieme::core::NodeAddress target;
		  Severity severity;
		  std::string message;

	  public:

		  Issue(insieme::core::NodeAddress target, Severity severity, std::string message)
			  : target(target), severity(severity), message(message) {
			  assert_true(target);
		  }

		  insieme::core::NodeAddress getTarget() const {
			  return target;
		  }

		  Severity getSeverity() const {
			  return severity;
		  }

		  std::string getMessage() const {
			  return message;
		  }

		  friend std::ostream& operator<<(std::ostream& out, const Issue& issue);

	};

	using Issues = std::vector<Issue>;

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization = false, bool printNodeAddresse = false);

	// a context object for re-using partial results of analysis calls
	using AnalysisContext = insieme::analysis::cba::haskell::Context;

	/**
	 * The main entry point of the diagnostics analysis.
	 *
	 * @param context a context for the analysis to reuse partial results of previous analysis steps.
	 * @param node the node to be analyzed
	 * @return the list of diagnostic issues obtained for the given node
	 */
	Issues runDiagnostics(AnalysisContext& context, const insieme::core::NodeAddress& node);

	/**
	 * A convenience entry for the diagnostics analysis, producing a temporary analysis context.
	 *
	 * @param node the node to be analyzed
	 * @return the list of diagnostic issues obtained for the given node
	 */
	Issues runDiagnostics(const insieme::core::NodeAddress& node);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
