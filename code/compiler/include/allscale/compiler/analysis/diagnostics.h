#pragma once

#include <ostream>
#include <set>
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

	/**
	 * Category of the Diagnostics Message
	 */
	enum class Category : int {
		Basic = 0,
	};

	std::ostream& operator<<(std::ostream& out, Category category) {
		switch(category) {
		case Category::Basic: return out << "Basic";
		}
		return out;
	}

	class Issue {

	  private:

		insieme::core::NodeAddress target;
		Severity severity;
		Category category;
		std::string message;

	  public:

		Issue(insieme::core::NodeAddress target, Severity severity, Category category, std::string message)
			: target(target), severity(severity), category(category), message(message) {
			assert_true(target);
		}

		insieme::core::NodeAddress getTarget() const {
			return target;
		}

		Severity getSeverity() const {
			return severity;
		}

		Category getCategory() const {
			return category;
		}

		std::string getMessage() const {
			return message;
		}

		bool operator==(const Issue&) const;
		bool operator<(const Issue&) const;

		friend std::ostream& operator<<(std::ostream& out, const Issue& issue);

	};

	using Issues = std::set<Issue>;

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization = false, bool printNodeAddresse = false);

	enum DiagnosisFlags : unsigned long {
		DiagnosisFlagsAll              = ~0u,
		DiagnosisFlagsUnknownReference = (1ul << 0),
		DiagnosisFlagsGlobalVariable   = (1ul << 1),
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
	Issues runDiagnostics(AnalysisContext& context, const insieme::core::NodeAddress& node, DiagnosisFlags flags = DiagnosisFlagsAll);

	/**
	 * A convenience entry for the diagnostics analysis, producing a temporary analysis context.
	 *
	 * @param node the node to be analyzed
	 * @param flags the combination of diagnosis to run on the target node
	 * @return the list of diagnostic issues obtained for the given node
	 */
	Issues runDiagnostics(const insieme::core::NodeAddress& node, DiagnosisFlags flags = DiagnosisFlagsAll);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
