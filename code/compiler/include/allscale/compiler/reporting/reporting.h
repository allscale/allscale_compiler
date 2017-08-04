#pragma once

#include <set>
#include <ostream>

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

namespace allscale {
namespace compiler {
namespace reporting {

	/**
	 * Severity of the Diagnostics Message
	 */
	enum class Severity : int {
		Warning = 0,
		Error = 1,
		Info = 2
	};

	std::ostream& operator<<(std::ostream& out, Severity severity);

	/**
	 * Category of the Diagnostics Message
	 */
	enum class Category : int {
		Basic = 0,
	};

	std::ostream& operator<<(std::ostream& out, Category category);

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

	void prettyPrintLocation(std::ostream& out, const insieme::core::NodeAddress& target, bool disableColorization = false, bool printNodeAddress = false);

} // end namespace reporting
} // end namespace compiler
} // end namespace allscale
