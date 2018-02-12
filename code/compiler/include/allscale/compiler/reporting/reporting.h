#pragma once

#include <memory>
#include <ostream>
#include <set>

#include <boost/property_tree/ptree.hpp>

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

namespace allscale {
namespace compiler {
namespace reporting {

	class Issue;

	using Issues = std::set<Issue>;

	/**
	 * A conversion report is produced by the core conversion process along with
	 * the conversion result. The report contains information regarding the conversion
	 * process to be forwarded to the user.
	 */
	struct ConversionReport {

		using PrecCall = insieme::core::CallExprAddress;

		using VariantId = std::string;

		using VariantIssues = std::map<VariantId, Issues>;

		// the collected issues, indexed by the prec operator location
		std::map<PrecCall, std::pair<Issues, VariantIssues>> issues;

		void addMessage(const PrecCall& prec, const Issue& issue) {
			issues[prec].first.insert(issue);
		}

		void addMessage(const PrecCall& prec, const VariantId& variant, const Issue& issue) {
			issues[prec].second[variant].insert(issue);
		}

		void addMessages(const PrecCall& prec, const Issues& is) {
			issues[prec].first.insert(is.begin(), is.end());
		}

		void addMessages(const PrecCall& prec, const VariantId& variant, const Issues& is) {
			issues[prec].second[variant].insert(is.begin(), is.end());
		}

		bool containsErrors() const;

		bool containsWarnings() const;

		void toJSON(const std::string& filename) const;

		void toJSON(std::ostream& out) const;

		void toHTML(const std::string& filename) const;

	};

	std::ostream& operator<<(std::ostream& out, const ConversionReport& report);

	/**
	 * Error Code to easily identify a given Issue.
	 *
	 * Needs to be kept in sync with Diagnostics.hs!
	 */
	enum class ErrorCode : int {
		Timeout,
		CallToUnknownFunction,
		ReadAccessToUnknownLocation,
		WriteAccessToUnknownLocation,
		ReadAccessToGlobal,
		WriteAccessToGlobal,
		ReadAccessToPotentialDataItemElementReference,
		WriteAccessToPotentialDataItemElementReference,
		UnobtainableDataRequirement,
		ObtainedDataRequirement,
		ConvertParRegionToSharedMemoryParRuntimeCode,
		ConvertParRegionToDistributedMemoryParRuntimeCode,
		UnableToInstrumentVariantForDataItemAccessCheck,
		InstrumentedVariantForDataItemAccessCheck,
		CallToInvalidFunctionForDistributedMemory,
		InvalidUseOfGlobalForDistributedMemory,
		ValidForDistributedMemory,
		InvalidForDistributedMemory,
		RefOrPtrFoundInCaptureList,
	};

	std::ostream& operator<<(std::ostream& out, ErrorCode err);

	/**
	 * Severity of the Diagnostics Message
	 */
	enum class Severity : int {
		Error = 0,
		Warning = 1,
		Info = 2
	};

	std::ostream& operator<<(std::ostream& out, Severity severity);

	/**
	 * Category of the Diagnostics Message
	 */
	enum class Category : int {
		Basic = 0,
		DistributedMemory,
	};

	std::ostream& operator<<(std::ostream& out, Category category);

	/**
	 * Tags of the Diagnostics Message
	 **/
	enum class Tag : int {
		Timeout,
		Read,
		Write,
		Global,
	};

	/**
	 * Details of a specific error code
	 */
	struct ErrorDetails {
		Severity severity;
		std::vector<Tag> tags;
		Category category;
		std::string defaultMessage;
	};

	std::ostream& operator<<(std::ostream& out, Tag tag);

	ErrorDetails lookupDetails(ErrorCode err);

	boost::optional<std::string> lookupHelpMessage(ErrorCode err);

	class IssueDetails {
	  public:
		virtual boost::property_tree::ptree toPropertyTree() const = 0;
	};

	class IssueDetailsMessage: public IssueDetails {
	  private:

		std::string message;

	  public:

		IssueDetailsMessage(const std::string& message) : message(message) {}

		virtual ~IssueDetailsMessage() = default;

		virtual boost::property_tree::ptree toPropertyTree() const override {
			boost::property_tree::ptree t;
			t.put<string>("type", "message");
			t.put<string>("message", message);
			return t;
		}

	};

	class Issue {

	  private:

		insieme::core::NodeAddress target;
		ErrorCode error_code;
		ErrorDetails error_details;

		boost::optional<std::string> message;
		std::shared_ptr<IssueDetails> detail;

		Issue(insieme::core::NodeAddress target, ErrorCode error_code, boost::optional<std::string> message)
			: target(target), error_code(error_code), error_details(lookupDetails(error_code)), message(message), detail() {
			assert_true(target);
		}

	  public:

		Issue(insieme::core::NodeAddress target, ErrorCode error_code)
			: Issue(target, error_code, boost::none) {}

		Issue(insieme::core::NodeAddress target, ErrorCode error_code, std::string message)
			: Issue(target, error_code, boost::optional<std::string>{message}) {}

		insieme::core::NodeAddress getTarget() const {
			return target;
		}

		ErrorCode getErrorCode() const {
			return error_code;
		}

		Severity getSeverity() const {
			return error_details.severity;
		}

		Category getCategory() const {
			return error_details.category;
		}

		const std::vector<Tag>& getTags() const {
			return error_details.tags;
		}

		std::string getMessage() const {
			if(!message) {
				return error_details.defaultMessage;
			}
			return *message;
		}

		std::shared_ptr<IssueDetails> getDetail() const {
			return detail;
		}

		void setDetail(std::shared_ptr<IssueDetails> detail) {
			this->detail = detail;
		}

		bool operator==(const Issue&) const;
		bool operator<(const Issue&) const;

		friend std::ostream& operator<<(std::ostream& out, const Issue& issue);

		/**
		 * Returns an issue representing a timeout.
		 */
		static Issue timeout(const insieme::core::NodeAddress& node);

	};

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization = false, bool printNodeAddresse = false);

	void prettyPrintLocation(std::ostream& out, const insieme::core::NodeAddress& target, bool disableColorization = false, bool printNodeAddress = false);

} // end namespace reporting
} // end namespace compiler
} // end namespace allscale
