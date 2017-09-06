#pragma once

#include <set>
#include <ostream>

#include <boost/property_tree/ptree.hpp>

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

namespace allscale {
namespace compiler {
namespace reporting {

	/**
	 * Error Code to easily identify a given Issue.
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
	};

	std::ostream& operator<<(std::ostream& out, ErrorCode err);

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

	class Issue {

	  private:

		insieme::core::NodeAddress target;
		ErrorCode error_code;
		ErrorDetails error_details;

		boost::optional<std::string> message;
		boost::optional<std::string> detail;

		Issue(insieme::core::NodeAddress target, ErrorCode error_code, boost::optional<std::string> message)
			: target(target), error_code(error_code), error_details(lookupDetails(error_code)), message(message), detail(boost::none) {
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

		boost::optional<std::string> getDetail() const {
			return detail;
		}

		void setDetail(const std::string& text) {
			detail = text;
		}

		bool operator==(const Issue&) const;
		bool operator<(const Issue&) const;

		friend std::ostream& operator<<(std::ostream& out, const Issue& issue);

		/**
		 * Returns an issue representing a timeout.
		 */
		static Issue timeout(const insieme::core::NodeAddress& node);

	};

	using Issues = std::set<Issue>;

	void prettyPrintIssue(std::ostream& out, const Issue& issue, bool disableColorization = false, bool printNodeAddresse = false);

	void prettyPrintLocation(std::ostream& out, const insieme::core::NodeAddress& target, bool disableColorization = false, bool printNodeAddress = false);

	boost::property_tree::ptree locationToPropertyTree(const insieme::core::NodeAddress& target);

	boost::property_tree::ptree toPropertyTree(const Issue& issue);

	boost::property_tree::ptree toPropertyTree(const Issues& issues);

} // end namespace reporting
} // end namespace compiler
} // end namespace allscale
