#pragma once

#include <ostream>
#include <set>

#include "insieme/core/ir.h"
#include "insieme/analysis/cba/haskell/context.h"

namespace allscale {
namespace compiler {
namespace analysis {

	/**
	 * The type of access required to some part of a data item.
	 */
	enum class AccessMode {
		ReadOnly,
		ReadWrite
	};

	// enable printing of access modes
	std::ostream& operator<<(std::ostream& out, AccessMode mode);


	/**
	 * A data requirement, referencing a data item, a (sub-)range of
	 * the data item and the mode of access.
	 */
	class DataRequirement {

		// the expression addressing the data item reference
		insieme::core::ExpressionPtr dataItem;

		// the expression computing the range
		insieme::core::ExpressionPtr range;

		// the access mode
		AccessMode mode;

	public:

		// TODO: add ways to express unknown requirements


		DataRequirement(const insieme::core::ExpressionPtr& dataItem, const insieme::core::ExpressionPtr& range, AccessMode mode)
			: dataItem(dataItem), range(range), mode(mode) {

			assert_true(dataItem) << "Data item reference must not be null!";
			assert_true(range) << "Data item range must not be null!";

		}


		bool operator==(const DataRequirement& other) const {
			// shortcut for the same object id
			if (this == &other) return true;
			// compare individual fields
			return mode == other.mode && *dataItem == *other.dataItem && *range == *other.range;
		}

		bool operator!=(const DataRequirement& other) const {
			return !(*this == other);
		}

		bool operator<(const DataRequirement& other) const {
			// shortcut for identity
			if (this == &other) return false;

			// start with mode
			if (mode < other.mode) return true;
			if (mode != other.mode) return false;

			// then the data item reference
			if (*dataItem < *other.dataItem) return true;
			if (*dataItem != *other.dataItem) return false;

			// and finally the range
			return *range < *other.range;
		}

		friend std::ostream& operator<<(std::ostream& out, const DataRequirement& req);

	};


	/**
	 *
	 */
	class DataRequirements {

		std::set<DataRequirement> requirements;


	public:

		bool empty() const {
			return requirements.empty();
		}

		friend std::ostream& operator<<(std::ostream& out, const DataRequirements& reqs);

	};

	// a context object for re-using partial results of analysis calls
	using AnalysisContext = insieme::analysis::cba::haskell::Context;

	/**
	 * The main entry point of the data requirement analysis, computing the
	 *
	 * @param context a context for the analysis to reuse partial results of previous analysis steps.
	 * @param stmt the statement to be analyzed
	 * @return the list of data requirements obtained for the given statement
	 */
	DataRequirements getDataRequirements(AnalysisContext& context, const insieme::core::StatementPtr& stmt);

	/**
	 * A convenience entry for the data requirement analysis, producing a temporary analysis context.
	 *
	 * @param stmt the statement to be analyzed
	 * @return the list of data requirements obtained for the given statement
	 */
	DataRequirements getDataRequirements(const insieme::core::StatementPtr& stmt);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
