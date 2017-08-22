#pragma once

#include <ostream>

#include <boost/optional.hpp>

#include "insieme/core/ir.h"
#include "insieme/analysis/cba/common/set.h"
#include "insieme/analysis/cba/haskell/context.h"

#include "insieme/utils/comparable.h"

namespace allscale {
namespace compiler {
namespace analysis {

	/**
	 * The type of access required to some part of a data item.
	 */
	enum class AccessMode : int {
		ReadOnly  = 0,
		ReadWrite = 1
	};

	// enable printing of access modes
	std::ostream& operator<<(std::ostream& out, AccessMode mode);


	/**
	 * The reference of a a single data point.
	 */
	class DataPoint : public insieme::utils::less_than_comparable<DataPoint> {

		insieme::core::ExpressionPtr expr;

	public:

		DataPoint(const insieme::core::ExpressionPtr& expr)
			: expr(expr) {
			assert_true(expr);
		}

		bool operator==(const DataPoint& other) const;

		bool operator<(const DataPoint& other) const;

		// add pretty printer support
		friend std::ostream& operator<<(std::ostream& out, const DataPoint& point);

	};

	/**
	 * A range of data points, spanning between two points.
	 */
	class DataSpan : public insieme::utils::less_than_comparable<DataSpan> {

		DataPoint from;

		DataPoint to;

	public:

		DataSpan(const DataPoint& point)
			: from(point), to(point) {}

		DataSpan(const DataPoint& from, const DataPoint& to)
			: from(from), to(to) {}

		bool operator==(const DataSpan& other) const;

		bool operator<(const DataSpan& other) const;

		// add pretty printer support
		friend std::ostream& operator<<(std::ostream& out, const DataSpan& span);

	};

	/**
	 * A range of data points formed by the union of a set of DataSpans.
	 */
	class DataRange : public insieme::utils::less_than_comparable<DataRange> {

	public:

		using set_type = insieme::analysis::cba::Set<DataSpan>;

	private:

		// the list of covered spans, or uninitialized if spans are unknown
		set_type spans;

	public:

		/**
		 * Creates a new, unknown data range.
		 */
		DataRange()
			: spans(set_type::getUniversal()) {}

		DataRange(const insieme::core::ExpressionPtr& expr)
			: DataRange(DataPoint(expr)) {}

		DataRange(const DataPoint& point)
			: DataRange(DataSpan(point)) {}

		DataRange(const DataSpan& span)
			: spans(std::set<DataSpan>{ span }) {}

		DataRange(const set_type& spans)
			: spans(spans) {}


		// -- factory functions --

		static DataRange unknown();

		static DataRange empty();

		static DataPoint term(const insieme::core::ExpressionPtr& expr);

		static DataSpan span(const DataPoint& from, const DataPoint& to);

		static DataRange merge(const std::vector<DataRange>& ranges);

		template<typename ... Ranges>
		static DataRange merge(const Ranges& ... ranges) {
			return merge(std::vector<DataRange>{ ranges ... });
		}

		// -- observers --

		bool isEmpty() const {
			return spans.empty();
		}

		bool isUnknown() const {
			return spans.isUniversal();
		}

		bool operator==(const DataRange& other) const;

		bool operator<(const DataRange& other) const;

		// add pretty printer support
		friend std::ostream& operator<<(std::ostream& out, const DataRange& range);

	};


	/**
	 * A data requirement, referencing a data item, a (sub-)range of
	 * the data item and the mode of access.
	 */
	class DataRequirement {

		// the expression addressing the data item reference
		insieme::core::ExpressionPtr dataItem;

		// the expression computing the range
		DataRange range;

		// the access mode
		AccessMode mode;

	public:

		DataRequirement(const insieme::core::ExpressionPtr& dataItem, const DataRange& range, AccessMode mode)
			: dataItem(dataItem), range(range), mode(mode) {
			assert_true(dataItem) << "Data item reference must not be null!";
		}

		bool operator==(const DataRequirement& other) const {
			// shortcut for the same object id
			if (this == &other) return true;
			// compare individual fields
			return mode == other.mode && *dataItem == *other.dataItem && range == other.range;
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
			return range < other.range;
		}

		friend std::ostream& operator<<(std::ostream& out, const DataRequirement& req);

	};


	/**
	 * The result of a data requirement analysis, summarizing the data accessed
	 * by a given code fragment.
	 */
	class DataRequirements {

	public:

		using set_type = insieme::analysis::cba::Set<DataRequirement>;

	private:

		// the data requirements, if known (if not set, requirements are not known)
		set_type requirements;

	public:

		DataRequirements() : requirements(set_type::getUniversal()) {}

		DataRequirements(const set_type& requirements)
			: requirements(requirements) {}

		bool isUnknown() const {
			return requirements.isUniversal();
		}

		bool empty() const {
			return requirements.empty();
		}

		bool isUniverse() const {
			return requirements.isUniversal();
		}

		auto begin() const {
			return requirements.begin();
		}

		auto end() const {
			return requirements.end();
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
	boost::optional<DataRequirements> getDataRequirements(AnalysisContext& context, const insieme::core::StatementPtr& stmt);

	/**
	 * A convenience entry for the data requirement analysis, producing a temporary analysis context.
	 *
	 * @param stmt the statement to be analyzed
	 * @return the list of data requirements obtained for the given statement unless timed
	 */
	boost::optional<DataRequirements> getDataRequirements(const insieme::core::StatementPtr& stmt);

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
