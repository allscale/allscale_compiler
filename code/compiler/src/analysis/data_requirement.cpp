#include "allscale/compiler/analysis/data_requirement.h"

#include "insieme/analysis/cba/haskell/context.h"
#include "insieme/analysis/cba/common/set.h"

extern "C" {

	using namespace insieme::analysis::cba::haskell;
	using namespace allscale::compiler::analysis;

	AnalysisResult<DataRequirements*>* hat_hs_data_requirements(StablePtr ctx, const HaskellNodeAddress node_hs);

}

namespace allscale {
namespace compiler {
namespace analysis {

	using namespace insieme::core;
	using namespace insieme::utils;

	// -- Utilities --

	std::ostream& operator<<(std::ostream& out, AccessMode mode) {
		if (mode == AccessMode::ReadOnly)  return out << "RO";
		if (mode == AccessMode::ReadWrite) return out << "RW";
		return out << "??";
	}

	// - DataPoint -

	bool DataPoint::operator==(const DataPoint& other) const {
		if (this == &other) return true;
		return *expr == *other.expr;
	}

	bool DataPoint::operator<(const DataPoint& other) const {
		if (this == &other) return false;
		return *expr < *other.expr;
	}

	std::ostream& operator<<(std::ostream& out, const DataPoint& point) {
		return out << dumpOneLine(point.expr);
	}


	// - DataSpan -

	bool DataSpan::operator==(const DataSpan& other) const {
		if (this == &other) return true;
		return from == other.from && to == other.to;
	}

	bool DataSpan::operator<(const DataSpan& other) const {
		if (this == &other) return false;
		if (from < other.from) return true;
		if (from != other.from) return false;
		return to < other.to;
	}

	std::ostream& operator<<(std::ostream& out, const DataSpan& span) {
		// check whether it is a single point
		if (span.from == span.to) return out << span.from;
		return out << "span(" << span.from << "," << span.to << ")";
	}


	// - DataRange -

	bool DataRange::operator==(const DataRange& other) const {
		if (this == &other) return true;
		if (isUnknown() && other.isUnknown()) return true;
		if (isUnknown() != other.isUnknown()) return false;
		assert_false(isUnknown());
		assert_false(other.isUnknown());
		return spans == other.spans;
	}

	bool DataRange::operator<(const DataRange& other) const {
		if (this == &other) return false;
		if (isUnknown()) return !other.isUnknown();
		if (other.isUnknown()) return false;
		assert_false(isUnknown());
		assert_false(other.isUnknown());
		return spans < other.spans;
 	}

	std::ostream& operator<<(std::ostream& out, const DataRange& range) {

		// check some corner cases
		if (range.isUnknown()) return out << "unknown";
		if (range.isEmpty())   return out << "empty";
		if (range.spans.size() == 1) return out << *range.spans.begin();

		// make sure strings are printed in fixed order
		std::vector<std::string> entries;
		for(const auto& cur : range.spans) {
			entries.push_back(toString(cur));
		}
		std::sort(entries.begin(),entries.end());
		return out << "union{" << join(",",entries) << "}";
	}


	DataRange DataRange::unknown() {
		return {};
	}

	DataRange DataRange::empty() {
		DataRange res = unknown();
		res.spans = std::set<DataSpan>();
		return res;
	}

	DataPoint DataRange::term(const insieme::core::ExpressionPtr& expr) {
		return expr;
	}

	DataSpan DataRange::span(const DataPoint& from, const DataPoint& to) {
		return DataSpan(from,to);
	}

	DataRange DataRange::merge(const std::vector<DataRange>& ranges) {

		// check whether there is any unknown value included
		if (any(ranges,[](const DataRange& r) { return r.isUnknown(); })) return unknown();

		// start with an empty result
		DataRange res = empty();

		// implicitly merge nested union nodes
		for(const auto& cur : ranges) {
			for(const auto& span : cur.spans) {
				res.spans.insert(span);
			}
		}

		// that's it
		return res;
	}


	std::ostream& operator<<(std::ostream& out, const DataRequirement& req) {
		return out << "Requirement { " << dumpOneLine(req.dataItem) << "[" << req.range << "] " << req.mode << " }";
	}

	std::ostream& operator<<(std::ostream& out, const DataRequirements& reqs) {
		if (reqs.isUnknown()) return out << "unknown";
		return out << reqs.requirements;
	}


	// -- Data Requirement Analysis --

	boost::optional<DataRequirements> getDataRequirements(const StatementPtr& stmt) {
		AnalysisContext context;
		return getDataRequirements(context, stmt);
	}

	boost::optional<DataRequirements> getDataRequirements(AnalysisContext& ctx, const StatementPtr& stmt) {
		auto node_hs = ctx.resolveNodeAddress(NodeAddress(stmt));
		auto result = hat_hs_data_requirements(ctx.getHaskellContext(), node_hs);
		return ctx.unwrapResult(result);
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale

extern "C" {

	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace allscale::compiler::analysis;

	DataPoint* hat_c_mk_data_point(NodePtr* node) {
		DataPoint* dp = new DataPoint(std::move(node->as<ExpressionPtr>()));
		return dp;
	}

	void hat_c_del_data_point(DataPoint* d) {
		delete d;
	}

	DataSpan* hat_c_mk_data_span(DataPoint* from, DataPoint* to) {
		DataSpan* ds = new DataSpan(std::move(*from), std::move(*to));
		return ds;
	}

	void hat_c_del_data_span(DataSpan* d) {
		delete d;
	}

	DataRange::set_type* hat_c_mk_data_span_set(DataSpan* spans[], long long size) {
		return DataRange::set_type::fromArray(spans, size);
	}

	void hat_c_del_data_span_set(DataRange::set_type* s) {
		delete s;
	}

	DataRange* hat_c_mk_data_range(DataRange::set_type* dss) {
		DataRange* dr = new DataRange(std::move(*dss));
		return dr;
	}

	void hat_c_del_data_range(DataRange* d) {
		delete d;
	}

	DataRequirement* hat_c_mk_data_requirement(NodePtr* node, DataRange* range, int accessMode) {
		DataRequirement* dr = new DataRequirement(
			std::move(node->as<ExpressionPtr>()),
			std::move(*range),
			static_cast<AccessMode>(accessMode)
		);
		return dr;
	}

	DataRequirements::set_type* hat_c_mk_data_requirement_set(DataRequirement* reqs[], long long size) {
		return DataRequirements::set_type::fromArray(reqs, size);
	}

	void hat_c_del_data_requirement_set(DataRequirements::set_type* d) {
		delete d;
	}

	DataRequirements* hat_c_mk_data_requirements(DataRequirements::set_type* drs) {
		DataRequirements* dr = new DataRequirements(std::move(*drs));
		return dr;
	}

}
