#include "allscale/compiler/analysis/data_requirement.h"

#include "insieme/analysis/cba/haskell/context.h"
#include "insieme/analysis/cba/common/set.h"

extern "C" {

	using namespace insieme::analysis::cba::haskell;
	using namespace allscale::compiler::analysis;

	DataRequirements* hat_hs_data_requirements(StablePtr ctx, const HaskellNodeAddress node_hs);

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
		return out << *point.expr;
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
		return out << "span(" << span.from << "-" << span.to << ")";
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

	DataRequirements getDataRequirements(const StatementPtr& stmt) {
		AnalysisContext context;
		return getDataRequirements(context, stmt);
	}

	DataRequirements getDataRequirements(AnalysisContext& ctx, const StatementPtr& stmt) {
		auto node_hs = ctx.resolveNodeAddress(NodeAddress(stmt));
		DataRequirements* res_ptr = hat_hs_data_requirements(ctx.getHaskellContext(), node_hs);
		assert_true(res_ptr);

		DataRequirements res = *res_ptr;
		delete res_ptr;

		return res;
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale

extern "C" {

	using namespace insieme::core;
	using namespace insieme::analysis;
	using namespace allscale::compiler::analysis;

	DataPoint* hat_c_mk_data_point(NodePtr* node) {
		DataPoint* dp = new DataPoint(node->as<ExpressionPtr>());
		delete node;
		return dp;
	}

	DataSpan* hat_c_mk_data_span(DataPoint* from, DataPoint* to) {
		DataSpan* ds = new DataSpan(*from, *to);
		delete from;
		delete to;
		return ds;
	}

	using DataSpanSet = cba::Set<DataSpan>;
	DataSpanSet* hat_c_mk_data_span_set(const DataSpan* spans[], long long size) {
		if(size < 0) {
			return new DataSpanSet(DataSpanSet::getUniversal());
		}

		auto dss = new DataSpanSet();
		for(int i = 0; i < size; i++) {
			dss->insert(*spans[i]);
			delete spans[i];
		}

		return dss;
	}

	DataRange* hat_c_mk_data_range(DataSpanSet* dss) {
		DataRange* dr = new DataRange(*dss);
		delete dss;
		return dr;
	}

	DataRequirement* hat_c_mk_data_requirement(NodePtr* node, DataRange* range, int accessMode) {
		DataRequirement* dr = new DataRequirement(
			node->as<ExpressionPtr>(),
			*range,
			static_cast<AccessMode>(accessMode)
		);
		delete node;
		delete range;
		return dr;
	}

	using DataRequirementSet = cba::Set<DataRequirement>;
	DataRequirementSet* hat_c_mk_data_requirement_set(const DataRequirement* reqs[], long long size) {
		if(size < 0) {
			return new DataRequirementSet(DataRequirementSet::getUniversal());
		}

		auto drs = new DataRequirementSet();
		for(int i = 0; i < size; i++) {
			drs->insert(*reqs[i]);
			delete reqs[i];
		}

		return drs;
	}

	DataRequirements* hat_c_mk_data_requirements(DataRequirementSet* drs) {
		DataRequirements* dr = new DataRequirements(*drs);
		delete drs;
		return dr;
	}

}