#include "allscale/compiler/analysis/data_requirement.h"

namespace allscale {
namespace compiler {
namespace analysis {

	// -- Utilities --

	std::ostream& operator<<(std::ostream& out, AccessMode mode) {
		if (mode == AccessMode::ReadOnly)  return out << "RO";
		if (mode == AccessMode::ReadWrite) return out << "RW";
		return out << "??";
	}

	std::ostream& operator<<(std::ostream& out, const DataRequirement& req) {
		return out << "Requirement { " << dumpOneLine(req.dataItem) << "[" << dumpOneLine(req.range) << "] " << req.mode << " }";
	}

	std::ostream& operator<<(std::ostream& out, const DataRequirements& reqs) {
		return out << reqs.requirements;
	}


	// -- Data Requirement Analysis --

	DataRequirements getDataRequirements(const insieme::core::StatementPtr& stmt) {
		AnalysisContext context;
		return getDataRequirements(context, stmt);
	}

	DataRequirements getDataRequirements(AnalysisContext&, const insieme::core::StatementPtr&) {
		// TODO: implement this analysis
		return DataRequirements();
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale

