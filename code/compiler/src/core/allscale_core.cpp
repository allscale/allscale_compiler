#include "allscale/compiler/core/allscale_core.h"

#include "allscale/compiler/core/cpp_lambda_to_ir_conversion.h"
#include "allscale/compiler/core/data_item_conversion.h"
#include "allscale/compiler/core/prec_to_work_item_conversion.h"

namespace allscale {
namespace compiler {
namespace core {

	std::ostream& operator<<(std::ostream& out, const ConversionReport& report) {

		// print issues, one after another
		for(const auto& cur : report.issues) {
			const auto& precCall = cur.first;
			const auto& issues = cur.second;

			// if there are no issues to report => skip this one
			if (issues.empty()) continue;

			out << "Processed parallel region ";
			reporting::prettyPrintLocation(out,precCall);
			for(const auto& cur : issues) {
				out << "\t";
				reporting::prettyPrintIssue(out,cur);
				out << "\n";
			}
			out << "\n";
		}

		// done
		return out;
	}

	ConversionResult convert(const insieme::core::ProgramPtr& program, const ProgressCallback& callback) {

		// Step 1: convert C++ lambdas to IR
		auto res = convertCppLambdaToIR(program);
		callback(ProgressUpdate { "C++ code simplifications ", 1, 1 });

		// Step 2: introduce data item references
		res = convertDataItemReferences(program, callback);

		// Step 3: convert prec calls
		res = convertPrecToWorkItem(res, callback);

		// Step 4: convert the entry point into a work item

		// Step 5: add default constructors to all closure types


//		be::makePreProcessor<CppLambdaToIRConverter>(),
//		be::makePreProcessor<PrecConverter>(),
//		be::makePreProcessor<EntryPointWrapper>(),
//		be::makePreProcessor<ClosureDefaultConstructorEnforcer>()

		return { ConversionReport(), res };
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
