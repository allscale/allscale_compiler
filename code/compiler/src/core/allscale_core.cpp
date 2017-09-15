#include "allscale/compiler/core/allscale_core.h"

#include <ctime>

#include <fstream>
#include <sstream>

#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"

#include "allscale/compiler/env_vars.h"
#include "allscale/compiler/core/cpp_lambda_to_ir_conversion.h"
#include "allscale/compiler/core/global_constant_propagation.h"
#include "allscale/compiler/core/data_item_conversion.h"
#include "allscale/compiler/core/prec_to_work_item_conversion.h"

namespace allscale {
namespace compiler {
namespace core {

	ConversionResult convert(const insieme::core::NodePtr& code, const ProgressCallback& callback) {

		// make sure the input is correct
		assert_correct_ir(code);

		// Step 1: convert C++ lambdas to IR
		callback(ProgressUpdate("Pre-processing C++ lambdas ..."));
		auto res = convertCppLambdaToIR(code);

		// Step 2: performing global constant propagation
		if(!std::getenv(ALLSCALE_SKIP_GLOBAL_CONSTANT_PROPAGATION)) {
			callback(ProgressUpdate("Propagating global constants ..."));
			res = propagateGlobalConstants(res);
		}

		// Step 3: introduce data item references
		res = convertDataItemReferences(res, callback);

		// Step 4: convert prec calls
		auto precConversionResult = convertPrecToWorkItem(res, callback);
		res = precConversionResult.result;

		// Step 5: convert the entry point into a work item
		// TODO: move this step from the backend to the core

		// Step 6: add default constructors to all closure types
		// TODO: move this step from the backend to the core

		// make sure the result is correct
		assert_correct_ir(res);

		return { precConversionResult.report, res };
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
