#include "allscale/compiler/core/allscale_core.h"

#include <ctime>

#include <fstream>
#include <sstream>

#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"

#include "allscale/compiler/env_vars.h"
#include "allscale/compiler/core/allscale_optimizations.h"
#include "allscale/compiler/core/cpp_lambda_to_ir_conversion.h"
#include "allscale/compiler/core/global_constant_propagation.h"
#include "allscale/compiler/core/data_item_conversion.h"
#include "allscale/compiler/core/data_serialization.h"
#include "allscale/compiler/core/prec_to_work_item_conversion.h"

namespace allscale {
namespace compiler {
namespace core {

	ConversionResult convert(const insieme::core::NodePtr& code, const ProgressCallback& callback) {
		return convert(ConversionConfig(),code,callback);
	}

	ConversionResult convert(const ConversionConfig& config, const insieme::core::NodePtr& code, const ProgressCallback& callback) {

		// make sure the input is correct
		assert_correct_ir(code);

		// Step 1: convert C++ lambdas to IR
		callback(ProgressUpdate("Pre-processing C++ lambdas ..."));
		auto res = convertCppLambdaToIR(code);

		// the next steps are only required in the distributed memory case
		if (!config.sharedMemoryOnly) {

			// Step 2: performing global constant propagation
			if(!std::getenv(ALLSCALE_SKIP_GLOBAL_CONSTANT_PROPAGATION)) {
				callback(ProgressUpdate("Propagating global constants ..."));
				res = propagateGlobalConstants(res);
			}

			// Step 3: introduce data item references
			callback(ProgressUpdate("Converting Data Items ..."));
			res = convertDataItemReferences(res, callback);

			// Step 4: capture data item references by value
			callback(ProgressUpdate("Capturing Data-Item-References by value ..."));
			res = convertCapturedDataItemReferences(res,callback);

			// Step 5: hoist data item access calls out as far as possible from loops
			callback(ProgressUpdate("Optimizing DataItem accesses ..."));
			res = performDataItemGetLoopHoisting(res, callback);

			// Step 6: adding serialization code
			callback(ProgressUpdate("Adding serialization code ..."));
			res = addAutoSerializationCode(res,callback);

		}

		// Step 7: convert prec calls
		auto precConversionResult = convertPrecToWorkItem(config, res, callback);
		res = precConversionResult.result;

		// Step 8: convert the entry point into a work item
		// TODO: move this step from the backend to the core

		// make sure the result is correct
		assert_correct_ir(res);

		return { precConversionResult.report, res };
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
