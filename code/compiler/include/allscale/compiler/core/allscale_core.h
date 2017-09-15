#pragma once

#include <map>
#include <ostream>

#include <boost/property_tree/ptree.hpp>

#include "insieme/core/ir.h"

#include "allscale/compiler/reporting/reporting.h"

namespace allscale {
namespace compiler {
namespace core {

	// -- Declarations -----------------------------------------------------------------

	/**
	 * A conversion result is merely the combination of a conversion report and the
	 * converted code structure.
	 */
	struct ConversionResult;

	/**
	 * A class modeling a progress message from the conversion process.
	 */
	struct ProgressUpdate;

	/**
	 * The type of a progress callback function, to be registered to the conversion
	 * to monitore progress.
	 */
	using ProgressCallback = std::function<void(const ProgressUpdate&)>;

	namespace detail {

		// the default progress callback function, ignoring everything
		inline void ignoreProgress(const ProgressUpdate&) {};

	}

	/**
	 * The actual core AllScale conversion function, converting a program utilizing
	 * the prec operator of the AllScale API into a program utilizing the primitives
	 * supported by the AllScale runtime system.
	 *
	 * @param code the code fragment to be converted
	 * @param callback an optional parameter for monitoring the conversion process
	 */
	ConversionResult convert(const insieme::core::NodePtr& code, const ProgressCallback& callback = detail::ignoreProgress);

	// -- Definitions ------------------------------------------------------------------

	struct ConversionResult {

		// the report generated during the generation
		reporting::ConversionReport report;

		// the converted code fragment
		insieme::core::NodePtr result;

		bool successful() const {
			return result;
		}

	};

	struct ProgressUpdate {

		// an update message
		std::string msg;

		// a step counter (or 0 if no step to be reported)
		int completedSteps;

		// the number of total steps (or 0 if no steps to be reported)
		int totalSteps;

		ProgressUpdate(const std::string& msg) : msg(msg), completedSteps(0), totalSteps(0) {}

		ProgressUpdate(const std::string& msg, int completed, int total)
			: msg(msg), completedSteps(completed), totalSteps(total) {}

	};

} // end namespace core
} // end namespace compiler
} // end namespace allscale
