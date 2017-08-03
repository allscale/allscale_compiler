#pragma once

#include <map>
#include <ostream>

#include "insieme/core/ir.h"

#include "allscale/compiler/reporting/reporting.h"

namespace allscale {
namespace compiler {
namespace core {

	using PrecCall = insieme::core::CallExprAddress;

	struct ConversionReport {

		std::map<PrecCall,reporting::Issues> issues;

		friend std::ostream& operator<<(std::ostream& out, const ConversionReport& report);
	};

	struct ConversionResult {

		ConversionReport report;

		insieme::core::ProgramPtr result;

		bool successful() const {
			return result;
		}

	};

	struct ProgressUpdate {

		std::string msg;

		int completedSteps;

		int totalSteps;

		ProgressUpdate(const std::string& msg) : msg(msg), completedSteps(0), totalSteps(0) {}

		ProgressUpdate(const std::string& msg, int completed, int total)
			: msg(msg), completedSteps(completed), totalSteps(total) {}

	};

	using ProgressCallback = std::function<void(const ProgressUpdate&)>;

	inline void ignoreProgress(const ProgressUpdate&) {};

	ConversionResult convert(const insieme::core::ProgramPtr& program, const ProgressCallback& callback = ignoreProgress);

} // end namespace core
} // end namespace compiler
} // end namespace allscale
