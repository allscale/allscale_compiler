#include <gtest/gtest.h>

#include <algorithm>
#include <iostream>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/dump/json_dump.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/core/allscale_core.h"
#include "allscale/compiler/core/prec_to_work_item_conversion.h"

#include "../test_utils.inc"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	namespace {

		using namespace reporting;

		Issues collectDistributedMemoryIssues(const ConversionReport& report) {
			Issues issues;

			auto isDistMemIssue = [](const Issue& issue) {
				return issue.getCategory() == Category::DistributedMemory
					   && (issue.getSeverity() == Severity::Warning || issue.getSeverity() == Severity::Error);
			};

			for(const auto& p : report.issues) {
				std::copy_if(p.second.first.begin(), p.second.first.end(), std::inserter(issues, issues.end()), isDistMemIssue);
				for(const auto& pp : p.second.second) {
					std::copy_if(pp.second.begin(), pp.second.end(), std::inserter(issues, issues.end()), isDistMemIssue);
				}
			}

			// DEBUG
			//{
			//	for(const auto& issue : issues) {
			//		std::cout << issue << " (" << issue.getTarget() << ")\n";
			//	}
			//}

			return issues;
		}

	}

	TEST(DistMemConversion, Simple) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/core/prec.h"

			using namespace allscale::api::core;

			int main()
			{
				prec(
					[](int x) { return x < 2; },
					[](int x) { },
					[](int x, const auto& f) {
						if(x == 1) {
							return;
						}
					}
				)(16);
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		//dump::json::dumpIR("simple.json", prog);

		auto result = convertPrecToWorkItem(ConversionConfig{}, prog, detail::ignoreProgress);
		auto dist_mem_issues = collectDistributedMemoryIssues(result.report);

		ASSERT_TRUE(dist_mem_issues.empty()) << dist_mem_issues;
	}

	TEST(DistMemConversion, UseOfInvalidFunctions) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/core/prec.h"

			using namespace allscale::api::core;

			int main()
			{
				prec(
					[](int x) { return x < 2; },
					[](int x) { printf("foo"); },
					[](int x, const auto& f) {
						if(x == 1) {
							return;
						}
					}
				)(16);
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		auto result = convertPrecToWorkItem(ConversionConfig{}, prog, detail::ignoreProgress);
		auto dist_mem_issues = collectDistributedMemoryIssues(result.report);
		auto it = std::find_if(dist_mem_issues.begin(), dist_mem_issues.end(), [](const Issue& issue) {
			return issue.getErrorCode() == ErrorCode::CallToInvalidFunctionForDistributedMemory;
		});

		ASSERT_TRUE(it != dist_mem_issues.end());
	}

	TEST(DistMemConversion, UseOfGlobal) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/core/prec.h"

			using namespace allscale::api::core;

			int g = 42;

			int main()
			{
				prec(
					[](int x) { return x < 2; },
					[](int x) { g++; },
					[](int x, const auto& f) {
						if(x == 1) {
							return;
						}
					}
				)(16);
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		auto result = convertPrecToWorkItem(ConversionConfig{}, prog, detail::ignoreProgress);
		auto dist_mem_issues = collectDistributedMemoryIssues(result.report);
		auto it = std::find_if(dist_mem_issues.begin(), dist_mem_issues.end(), [](const Issue& issue) {
			return issue.getErrorCode() == ErrorCode::InvalidUseOfGlobalForDistributedMemory;
		});

		ASSERT_TRUE(it != dist_mem_issues.end()) << dist_mem_issues;
	}

	TEST(DistMemConversion, CaptureByRef) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/core/prec.h"

			using namespace allscale::api::core;

			int main()
			{
				int y = 42;

				prec(
					[&](int x) { return x < y; },
					[&](int x) { y++; },
					[](int x, const auto& f) {
						if(x == 1) {
							return;
						}
					}
				)(16);
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		auto result = convertPrecToWorkItem(ConversionConfig{}, prog, detail::ignoreProgress);
		auto dist_mem_issues = collectDistributedMemoryIssues(result.report);
		auto it = std::find_if(dist_mem_issues.begin(), dist_mem_issues.end(), [](const Issue& issue) {
			return issue.getErrorCode() == ErrorCode::RefOrPtrFoundInCaptureList;
		});

		ASSERT_TRUE(it != dist_mem_issues.end()) << dist_mem_issues;
	}

	TEST(DistMemConversion, CaptureByPtr) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/core/prec.h"

			using namespace allscale::api::core;

			int main()
			{
				int y   = 42;
				int *yp = &y;

				prec(
					[=](int x) { return x < *yp; },
					[](int x) { },
					[](int x, const auto& f) {
						if(x == 1) {
							return;
						}
					}
				)(16);
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		auto result = convertPrecToWorkItem(ConversionConfig{}, prog, detail::ignoreProgress);
		auto dist_mem_issues = collectDistributedMemoryIssues(result.report);
		auto it = std::find_if(dist_mem_issues.begin(), dist_mem_issues.end(), [](const Issue& issue) {
			return issue.getErrorCode() == ErrorCode::RefOrPtrFoundInCaptureList;
		});

		ASSERT_TRUE(it != dist_mem_issues.end()) << dist_mem_issues;
	}

	TEST(DistMemConversion, CaptureByValue) {
		NodeManager mgr;

		auto code = R"(
			#include "allscale/api/core/prec.h"

			using namespace allscale::api::core;

			int main()
			{
				int y   = 42;

				prec(
					[=](int x) { return x < y; },
					[](int x) { },
					[](int x, const auto& f) {
						if(x == 1) {
							return;
						}
					}
				)(16);
			}
		)";

		auto prog = frontend::parseCode(mgr,code);
		ASSERT_TRUE(prog);
		ASSERT_TRUE(checks::check(prog).empty()) << printer::dumpErrors(checks::check(prog));

		auto result = convertPrecToWorkItem(ConversionConfig{}, prog, detail::ignoreProgress);
		auto dist_mem_issues = collectDistributedMemoryIssues(result.report);
		auto it = std::find_if(dist_mem_issues.begin(), dist_mem_issues.end(), [](const Issue& issue) {
			return issue.getErrorCode() == ErrorCode::RefOrPtrFoundInCaptureList;
		});

		ASSERT_TRUE(it == dist_mem_issues.end()) << dist_mem_issues;
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
