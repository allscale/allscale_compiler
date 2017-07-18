#include <gtest/gtest.h>

#include "allscale/compiler/analysis/diagnostics.h"

#include "insieme/core/ir_builder.h"

using namespace insieme::core;
using namespace insieme::analysis::cba::haskell;

namespace allscale {
namespace compiler {
namespace analysis {

	TEST(Diagnostic, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto comp = NodeAddress(builder.parseStmt(R"(
			{
				auto ref = ref_decl(type_lit(ref<array<int<4>, 21u>>));
				*ref;
			}
		)")).as<CompoundStmtAddress>();

		Issues issues = runDiagnostics(comp[1]);
		EXPECT_EQ(2, issues.size());

		for(const auto& issue : issues) {
			std::cout << toString(issue.getTarget())
			          << " [" << toString(issue.getSeverity()) << "] "
			          << issue.getMessage() << "\n";
		}
	}

} // end namespace analysis
} // end namespace compiler
} // end namespace allscale
