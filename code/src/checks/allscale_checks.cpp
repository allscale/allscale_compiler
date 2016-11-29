#include "allscale/compiler/checks/allscale_checks.h"

#include <vector>

#include "insieme/core/checks/full_check.h"

using namespace insieme::core::checks;

namespace allscale {
namespace compiler {
namespace checks {

	namespace {

		CheckPtr buildFullCheck() {
			std::vector<CheckPtr> context_free_checks;
			context_free_checks.push_back(make_check<LambdaToClosureCheck>());

			return makeVisitOnce(combine(context_free_checks, true));
		}

	}

	insieme::core::checks::CheckPtr getFullCheck() {
		return combine(buildFullCheck(), insieme::core::checks::getFullCheck());
	}

	insieme::core::checks::MessageList check(const insieme::core::NodePtr& node) {
		return check(node, getFullCheck());
	}

} // end namespace checks
} // end namespace compiler
} // end namespace allscale
