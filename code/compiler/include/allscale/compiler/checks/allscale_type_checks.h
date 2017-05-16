#pragma once

#include "insieme/core/checks/ir_checks.h"

namespace allscale {
namespace compiler {
namespace checks {

#include "insieme/core/checks/check_macros.inc"

	SIMPLE_CHECK(CppLambdaToClosure, CallExpr , false);
	SIMPLE_CHECK(CppLambdaToLambda, CallExpr , false);

#undef SIMPLE_CHECK

} // end namespace checks
} // end namespace compiler
} // end namespace allscale
