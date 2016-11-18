#include "allscale/compiler/utils.h"

#include "insieme/core/ir_types.h"

#include "insieme/utils/name_mangling.h"

using namespace insieme::core;

namespace allscale {
namespace compiler {
namespace utils {

	FunctionTypePtr extractCallOperatorType(const StructPtr& sourceLambda) {
		auto mems = sourceLambda->getMemberFunctions();
		for(const auto& mem : mems) {
			if(mem->getNameAsString() == insieme::utils::mangle("operator()")) { return mem->getType().as<FunctionTypePtr>(); }
		}

		assert_fail() << "Could not extract type from callable lambda:\n" << dumpPretty(sourceLambda);
		return {};
	}

}
}
}