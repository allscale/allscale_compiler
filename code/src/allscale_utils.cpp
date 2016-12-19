#include "allscale/compiler/allscale_utils.h"

#include "insieme/core/ir_types.h"

#include "insieme/utils/name_mangling.h"

using namespace insieme::core;

namespace allscale {
namespace compiler {
namespace utils {

	bool hasCallOperator(const StructPtr& sourceLambda) {
		return ::any(sourceLambda->getMemberFunctions()->getMembers(), [](const auto& memFun) {
			return memFun->getNameAsString() == insieme::utils::getMangledOperatorCallName();
		});
	}

	MemberFunctionPtr extractCallOperator(const StructPtr& sourceLambda) {
		auto mems = sourceLambda->getMemberFunctions();
		for(const auto& mem : mems) {
			if(mem->getNameAsString() == insieme::utils::getMangledOperatorCallName()) { return mem; }
		}

		assert_fail() << "Could not extract call operator from lambda:\n" << dumpPretty(sourceLambda);
		return {};
	}

	FunctionTypePtr extractCallOperatorType(const StructPtr& sourceLambda) {
		auto mem = extractCallOperator(sourceLambda);
		if(!mem) {
			assert_fail() << "Could not extract type from callable lambda:\n" << dumpPretty(sourceLambda);
		}

		return mem->getType().as<FunctionTypePtr>();
	}

}
}
}
