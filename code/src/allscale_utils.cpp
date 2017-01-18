#include "allscale/compiler/allscale_utils.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/name_mangling.h"

using namespace insieme::core;

namespace allscale {
namespace compiler {
namespace utils {

	namespace {
		StructPtr getStructFromNode(const NodePtr& node) {
			if(auto s = node.isa<StructPtr>()) return s;
			auto t = node.isa<TypePtr>();
			if(auto exp = node.isa<ExpressionPtr>()) t = exp->getType();
			if(!t) return nullptr;
			if(insieme::core::analysis::isRefType(t)) t = insieme::core::analysis::getReferencedType(t);
			auto tt = t.isa<TagTypePtr>();
			if(tt && tt->isStruct()) return tt->getStruct();
			return nullptr;
		}
	}

	bool hasCallOperator(const NodePtr& node) {
		auto sourceLambda = getStructFromNode(node);
		if(!sourceLambda) return false;
		return ::any(sourceLambda->getMemberFunctions()->getMembers(), [](const auto& memFun) {
			return memFun->getNameAsString() == insieme::utils::getMangledOperatorCallName();
		});
	}

	MemberFunctionPtr extractCallOperator(const NodePtr& node) {
		auto sourceLambda = getStructFromNode(node);
		assert_true(sourceLambda);
		auto mems = sourceLambda->getMemberFunctions();
		for(const auto& mem : mems) {
			if(mem->getNameAsString() == insieme::utils::getMangledOperatorCallName()) { return mem; }
		}

		assert_fail() << "Could not extract call operator from lambda:\n" << dumpPretty(sourceLambda);
		return {};
	}

	FunctionTypePtr extractCallOperatorType(const NodePtr& node) {
		auto sourceLambda = getStructFromNode(node);
		assert_true(sourceLambda);
		auto mem = extractCallOperator(sourceLambda);
		if(!mem) {
			assert_fail() << "Could not extract type from callable lambda:\n" << dumpPretty(sourceLambda);
		}

		return mem->getType().as<FunctionTypePtr>();
	}

}
}
}
