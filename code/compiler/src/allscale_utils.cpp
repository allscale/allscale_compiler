#include "allscale/compiler/allscale_utils.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/materialize.h"

#include "insieme/utils/name_mangling.h"

namespace allscale {
namespace compiler {
namespace utils {

	namespace core = insieme::core;

	namespace {
		core::StructPtr getStructFromNode(const core::NodePtr& node) {
			if(auto s = node.isa<core::StructPtr>()) return s;
			auto t = node.isa<core::TypePtr>();
			if(auto exp = node.isa<core::ExpressionPtr>()) t = exp->getType();
			if(!t) return nullptr;
			if(core::analysis::isRefType(t)) t = core::analysis::getReferencedType(t);
			auto tt = t.isa<core::TagTypePtr>();
			if(tt && tt->isStruct()) return tt->getStruct();
			return nullptr;
		}
	}

	bool hasCallOperator(const core::NodePtr& node) {
		auto sourceLambda = getStructFromNode(node);
		if(!sourceLambda) return false;
		return ::any(sourceLambda->getMemberFunctions()->getMembers(), [](const auto& memFun) {
			return memFun->getNameAsString() == insieme::utils::getMangledOperatorCallName();
		});
	}

	core::MemberFunctionPtr extractCallOperator(const core::NodePtr& node) {
		auto sourceLambda = getStructFromNode(node);
		assert_true(sourceLambda);
		auto mems = sourceLambda->getMemberFunctions();
		for(const auto& mem : mems) {
			if(mem->getNameAsString() == insieme::utils::getMangledOperatorCallName()) { return mem; }
		}

		assert_fail() << "Could not extract call operator from lambda:\n" << dumpPretty(sourceLambda);
		return {};
	}

	core::FunctionTypePtr extractCallOperatorType(const core::NodePtr& node) {
		auto sourceLambda = getStructFromNode(node);
		assert_true(sourceLambda);
		auto mem = extractCallOperator(sourceLambda);
		if(!mem) {
			assert_fail() << "Could not extract type from callable lambda:\n" << dumpPretty(sourceLambda);
		}

		return mem->getType().as<core::FunctionTypePtr>();
	}

	core::LambdaExprPtr getCallOperatorImplementation(const core::ExpressionPtr& lambda) {

		auto cppLambdaType = lambda->getType();
		if (core::lang::isReference(cppLambdaType)) {
			cppLambdaType = core::lang::ReferenceType(cppLambdaType).getElementType();
		}
		assert_true(cppLambdaType.isa<core::TagTypePtr>()) << cppLambdaType;

		// get call operator member
		auto tagType = cppLambdaType.as<core::TagTypePtr>();
		core::MemberFunctionPtr callOperator;
		for(const auto& cur : tagType->getRecord()->getMemberFunctions()) {
			if ("IMP__operator_call_" == cur->getNameAsString()) {
				callOperator = tagType->peel(cur);
				break;
			}
		}
		assert_true(callOperator) << "No call operator found in lambda!";

		// extract a lambda
		auto impl = callOperator->getImplementation().isa<core::LambdaExprPtr>();

		assert_true(impl) << "Lambda implementation must not be abstract!";

		return impl;
	}

	namespace {
		bool canCopyWithCtor(const core::TypePtr& type) {
			if(core::lang::isReference(type)) {
				const auto& innerType = core::analysis::getReferencedType(type);
				// we can copy/move everything that has a matching ctor
				if(const auto& tagType = innerType.isa<core::TagTypePtr>()) {
					return core::analysis::hasCopyConstructor(tagType);
				}
				// or is a (non reference) generic type, tuple or TagTypeReference
				return !core::lang::isReference(innerType) && (innerType.isa<core::GenericTypePtr>() || innerType.isa<core::TupleTypePtr>() || innerType.isa<core::TagTypeReferencePtr>());
			}
			return false;
		}
	}

	core::DeclarationPtr buildPassByValueDeclaration(const core::ExpressionPtr& exprIn) {
		core::IRBuilder builder(exprIn->getNodeManager());
		auto expr = exprIn;

		// if it should be moved
		if(core::lang::isCppRValueReference(expr)) {
			// create a decl of non-const plain type and init it with the expression
			core::lang::ReferenceType declRefType(expr);
			declRefType.setConst(false);
			declRefType.setKind(core::lang::ReferenceType::Kind::Plain);
			return builder.declaration(declRefType.toType(), expr);

			// if we do have a reference here
		} else if(core::lang::isReference(expr)) {
			// and we can copy it with a ctor
			if(canCopyWithCtor(expr->getType())) {
				// create a decl of non-const plain type and init it with a cast to const cpp_ref
				core::lang::ReferenceType declRefType(expr);
				declRefType.setConst(false);
				declRefType.setKind(core::lang::ReferenceType::Kind::Plain);
				core::lang::ReferenceType refType(expr);
				refType.setConst(true);
				refType.setKind(core::lang::ReferenceType::Kind::CppReference);
				expr = core::lang::buildRefCast(expr, refType.toType());
				return builder.declaration(declRefType.toType(), expr);

				// otherwise deref the expression
			} else {
				expr = builder.deref(expr);
			}
		}
		return builder.declaration(core::transform::materialize(expr->getType()), expr);
	}

}
}
}
