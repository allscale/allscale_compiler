#pragma once

#include "insieme/core/ir_node.h"

namespace allscale {
namespace compiler {
namespace utils {

	bool hasCallOperator(const insieme::core::NodePtr& node);

	insieme::core::MemberFunctionPtr extractCallOperator(const insieme::core::NodePtr& node);

	insieme::core::FunctionTypePtr extractCallOperatorType(const insieme::core::NodePtr& node);

	insieme::core::LambdaExprPtr getCallOperatorImplementation(const insieme::core::ExpressionPtr& lambda);

	bool isCopyable(const insieme::core::TypePtr& type);

	insieme::core::DeclarationPtr buildPassByValueDeclaration(const insieme::core::ExpressionPtr& expr);

}
}
}
