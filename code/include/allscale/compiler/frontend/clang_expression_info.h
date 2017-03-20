
#pragma once

#include <vector>

#include "insieme/frontend/clang.h"


namespace insieme {
namespace frontend {
namespace conversion {
	class Converter;
}
}
}

namespace allscale {
namespace compiler {
namespace frontend {

	struct ClangExpressionInfo {

		const unsigned numArgs;

		const std::vector<const clang::Expr*> args;

		const clang::QualType clangType;

		const clang::Expr* implicitObjectArgument;

		const bool isMemberCall;

		const bool isOperatorCall;

		const bool isConstructorCall;

		clang::SourceLocation locStart;

		insieme::frontend::conversion::Converter& converter;

		static ClangExpressionInfo getClangExpressionInfo(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter);

	  private:
		ClangExpressionInfo(const unsigned numArgs, const std::vector<const clang::Expr*> args, const clang::QualType clangType, const clang::Expr* implicitObjectArgument,
		                    const bool isMemberCall, const bool isOperatorCall, const bool isConstructorCall, clang::SourceLocation locStart,
		                    insieme::frontend::conversion::Converter& converter) :
		                    	numArgs(numArgs), args(args), clangType(clangType), implicitObjectArgument(implicitObjectArgument),
		                    	isMemberCall(isMemberCall), isOperatorCall(isOperatorCall),
		                    	isConstructorCall(isConstructorCall), converter(converter) { }
	};
}
}
}
