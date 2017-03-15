
#pragma once

#include <string>
#include <map>
#include <functional>

#include "insieme/frontend/clang.h"
#include "insieme/core/ir_node.h"


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

	namespace detail {

		namespace core = insieme::core;

		/// implements the actual mapping from clang exprs to IR
		insieme::core::ExpressionPtr mapExpr(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter);


		//////// implementation details ------------------------------------------------------------------------------------------------------------------

		using CallMapper = std::function<insieme::core::ExpressionPtr(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter)>;

		/// Utility for the specification of noop call mappings (C++ to IR)
		class NoopCallMapper {
		  public:
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter);
		};

		/// Utility for the specification of simple call mappings (C++ to IR)
		class SimpleCallMapper {
			const string targetIRString;
			bool derefThisArg;
			core::ExpressionPtr buildCallWithDefaultParamConversion(const core::ExpressionPtr& callee, const clang::CallExpr* call,
			                                                        insieme::frontend::conversion::Converter& converter);

		  protected:
			virtual core::ExpressionPtr convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionPtr generateCallee(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionPtr postprocessCall(const clang::CallExpr* call, const core::ExpressionPtr& translatedCall,
			                                            insieme::frontend::conversion::Converter& converter);

		  public:
			SimpleCallMapper(const string& targetIRString, bool derefThisArg = false) : targetIRString(targetIRString), derefThisArg(derefThisArg) {}
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter);
		};

		/// Utility for the specification of treeture/task aggregation (C++ to IR)
		/// same as SimpleCallMapper, but skips std::move and converts completed_task to treeture as required
		/// also postprocesses argument list in order to generate empty dependencies list if none is available
		class AggregationCallMapper : public SimpleCallMapper {
			bool requiresDependencies = false;

		  protected:
			virtual core::ExpressionPtr convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) override;
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args,
			                                                     insieme::frontend::conversion::Converter& converter) override;

		  public:
			AggregationCallMapper(const string& targetIRString, bool requiresDependencies = false)
				: SimpleCallMapper(targetIRString, true), requiresDependencies(requiresDependencies) {}
		};

		/// Utility to map the call operator call of recfun and precfun objects
		class RecOrPrecFunCallMapper : public SimpleCallMapper {
		  protected:
			virtual core::ExpressionPtr generateCallee(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) override;
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args,
			                                                     insieme::frontend::conversion::Converter& converter) override;
			virtual core::ExpressionPtr postprocessCall(const clang::CallExpr* call, const core::ExpressionPtr& translatedCall,
			                                            insieme::frontend::conversion::Converter& converter) override;

			virtual core::ExpressionPtr buildWrapper(const core::ExpressionPtr&) = 0;

		  public:
			RecOrPrecFunCallMapper() : SimpleCallMapper("") { }
			virtual ~RecOrPrecFunCallMapper() {}
		};


		/// Utility to map the call operator call of recfun objects
		class RecFunCallMapper : public RecOrPrecFunCallMapper {
		  public:
			RecFunCallMapper() : RecOrPrecFunCallMapper() { }
		  protected:
			virtual core::ExpressionPtr buildWrapper(const core::ExpressionPtr&) override;
		};

		/// Utility to map the call operator call of precfun objects
		class PrecFunCallMapper : public RecOrPrecFunCallMapper {
		  public:
			PrecFunCallMapper() : RecOrPrecFunCallMapper() { }
		  protected:
			virtual core::ExpressionPtr buildWrapper(const core::ExpressionPtr&) override;
		};


		/// Utility to map the call to fun
		class FunConstructionMapper {
		  public:
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter);
		};

		/// Utility to map the call to prec
		class PrecMapper {
		  public:
			core::ExpressionPtr operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter);
		};
	}
}
}
}
