
#pragma once

#include <string>
#include <functional>
#include <regex>
#include <memory>

#include "insieme/core/ir_node.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/extensions/mapping_frontend_extension.h"


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
		namespace fed = insieme::frontend::extensions::detail;

		extern const std::vector<fed::FilterMapper> exprMappings;


		//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Mapping functions/classes

		/// Utility for the mapping of copy|move ctor calls (or anything which just returns the first argument)
		core::ExpressionPtr mapToFirstArgument(const fed::ClangExpressionInfo&);


		/// Utility for the mapping of the call to done without arguments
		core::ExpressionPtr mapDoneCall(const fed::ClangExpressionInfo&);


		/// Utility for the specification of simple call mappings (C++ to IR)
		class SimpleCallMapper {
			const string targetIRString;
			const bool derefThisArg;
			core::ExpressionPtr buildCallWithDefaultParamConversion(const core::ExpressionPtr& callee, const fed::ClangExpressionInfo& exprInfo);

		  protected:
			virtual core::ExpressionPtr convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionPtr generateCallee(const fed::ClangExpressionInfo& exprInfo);
			virtual core::ExpressionPtr postprocessCall(const fed::ClangExpressionInfo& exprInfo, const core::ExpressionPtr& translatedCall);

		  public:
			SimpleCallMapper(const string& targetIRString, bool derefThisArg = false) : targetIRString(targetIRString), derefThisArg(derefThisArg) {}
			core::ExpressionPtr operator()(const fed::ClangExpressionInfo& exprInfo);
		};


		/// Utility for the specification of task_reference constructor calls
		core::ExpressionPtr mapToTaskRefDone(const fed::ClangExpressionInfo&);


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
			virtual core::ExpressionPtr generateCallee(const fed::ClangExpressionInfo& exprInfo) override;
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args,
			                                                     insieme::frontend::conversion::Converter& converter) override;
			virtual core::ExpressionPtr postprocessCall(const fed::ClangExpressionInfo& exprInfo, const core::ExpressionPtr& translatedCall) override;

			virtual core::ExpressionPtr buildWrapper(const core::ExpressionPtr&) = 0;
			virtual core::ExpressionPtr buildDepWrapper(const core::ExpressionPtr&) = 0;

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
			virtual core::ExpressionPtr buildDepWrapper(const core::ExpressionPtr&) override;
		};

		/// Utility to map the call operator call of precfun objects
		class PrecFunCallMapper : public RecOrPrecFunCallMapper {
		  public:
			PrecFunCallMapper() : RecOrPrecFunCallMapper() { }
		  protected:
			virtual core::ExpressionPtr buildWrapper(const core::ExpressionPtr&) override;
			virtual core::ExpressionPtr buildDepWrapper(const core::ExpressionPtr&) override;
		};


		/// Utility to map the call to fun
		core::ExpressionPtr mapToBuildRecFun(const fed::ClangExpressionInfo& exprInfo);


		/// Utility for the aggregation of function arguments into a tuple
		core::ExpressionPtr aggregateArgumentsToTuple(const fed::ClangExpressionInfo&);


		/// Utility for the aggregation of arguments into a list
		core::ExpressionPtr aggregateArgumentsToList(const fed::ClangExpressionInfo&);


		/// Utilities to map the call to prec
		core::ExpressionPtr mapPrecRecDefs(const fed::ClangExpressionInfo& exprInfo);
		core::ExpressionPtr mapPrecFun(const fed::ClangExpressionInfo& exprInfo);
		/// Utility to map the call to prec while passing the three lambdas directly - without a fun call
		core::ExpressionPtr mapPrecDirect(const fed::ClangExpressionInfo& exprInfo);

	}
}
}
}
