
#pragma once

#include <string>
#include <functional>
#include <regex>
#include <memory>

#include "insieme/frontend/clang.h"
#include "insieme/core/ir_node.h"

#include "allscale/compiler/frontend/clang_expression_info.h"


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

		/// checks whether the given clang decl will is mapped
		bool isMapped(const clang::Decl* decl);


		//////// implementation details ------------------------------------------------------------------------------------------------------------------

		using CallMapper = std::function<insieme::core::ExpressionPtr(const ClangExpressionInfo&)>;

		/// A filter matching the qualified name of the passed FunctionDecl against a regex
		class RegexCallFilter {

		  protected:
			const std::string patternString;

			const std::regex pattern;

		  public:
			RegexCallFilter(const std::string& patternString) : patternString(patternString), pattern(std::regex(patternString)) { }

			virtual bool matches(const clang::FunctionDecl* funDecl) const;

			virtual const std::string getFilterRepresentation() const;
		};

		/// A filter matching the qualified name of the passed FunctionDecl against a regex and also comparing the number of parameters
		class NumParamRegexCallFilter : public RegexCallFilter {

			const unsigned numParams;

		  public:
			NumParamRegexCallFilter(const std::string& patternString, const unsigned numParams) : RegexCallFilter(patternString), numParams(numParams) { }

			virtual bool matches(const clang::FunctionDecl* funDecl) const override;

			virtual const std::string getFilterRepresentation() const override;
		};

		/// The mapper which combines a filter and the corresponding CallMapper
		class FilterMapper {

			const std::shared_ptr<RegexCallFilter> filter;

			const CallMapper mapper;

		  public:
			/// Constructs a FilterMapper with the given regexp
			FilterMapper(const char* filterString, const CallMapper& mapper)
					: filter(std::make_shared<RegexCallFilter>(filterString)), mapper(mapper) { }

			/// Constructs a FilterMapper with the given regexp and the number of parameters
			FilterMapper(const char* filterString, const unsigned numParams, const CallMapper& mapper)
					: filter(std::make_shared<NumParamRegexCallFilter>(filterString, numParams)), mapper(mapper) { }

			/// Checks whether this filter matches the given FunctionDecl
			bool matches(const clang::FunctionDecl* funDecl) const {
				return filter->matches(funDecl);
			};

			/// Returns the representation of the used pattern
			const std::string getFilterRepresentation() const {
				return filter->getFilterRepresentation();
			}

			/// Returns the CallMapper used for this Filter
			const CallMapper& getCallMapper() const {
				return mapper;
			}
		};


		/// Utility for the specification of noop call mappings (C++ to IR)
		class NoopCallMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo&);
		};


		/// Utility for the mapping of the call to done without arguments
		class DoneCallMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo&);
		};


		/// Utility for the specification of simple call mappings (C++ to IR)
		class SimpleCallMapper {
			const string targetIRString;
			bool derefThisArg;
			core::ExpressionPtr buildCallWithDefaultParamConversion(const core::ExpressionPtr& callee, const ClangExpressionInfo& exprInfo);

		  protected:
			virtual core::ExpressionPtr convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args, insieme::frontend::conversion::Converter& converter);
			virtual core::ExpressionPtr generateCallee(const ClangExpressionInfo& exprInfo);
			virtual core::ExpressionPtr postprocessCall(const ClangExpressionInfo& exprInfo, const core::ExpressionPtr& translatedCall);

		  public:
			SimpleCallMapper(const string& targetIRString, bool derefThisArg = false) : targetIRString(targetIRString), derefThisArg(derefThisArg) {}
			core::ExpressionPtr operator()(const ClangExpressionInfo& exprInfo);
		};


		/// Utility for the specification of task_reference constructor calls
		class TaskRefDoneCallMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo&);
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
			virtual core::ExpressionPtr generateCallee(const ClangExpressionInfo& exprInfo) override;
			virtual core::ExpressionList postprocessArgumentList(const core::ExpressionList& args,
			                                                     insieme::frontend::conversion::Converter& converter) override;
			virtual core::ExpressionPtr postprocessCall(const ClangExpressionInfo& exprInfo, const core::ExpressionPtr& translatedCall) override;

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
			core::ExpressionPtr operator()(const ClangExpressionInfo& exprInfo);
		};


		/// Utility for the aggregation of function arguments into a tuple
		class TupleAggregationMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo&);
		};


		/// Utility for the aggregation of arguments into a list
		class ListAggregationMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo&);
		};


		/// Utilities to map the call to prec
		class PrecRecDefsMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo& exprInfo);
		};
		class PrecFunMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo& exprInfo);
		};
		/// Utility to map the call to prec while passing the three lambdas directly - without a fun call
		class PrecDirectMapper {
		  public:
			core::ExpressionPtr operator()(const ClangExpressionInfo& exprInfo);
		};
	}
}
}
}
