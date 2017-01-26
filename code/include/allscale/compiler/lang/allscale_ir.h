
#pragma once

#include "insieme/core/lang/extension.h"
#include "insieme/core/encoder/encoder.h"

namespace core = insieme::core;

namespace allscale {
namespace compiler {
namespace lang {

	class AllscaleModule : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		AllscaleModule(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:


		/**
		 * A constructor for functions usable within a prec call.
		 * Each function has a cutoff check, at least one base case and at least one step case
		 */
		LANG_EXT_LITERAL(BuildRecfun, "build_recfun" , "(('a) => bool, list<('a) => 'b>, list<('a, (recfun<'a,'b>, 'c...)) => treeture<'b,f>>) -> recfun<'a,'b>")

		LANG_EXT_LITERAL(Prec, "prec", "( (recfun<'a,'b>, 'c...) ) -> recfun<'a,'b>")

		// dependencies

		TYPE_ALIAS("dependencies", "list<treeture<'_dT, '_dR>>");

		LANG_EXT_LITERAL(DependencyAfter, "dependency_after", "('a...) -> dependencies")


		// treetures

		LANG_EXT_LITERAL(TreetureDone, "treeture_done", "('a) -> treeture<'a, f>")
		LANG_EXT_LITERAL(TreetureRun, "treeture_run", "(treeture<'a, f>) -> treeture<'a, t>")

		LANG_EXT_LITERAL(TreetureGet, "treeture_get", "(treeture<'a,'r>) -> 'a")
		LANG_EXT_LITERAL(TreetureLeft, "treeture_left", "(treeture<'a,'r>) -> treeture<unit, f>")
		LANG_EXT_LITERAL(TreetureRight, "treeture_right", "(treeture<'a,'r>) -> treeture<unit, f>")
		LANG_EXT_DERIVED(TreetureWait, "(t : treeture<'a,'r>) -> unit { treeture_get(t); }")

		LANG_EXT_LITERAL(TreetureSequential, "treeture_sequential", "(dependencies, treeture<'a, f>, treeture<'b, f>) -> treeture<'c, f>")
		LANG_EXT_LITERAL(TreetureParallel, "treeture_parallel", "(dependencies, treeture<'a, f>, treeture<'b, f>) -> treeture<'c, f>")
		LANG_EXT_LITERAL(TreetureCombine, "treeture_combine", "(dependencies, treeture<'a, f>, treeture<'b, f>, ('a,'b) -> 'c, bool) -> treeture<'c, f>")

		LANG_EXT_LITERAL(TreetureToRef,   "treeture_to_ref",   "(treeture<'a,'r>, type<ref<treeture<'a, 'r>, 'c, 'v, 'k>>) -> ref<treeture<'a,'r>, 'c, 'v, 'k>")
		LANG_EXT_LITERAL(TreetureFromRef, "treeture_from_ref", "(ref<treeture<'a,'r>, 'c, 'v, 'k>) -> treeture<'a,'r>")

		// recfuns and lambda operations

		LANG_EXT_LITERAL(RecfunToFun, "recfun_to_fun", "(recfun<'a,'b>) -> ('a) -> treeture<'b,f>")
		LANG_EXT_LITERAL(RecfunToDepFun, "recfun_to_dep_fun", "(recfun<'a,'b>) -> (dependencies, 'a) -> treeture<'b,f>")

		LANG_EXT_LITERAL(CppLambdaToClosure, "cpp_lambda_to_closure", "('l, type<('a...) => 'b>) -> ('a...) => 'b")
		LANG_EXT_LITERAL(CppLambdaToLambda,  "cpp_lambda_to_lambda",  "('l, type<('a...) -> 'b>) -> ('a...) -> 'b")
	};

	class RecFunType {
		core::TypePtr param, ret;

	  public:
		RecFunType(const core::TypePtr& param, const core::TypePtr& ret);
		RecFunType(const core::NodePtr& node);

		core::TypePtr getParamType() const {
			return param;
		}

		void setParamType(const core::TypePtr& type) {
			param = type;
		}

		core::TypePtr getReturnType() const {
			return ret;
		}

		void setReturnType(const core::TypePtr& type) {
			ret = type;
		}

		core::GenericTypePtr toIRType() const;

		operator core::GenericTypePtr() const;

		RecFunType(const RecFunType&) = default;
		RecFunType(RecFunType&&) = default;
		RecFunType& operator=(const RecFunType&) = default;
		RecFunType& operator=(RecFunType&&) = default;
	};

	class TreetureType {
		core::TypePtr valueType, released;

	  public:
		TreetureType(const core::TypePtr& valueType, bool released);
		TreetureType(const core::TypePtr& valueType, const core::TypePtr& released);

		TreetureType(const core::NodePtr& node);

		core::TypePtr getValueType() const {
			return valueType;
		}

		// NOTE: unknown is treated as not released
		bool isReleased() const;

		core::GenericTypePtr toIRType() const;

		operator core::GenericTypePtr() const;

		TreetureType(const TreetureType&) = default;
		TreetureType(TreetureType&&) = default;
		TreetureType& operator=(const TreetureType&) = default;
		TreetureType& operator=(TreetureType&&) = default;
	};

	/**
	 * A convenience wrapper for prec operation functions.
	 */
	class PrecFunction : public core::encoder::encodable {

		core::ExpressionPtr baseCaseTest;

		core::ExpressionList baseCases;

		core::ExpressionList stepCases;

	public:

		PrecFunction(const core::ExpressionPtr& baseCaseTest, const core::ExpressionList& baseCases, const core::ExpressionList& stepCases);

		static bool isPrecOperation(const core::NodePtr&);


		// -- getters and setters --

		const core::ExpressionPtr& getBaseCaseTest() const {
			return baseCaseTest;
		}

		void setBaseCaseTest(const core::ExpressionPtr&);

		const core::ExpressionList& getBaseCases() const {
			return baseCases;
		}

		void setBaseCases(const core::ExpressionList&);

		void addBaseCase(const core::ExpressionPtr&);

		const core::ExpressionList& getStepCases() const {
			return stepCases;
		}

		void setStepCases(const core::ExpressionList&);

		void addStepCase(const core::ExpressionPtr&);


		// -- more observers --

		core::FunctionTypePtr getBaseCaseTestType() const;

		core::FunctionTypePtr getBaseCaseType() const;

		core::FunctionTypePtr getStepCaseType() const;

		core::TypePtr getParameterType() const;

		core::TypePtr getResultType() const;

		TreetureType getTreetureType() const;

		core::TypePtr getRecursiveFunctionType() const;

		core::TypeList getRecursiveFunctionParameterTypes() const;



		// -- encoder interface --

		static core::TypePtr getEncodedType(core::NodeManager&);

		static bool isEncoding(const core::ExpressionPtr&);

		core::ExpressionPtr toIR(core::NodeManager&) const;

		static PrecFunction fromIR(const core::ExpressionPtr&);

	};


	/**
	 * A convenience wrapper for prec operations.
	 */
	class PrecOperation : public core::encoder::encodable {

		std::vector<PrecFunction> functions;

	public:

		PrecOperation(const std::vector<PrecFunction>& functions);

		static bool isPrecOperation(const core::NodePtr&);


		// -- getters and setters --

		const PrecFunction& getFunction() const {
			return functions[0];
		}

		const std::vector<PrecFunction>& getFunctions() const {
			return functions;
		}


		// -- more observers --

		core::TypePtr getParameterType() const;

		core::TypePtr getResultType() const;

		TreetureType getTreetureType() const;


		// -- encoder interface --

		static core::TypePtr getEncodedType(core::NodeManager&);

		static bool isEncoding(const core::ExpressionPtr&);

		core::ExpressionPtr toIR(core::NodeManager&) const;

		static PrecOperation fromIR(const core::ExpressionPtr&);

	};

	bool isRecFun(const core::NodePtr& node);

	bool isTreeture(const core::NodePtr& node);

	bool isCompletedTask(const core::NodePtr& node);

	core::ExpressionPtr buildBuildRecFun(const core::ExpressionPtr& cutoffBind,
	                                     const core::ExpressionList& baseBinds,
	                                     const core::ExpressionList& stepBinds);

	core::ExpressionPtr buildPrec(const core::ExpressionList& recFuns);

	// treeture

	core::ExpressionPtr buildTreetureRun(const core::ExpressionPtr& param);

	core::ExpressionPtr buildTreetureCombine(const core::ExpressionPtr& a, const core::ExpressionPtr& b,
	                                         const core::ExpressionPtr& combinerLambda, const core::ExpressionPtr& parallel);

	core::ExpressionPtr buildTreetureGet(const core::ExpressionPtr& param);

	core::ExpressionPtr buildTreetureToRef(const core::ExpressionPtr& treetureExpr, const core::TypePtr& targetType);

	core::ExpressionPtr buildTreetureFromRef(const core::ExpressionPtr& refTreetureExpr);

	core::ExpressionPtr buildRecfunToFun(const core::ExpressionPtr& param);

	// lambda utils

	core::ExpressionPtr buildCppLambdaToClosure(const core::ExpressionPtr& lambdaExpr, core::FunctionTypePtr closureType = nullptr);

	core::ExpressionPtr buildCppLambdaToLambda(const core::ExpressionPtr& lambdaExpr, core::FunctionTypePtr closureType = nullptr);

}
}
}
