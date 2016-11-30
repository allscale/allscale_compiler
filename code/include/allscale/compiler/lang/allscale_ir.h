
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

		LANG_EXT_LITERAL(Prec, "prec", "( (recfun<'a,'b>, 'c...) ) -> ('a) => treeture<'b,f>")
		LANG_EXT_LITERAL(TreetureDone, "treeture_done", "('a) -> treeture<'a,f>")
		LANG_EXT_LITERAL(TreetureRun, "treeture_run", "(treeture<'a, f>) -> treeture<'a, t>")

		LANG_EXT_LITERAL(TreetureCombine, "treeture_combine", "(treeture<'a,f>,treeture<'b,f>,('a,'b)->'c)->treeture<'c,f>")

		LANG_EXT_LITERAL(TreetureGet, "treeture_get", "(treeture<'a,'r>) -> 'a")
		LANG_EXT_LITERAL(TreetureLeft, "treeture_left", "(treeture<'a,'r>) -> treeref")
		LANG_EXT_LITERAL(TreetureRight, "treeture_right", "(treeture<'a,'r>) -> treeref")

		LANG_EXT_DERIVED(TreetureWait, "(t : treeture<'a,'r>) -> unit { treeture_get(t); }")

		LANG_EXT_LITERAL(RecfunToFun, "recfun_to_fun", "(recfun<'a,'b>) -> ('a) -> treeture<'b,f>")

		LANG_EXT_LITERAL(CppLambdaToClosure, "cpp_lambda_to_closure", "('l, type<('a...) => 'b>) -> ('a...) => 'b")
	};

	class RecFunType {
		core::TypePtr param, ret;

	  public:
		RecFunType(const core::TypePtr& param, const core::TypePtr& ret);
		RecFunType(const core::NodePtr& node);

		core::TypePtr getParamType() {
			return param;
		}

		core::TypePtr getReturnType() {
			return ret;
		}

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

	core::ExpressionPtr buildBuildRecFun(const core::ExpressionPtr& cutoffBind,
	                                     const core::ExpressionList& baseBinds,
	                                     const core::ExpressionList& stepBinds);

	core::ExpressionPtr buildPrec(const core::ExpressionList& recFuns);

	core::ExpressionPtr buildTreetureDone(const core::ExpressionPtr& param);

	core::ExpressionPtr buildTreetureRun(const core::ExpressionPtr& param);

	core::ExpressionPtr buildTreetureGet(const core::ExpressionPtr& param);

	core::ExpressionPtr buildRecfunToFun(const core::ExpressionPtr& param);

	core::ExpressionPtr buildCppLambdaToClosure(const core::ExpressionPtr& lambdaExpr, const core::FunctionTypePtr& closureType);

}
}
}
