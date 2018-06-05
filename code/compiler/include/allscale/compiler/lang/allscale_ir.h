
#pragma once

#include "insieme/core/lang/extension.h"
#include "insieme/core/encoder/encoder.h"

namespace allscale {
namespace compiler {
namespace lang {

	class AllscaleModule : public insieme::core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class insieme::core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		AllscaleModule(insieme::core::NodeManager& manager) : insieme::core::lang::Extension(manager) {}

	  public:


		/**
		 * A constructor for functions usable within a prec call.
		 * Each function has a cutoff check, at least one base case and at least one step case
		 */
		LANG_EXT_LITERAL(BuildRecfun, "build_recfun" , "(('a) => bool, list<('a) => 'b>, list<('a, (recfun<'a,'b>, 'c...)) => treeture<'b,f>>) -> recfun<'a,'b>")

		LANG_EXT_LITERAL(Prec, "prec", "( (recfun<'a,'b>, 'c...) ) -> precfun<'a,'b>")

		// dependencies

		LANG_EXT_TYPE(DependenciesType,"dependencies")
		LANG_EXT_LITERAL(DependencyAfter, "dependency_after", "('a...) -> dependencies")
		LANG_EXT_LITERAL(DependencyAdd, "dependency_add", "(dependencies, task_ref) -> dependencies")

		// treetures

		LANG_EXT_LITERAL(TreetureDone,        "treeture_done",          "('a)              -> treeture<'a, f>")
		LANG_EXT_LITERAL(TreetureDoneFromRef, "treeture_done_from_ref", "(ref<'a,'c,f,'k>) -> treeture<'a, f>")
		LANG_EXT_LITERAL(TreetureRun, "treeture_run", "(treeture<'a, f>) -> treeture<'a, t>")

		LANG_EXT_LITERAL(TreetureGet,     "treeture_get",     "(treeture<'a,'r>) -> 'a")
		LANG_EXT_LITERAL(TreetureExtract, "treeture_extract", "(treeture<'a,'r>) -> ref<'a,f,f,cpp_rref>") // this is the version of treeture_get, which can be used to move the result
		LANG_EXT_DERIVED(TreetureLeft, "(t : treeture<'a,'r>) -> task_ref { return task_ref_left(treeture_to_task_ref(t)); }")
		LANG_EXT_DERIVED(TreetureRight, "(t : treeture<'a,'r>) -> task_ref { return task_ref_right(treeture_to_task_ref(t)); }")
		LANG_EXT_DERIVED(TreetureWait, "(t : treeture<'a,'r>) -> unit { treeture_get(t); }")

		LANG_EXT_LITERAL(TreetureIsDone,  "treeture_is_done",  "(treeture<'a,'r>) -> bool")
		LANG_EXT_LITERAL(TreetureIsValid, "treeture_is_valid", "(treeture<'a,'r>) -> bool")

		LANG_EXT_LITERAL(TreetureSequential, "treeture_sequential", "(dependencies, treeture<'a, f>, treeture<'b, f>) -> treeture<unit, f>")
		LANG_EXT_LITERAL(TreetureParallel, "treeture_parallel", "(dependencies, treeture<'a, f>, treeture<'b, f>) -> treeture<unit, f>")
		LANG_EXT_LITERAL(TreetureCombine, "treeture_combine", "(dependencies, treeture<'a, f>, treeture<'b, f>, ('av,'bv) -> 'r, bool) -> treeture<'r, f>")

		LANG_EXT_LITERAL(TreetureToRef,   "treeture_to_ref",   "(treeture<'a,'r>, type<ref<treeture<'a, 'r>, 'c, 'v, 'k>>) -> ref<treeture<'a,'r>, 'c, 'v, 'k>")
		LANG_EXT_LITERAL(TreetureFromRef, "treeture_from_ref", "(ref<treeture<'a,'r>, 'c, 'v, 'k>) -> treeture<'a,'r>")

		// task references

		LANG_EXT_LITERAL(TaskRefDone, "task_ref_done", "() -> task_ref")
		LANG_EXT_LITERAL(TaskRefLeft, "task_ref_left", "(task_ref) -> task_ref")
		LANG_EXT_LITERAL(TaskRefRight, "task_ref_right", "(task_ref) -> task_ref")
		LANG_EXT_LITERAL(TaskRefWait, "task_ref_wait", "(task_ref) -> unit")
		LANG_EXT_LITERAL(TaskRefValid, "task_ref_valid", "(task_ref) -> bool")

		LANG_EXT_LITERAL(TreetureToTaskRef, "treeture_to_task_ref", "(treeture<'a,'r>) -> task_ref")

		// recfuns and lambda operations

		LANG_EXT_LITERAL(RecfunToFun, "recfun_to_fun", "(recfun<'a,'b>) -> ('a) -> treeture<'b,f>")
		LANG_EXT_LITERAL(RecfunToDepFun, "recfun_to_dep_fun", "(recfun<'a,'b>) -> (dependencies, 'a) -> treeture<'b,f>")

		LANG_EXT_LITERAL(PrecfunToFun, "precfun_to_fun", "(precfun<'a,'b>) -> ('a) -> treeture<'b,f>")
		LANG_EXT_LITERAL(PrecfunToDepFun, "precfun_to_dep_fun", "(precfun<'a,'b>) -> (dependencies, 'a) -> treeture<'b,f>")

		LANG_EXT_LITERAL(CppLambdaToClosure, "cpp_lambda_to_closure", "('l, type<('a...) => 'b>) -> ('a...) => 'b")
		LANG_EXT_LITERAL(CppLambdaToLambda,  "cpp_lambda_to_lambda",  "('l, type<('a...) -> 'b>) -> ('a...) -> 'b")

		// data item sema function

		LANG_EXT_LITERAL(DataItemElementAccess, "data_item_element_access",
			"(ref<'data_item, 'c, 'v, 'k>, 'region, type<ref<'element_type, 'c2, 'v2, 'k2>>) -> ref<'element_type, 'c2, 'v2, 'k2>")

		LANG_EXT_LITERAL(DataItemReadRequirement, "data_item_read_requirement",
			"(ref<'data_item, 'c, 'v, 'k>, 'region) -> unit")

		LANG_EXT_LITERAL(DataItemWriteRequirement, "data_item_write_requirement",
			"(ref<'data_item, 'c, 'v, 'k>, 'region) -> unit")

		LANG_EXT_LITERAL(DataItemNoDependencies, "data_item_no_dependencies", "()->unit")
	};

	class RecOrPrecFunType {
		insieme::core::TypePtr param, ret;

	  public:
		RecOrPrecFunType(const insieme::core::TypePtr& param, const insieme::core::TypePtr& ret);
		RecOrPrecFunType(const insieme::core::NodePtr& node);

		insieme::core::TypePtr getParamType() const {
			return param;
		}

		void setParamType(const insieme::core::TypePtr& type) {
			param = type;
		}

		insieme::core::TypePtr getReturnType() const {
			return ret;
		}

		void setReturnType(const insieme::core::TypePtr& type) {
			ret = type;
		}

		RecOrPrecFunType(const RecOrPrecFunType&) = default;
		RecOrPrecFunType(RecOrPrecFunType&&) = default;
		RecOrPrecFunType& operator=(const RecOrPrecFunType&) = default;
		RecOrPrecFunType& operator=(RecOrPrecFunType&&) = default;
	};


	class RecFunType : public RecOrPrecFunType {

	  public:

		using RecOrPrecFunType::RecOrPrecFunType;

		insieme::core::GenericTypePtr toIRType() const;

		operator insieme::core::GenericTypePtr() const;

	};

	class PrecFunType : public RecOrPrecFunType {

	  public:

		using RecOrPrecFunType::RecOrPrecFunType;

		insieme::core::GenericTypePtr toIRType() const;

		operator insieme::core::GenericTypePtr() const;

	};

	class TreetureType {
		insieme::core::TypePtr valueType, released;

	  public:
		TreetureType(const insieme::core::TypePtr& valueType, bool released);
		TreetureType(const insieme::core::TypePtr& valueType, const insieme::core::TypePtr& released);

		TreetureType(const insieme::core::NodePtr& node);

		insieme::core::TypePtr getValueType() const {
			return valueType;
		}

		// NOTE: unknown is treated as not released
		bool isReleased() const;

		insieme::core::GenericTypePtr toIRType() const;

		operator insieme::core::GenericTypePtr() const;

		TreetureType(const TreetureType&) = default;
		TreetureType(TreetureType&&) = default;
		TreetureType& operator=(const TreetureType&) = default;
		TreetureType& operator=(TreetureType&&) = default;
	};

	/**
	 * A convenience wrapper for prec operation functions.
	 */
	class PrecFunction : public insieme::core::encoder::encodable {

		insieme::core::ExpressionPtr baseCaseTest;

		insieme::core::ExpressionList baseCases;

		insieme::core::ExpressionList stepCases;

	public:

		PrecFunction(const insieme::core::ExpressionPtr& baseCaseTest, const insieme::core::ExpressionList& baseCases, const insieme::core::ExpressionList& stepCases);

		static bool isPrecOperation(const insieme::core::NodePtr&);


		// -- getters and setters --

		const insieme::core::ExpressionPtr& getBaseCaseTest() const {
			return baseCaseTest;
		}

		void setBaseCaseTest(const insieme::core::ExpressionPtr&);

		const insieme::core::ExpressionList& getBaseCases() const {
			return baseCases;
		}

		void setBaseCases(const insieme::core::ExpressionList&);

		void addBaseCase(const insieme::core::ExpressionPtr&);

		const insieme::core::ExpressionList& getStepCases() const {
			return stepCases;
		}

		void setStepCases(const insieme::core::ExpressionList&);

		void addStepCase(const insieme::core::ExpressionPtr&);


		// -- more observers --

		insieme::core::FunctionTypePtr getBaseCaseTestType() const;

		insieme::core::FunctionTypePtr getBaseCaseType() const;

		insieme::core::FunctionTypePtr getStepCaseType() const;

		insieme::core::TypePtr getParameterType() const;

		insieme::core::TypePtr getResultType() const;

		TreetureType getTreetureType() const;

		insieme::core::TypePtr getRecursiveFunctionType() const;

		insieme::core::TypeList getRecursiveFunctionParameterTypes() const;



		// -- encoder interface --

		static insieme::core::TypePtr getEncodedType(insieme::core::NodeManager&);

		static bool isEncoding(const insieme::core::ExpressionPtr&);

		insieme::core::ExpressionPtr toIR(insieme::core::NodeManager&) const;

		static PrecFunction fromIR(const insieme::core::ExpressionPtr&);

	};


	/**
	 * A convenience wrapper for prec operations.
	 */
	class PrecOperation : public insieme::core::encoder::encodable {

		std::vector<PrecFunction> functions;

	public:

		PrecOperation(const std::vector<PrecFunction>& functions);

		static bool isPrecOperation(const insieme::core::NodePtr&);


		// -- getters and setters --

		const PrecFunction& getFunction() const {
			return functions[0];
		}

		const std::vector<PrecFunction>& getFunctions() const {
			return functions;
		}


		// -- more observers --

		insieme::core::TypePtr getParameterType() const;

		insieme::core::TypePtr getResultType() const;

		TreetureType getTreetureType() const;


		// -- encoder interface --

		static insieme::core::TypePtr getEncodedType(insieme::core::NodeManager&);

		static bool isEncoding(const insieme::core::ExpressionPtr&);

		insieme::core::ExpressionPtr toIR(insieme::core::NodeManager&) const;

		static PrecOperation fromIR(const insieme::core::ExpressionPtr&);

	};

	bool isDependencies(const insieme::core::NodePtr& node);

	bool isRecFun(const insieme::core::NodePtr& node);

	bool isRecFunToFunCall(const insieme::core::NodePtr&);

	bool isRecFunToDepFunCall(const insieme::core::NodePtr&);

	bool isRecFunUnwrapperCall(const insieme::core::NodePtr&);

	bool isPrecFun(const insieme::core::NodePtr& node);

	bool isPrecFunToFunCall(const insieme::core::NodePtr&);

	bool isPrecFunToDepFunCall(const insieme::core::NodePtr&);

	bool isPrecFunUnwrapperCall(const insieme::core::NodePtr&);

	bool isTreeture(const insieme::core::NodePtr& node);

	bool isTaskReference(const insieme::core::NodePtr& node);

	bool isCompletedTask(const insieme::core::NodePtr& node);

	bool isAllscaleType(const insieme::core::NodePtr& node);

	insieme::core::ExpressionPtr buildBuildRecFun(const insieme::core::ExpressionPtr& cutoffBind,
	                                     const insieme::core::ExpressionList& baseBinds,
	                                     const insieme::core::ExpressionList& stepBinds);

	insieme::core::ExpressionPtr buildPrec(const insieme::core::ExpressionPtr& recFunTuple);
	insieme::core::ExpressionPtr buildPrec(const insieme::core::ExpressionList& recFuns);

	// dependencies

	insieme::core::ExpressionPtr buildNoDependencies(insieme::core::NodeManager&);

	// treeture

	insieme::core::ExpressionPtr buildTreetureDone(const insieme::core::ExpressionPtr& param);

	insieme::core::ExpressionPtr buildTreetureRun(const insieme::core::ExpressionPtr& param);

	insieme::core::ExpressionPtr buildTreetureCombine(const insieme::core::ExpressionPtr& a, const insieme::core::ExpressionPtr& b,
	                                         const insieme::core::ExpressionPtr& combinerLambda, const insieme::core::ExpressionPtr& parallel);

	insieme::core::ExpressionPtr buildTreetureGet(const insieme::core::ExpressionPtr& param);
	insieme::core::ExpressionPtr buildTreetureExtract(const insieme::core::ExpressionPtr& param);

	insieme::core::ExpressionPtr buildTreetureToRef(const insieme::core::ExpressionPtr& treetureExpr, const insieme::core::TypePtr& targetType);

	insieme::core::ExpressionPtr buildTreetureFromRef(const insieme::core::ExpressionPtr& refTreetureExpr);

	insieme::core::ExpressionPtr buildRecfunToFun(const insieme::core::ExpressionPtr& param);
	insieme::core::ExpressionPtr buildRecfunToDepFun(const insieme::core::ExpressionPtr& param);

	insieme::core::ExpressionPtr buildPrecfunToFun(const insieme::core::ExpressionPtr& param);
	insieme::core::ExpressionPtr buildPrecfunToDepFun(const insieme::core::ExpressionPtr& param);

	// lambda utils

	insieme::core::ExpressionPtr buildCppLambdaToClosure(const insieme::core::ExpressionPtr& lambdaExpr, insieme::core::FunctionTypePtr closureType);

	insieme::core::ExpressionPtr buildCppLambdaToLambda(const insieme::core::ExpressionPtr& lambdaExpr, insieme::core::FunctionTypePtr closureType);
}
}
}
