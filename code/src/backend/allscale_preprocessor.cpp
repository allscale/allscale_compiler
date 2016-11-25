#include "allscale/compiler/backend/allscale_preprocessor.h"

#include <map>
#include <string>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/backend/preprocessor.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/backend/allscale_runtime_entities.h"

namespace allscale {
namespace compiler {
namespace backend {

	namespace core = insieme::core;
	namespace be = insieme::backend;

	using std::string;
	using std::map;


	namespace {

		core::ProgramPtr wrapStmt(const core::StatementPtr& expr);

		core::ProgramPtr replaceMain(const core::ProgramPtr& prog, const be::Converter& converter);

	}

	core::NodePtr EntryPointWrapper::process(const be::Converter& converter, const core::NodePtr& code) {

		// if the given node is a stmt ..
		if (auto stmt = code.isa<core::StatementPtr>()) {
			// .. wrap the expression into a main function and convert the result
			return process(converter, wrapStmt(stmt));
		}

		// the rest only concerns programs
		auto program = code.isa<core::ProgramPtr>();
		if (!program) return code;

		// replace the main function
		return replaceMain(program, converter);
	}


	namespace {

		core::ProgramPtr wrapStmt(const core::StatementPtr& stmt) {
			core::IRBuilder builder(stmt->getNodeManager());

			// if it is a lambda, make it the entry point
			if (auto lambda = stmt.isa<core::LambdaExprPtr>()) {
				return builder.program(core::ExpressionList({lambda}));
			}

			// create a symbol
			map<string,core::NodePtr> symbols;
			symbols["code"] = stmt;

			// simply wrap the given expression into an otherwise empty program
			return builder.parseProgram(
					"int<4> main() {"
					"	code;"
					"	return 0;"
					"}",
					symbols
			);
		}

		core::ProgramPtr replaceMain(const core::ProgramPtr& prog, const be::Converter& converter) {
			core::NodeManager& mgr = prog.getNodeManager();
			core::IRBuilder builder(mgr);

			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// TODO: wrap startup / shutdown + task wrapper

			// get the main entry point
			assert_eq(1, prog->getEntryPoints().size());
			core::LambdaExprPtr main = prog->getEntryPoints().front().as<core::LambdaExprPtr>();

			// convert main to accept a tuple of parameters
			auto closureType = builder.tupleType(main->getFunctionType()->getParameterTypeList());
			auto paramType = core::lang::buildRefType(closureType,true,false,core::lang::ReferenceType::Kind::CppReference);
			auto resType = lang::TreetureType(main->getFunctionType()->getReturnType(),false).toIRType();

			auto newFunType = builder.functionType(paramType,resType);

			auto param = builder.variable(paramType);

			core::ExpressionList args;
			for(unsigned i=0; i<main->getFunctionType()->getParameterTypes().size(); ++i) {
				args.push_back(builder.refComponent(param,i));
			}

			// create body wrapping up computation and treeture conversion
			auto body = builder.compoundStmt(
					builder.returnStmt(
							builder.callExpr(
									ext.getTreetureDone(),
									builder.callExpr(main, args)
							)
					)
			);

			// replace main by proper wrapper
			core::LambdaExprPtr oldMain = main;
			main = builder.lambdaExpr(newFunType, { param }, body);

			// check that what has been build is properly composed
			assert_true(core::checks::check(main).empty())
				<< core::checks::check(main);

			// convert this main into a work item
			WorkItemVariant impl(main);
			WorkItemDescription desc("main", impl);


			// create a new main entry point
			core::ExpressionList entryPointArgs;
			entryPointArgs.push_back(desc.toIR(mgr));
			for(const auto& cur : oldMain->getParameterList()->getParameters()) entryPointArgs.push_back(cur);

			auto& asbm = mgr.getLangExtension<AllScaleBackendModule>();
			auto newMainBody = builder.compoundStmt(
					builder.returnStmt(
							builder.callExpr(
									asbm.getProcessMain(),
									entryPointArgs
							)
					)
			);

			// create final main
			main = core::transform::replaceNode(mgr, core::LambdaExprAddress(oldMain)->getBody(), newMainBody).as<core::LambdaExprPtr>();

			// check that what has been build is properly composed
			assert_true(core::checks::check(main).empty())
				<< core::checks::check(main);

			// wrap up into a program, and be done
			return builder.program(core::ExpressionList{ main });
		}

	}

	namespace {

		core::ExpressionPtr inlineStep(const core::ExpressionPtr& stepCase, const core::ExpressionPtr& recFun) {
			core::IRBuilder builder(stepCase->getNodeManager());



			// TODO: return something like this:
			std::map<std::string,core::NodePtr> symbols;
			symbols["rec"] = recFun;
			return builder.parseExpr(
					"( x : int<4> ) -> int<4> { return rec(x-1) + rec(x-2); }",
					symbols
			);

		}


		core::ExpressionPtr getSequentialImplementation(const lang::PrecOperation& op) {
			core::IRBuilder builder(op.getFunction().getBaseCaseTest()->getNodeManager());

			// -- build up the sequential implementation of this function --

			assert_eq(1,op.getFunctions().size())
				<< "Mutual recursive functions not yet supported!";

			// get the function to be encoded
			const auto& fun = op.getFunction();

			// get the type of the resulting function (same as the base case type)
			auto funType = fun.getBaseCaseType();

			// create the recursive function reference
			auto recFun = builder.lambdaReference(funType,"rec");

			// get the in-parameter
			auto in = builder.variable(builder.refType(fun.getParameterType()));
			auto inVal = builder.deref(in);

			// get instantiated step implementation
			auto stepFun = inlineStep(fun.getStepCases()[0],recFun);

			// create the body of the lambda
			auto body = builder.compoundStmt(
				builder.ifStmt(
					// check the base case test
					builder.callExpr(fun.getBaseCaseTest(), inVal),
					// if in the base case => run base case
					builder.returnStmt(builder.callExpr(fun.getBaseCases()[0],inVal)),
					// else run step case
					builder.returnStmt(builder.callExpr(stepFun,inVal))
				)
			);

			// build a lambda ..
			auto lambda = builder.lambda(funType,{in},body);

			// .. the enclosing definition ..
			core::LambdaBindingMap bindings;
			bindings[recFun] = lambda;
			auto lambdaDef = builder.lambdaDefinition(bindings);

			// and the resulting lambda expression
			return builder.lambdaExpr(recFun,lambdaDef);
		}


		// TODO: add closure support

		core::LambdaExprPtr convertToLambda(const core::LambdaExprPtr& expr) {

			core::NodeManager& mgr = expr.getNodeManager();
			core::IRBuilder builder(mgr);

			// create the new parameter type
			core::TypePtr paramType = builder.refType(
					builder.tupleType(expr->getFunctionType()->getParameterTypeList()),
					true,false,core::lang::ReferenceType::Kind::CppReference
			);

			// create a new parameter
			auto param = builder.variable(paramType);

			// create expressions unpacking the arguments
			core::ExpressionList args;
			for(unsigned i=0; i<expr->getParameterList().size(); ++i) {
				args.push_back(builder.deref(builder.refComponent(param,i)));
			}

			// create wrapper function body
			auto body = builder.compoundStmt(
					builder.returnStmt(
							builder.callExpr(expr,args)
					)
			);

			// create the new function type
			auto funType = builder.functionType(paramType, expr->getFunctionType()->getReturnType());

			// create resulting function
			return builder.lambdaExpr(funType,{ param }, body);

		}

		core::LambdaExprPtr convertToLambda(const core::BindExprPtr& expr) {
			assert_not_implemented() << "Not yet implemented!";
			return {};
		}

		/**
		 * This function converts the given lambda or bind into a
		 * lambda accepting all its parameters as a tuple, not capturing
		 * any values implicitly, and a list of expressions describing
		 * the captured values.
		 */
		core::LambdaExprPtr convertToLambda(const core::ExpressionPtr& expr) {
			// distinguish the supported cases
			if (auto lambda = expr.isa<core::LambdaExprPtr>()) {
				return convertToLambda(lambda);
			}
			if (auto bind = expr.isa<core::BindExprPtr>()) {
				return convertToLambda(bind);
			}

			// all others are not supported
			assert_fail() << "Unsupported expression of type " << expr->getNodeType();
			return {};
		}


		WorkItemVariant getProcessVariant(const lang::PrecOperation& op) {

			// pick the base case implementation
			// TODO: create the actual implementation
			// TODO: implement a tool converting a bind into a function
			auto impl = getSequentialImplementation(op);

			core::NodeManager& mgr = impl.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

			// convert into a lambda, making captured parameters explicit
			core::LambdaExprPtr lambda = convertToLambda(impl);

			// create a wrapper which is spawning a treeture
			auto body =
				builder.compoundStmt(
					builder.returnStmt(
						builder.callExpr(
							ext.getTreetureDone(),
							builder.callExpr(lambda,lambda->getParameterList()[0])
						)
					)
				);

			// create the resultig function type
			auto funType = builder.functionType(
				lambda->getFunctionType()->getParameterType(0),
				lang::TreetureType(lambda->getFunctionType()->getReturnType(),false).toIRType()
			);

			// use this lambda for creating the work item variant
			return WorkItemVariant(builder.lambdaExpr(funType, lambda->getParameterList(), body));
		}

		WorkItemVariant getSplitVariant(const lang::PrecOperation& op) {
			// return the process function for now
			return getProcessVariant(op);
		}

		core::NodePtr convertPrecOperator(const core::NodePtr& code) {

			// only interested in prec operators
			if (!lang::PrecOperation::isPrecOperation(code)) return code;

			// get build utilities
			core::NodeManager& mgr = code.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

			// parse prec operation
			lang::PrecOperation op = lang::PrecOperation::fromIR(code.as<core::ExpressionPtr>());

			// extract a sequential implementation of the prec operation
			auto process = getProcessVariant(op);

			// extract a parallel implementation of the prec operation
			auto split = getSplitVariant(op);

			// wrap it up in a work item
			WorkItemDescription desc("name",process,split);

			// create a function wrapping the spawn call (need for bind)
			core::VariableList params;

			// TODO: add closure values to parameters

			// add the input parameter
			params.push_back(builder.variable(builder.refType(op.getFunction().getParameterType())));

			core::ExpressionList args;
			args.push_back(desc.toIR(mgr));
			for(const auto& cur : params) args.push_back(builder.deref(cur));

			// build the nested lambda
			auto nestedLambda = builder.lambdaExpr(
				op.getFunction().getTreetureType().toIRType(),
				params,
				builder.compoundStmt(
					builder.returnStmt(
						builder.callExpr(
							ext.getSpawnWorkItem(), args
						)
					)
				)
			);

			// create a bind spawning the work item - TODO: include closure values
			auto param = builder.variable(op.getParameterType());
			return builder.bindExpr({ param }, builder.callExpr(nestedLambda, param));
		}

	}



	core::NodePtr PrecConverter::process(const be::Converter& converter, const core::NodePtr& code) {

		// replace all prec calls with actual lambdas
		auto res = core::transform::transformBottomUp(code, convertPrecOperator, core::transform::globalReplacement);

		// check that the result is properly typed
		assert_true(core::checks::check(res).empty())
			<< dumpPretty(res) << "\n"
			<< core::checks::check(res);

		// return result
		return res;

	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
