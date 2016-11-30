#include "allscale/compiler/backend/allscale_preprocessor.h"

#include <map>
#include <string>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/name_manager.h"

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

			// simply wrap the given expression into an otherwise empty program
			auto wrapper = builder.parseProgram(
					"int<4> main() {"
					"	return 0;"
					"}"
			);

			// insert the statement as the first statement of the body
			return core::transform::insert(
					stmt->getNodeManager(),
					core::ProgramAddress(wrapper)->getEntryPoints()[0].as<core::LambdaExprAddress>()->getBody(),
					stmt,
					0
			).as<core::ProgramPtr>();
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

		core::ExpressionPtr inlineStep(const core::ExpressionPtr& stepCase, const core::ExpressionPtr& recFun, bool serialize) {
			auto& mgr = stepCase->getNodeManager();
			core::IRBuilder builder(stepCase->getNodeManager());

			assert_true(stepCase.isa<core::LambdaExprPtr>())
				<< "Only supported for lambdas so far!\n"
				<< "found: " << *stepCase;

			// This function does:
			//  - remove the recursive function parameter
			//  - replace the recursive function parameter by recFun in body
			//  - remove treeture_run, treeture_get and treeture_done in body
			//  - remove treeture in return type

			// get incoming lambda
			auto in = stepCase.as<core::LambdaExprPtr>();

			// get the result type
			auto resType = in->getFunctionType()->getReturnType();

			// get the body
			auto body = in->getBody();

			// replace calls to recursive token by actual recursive call
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			body = core::transform::transformBottomUp(body, [&](const core::NodePtr& node)->core::NodePtr {

				// replace recursive calls
				if (core::analysis::isCallOf(node,ext.getRecfunToFun())) {
					return recFun;
				}

				// everything else remains untouched
				return node;

			}).as<core::CompoundStmtPtr>();


			// if serialization should be applied, do so
			if (serialize) {

				// a utility to remove treeture types
				auto removeTreetures = [&](const core::NodePtr& node) {
					return core::transform::transformBottomUp(node, [&](const core::NodePtr& node)->core::NodePtr {

						// check whether it is a treeture type
						if (node.isa<core::TypePtr>() && lang::isTreeture(node)) {
							return lang::TreetureType(node).getValueType();
						}

						// not of interest either
						return node;
					});
				};

				// get body, replace treeture operations and recFun calls
				body = core::transform::transformBottomUp(body, [&](const core::NodePtr& node)->core::NodePtr {

					// only interested in calls
					auto call = node.isa<core::CallExprPtr>();
					if (!call) return node;

					if (core::analysis::isCallOf(call,ext.getTreetureDone())) {
						return call->getArgument(0);
					}

					if (core::analysis::isCallOf(call,ext.getTreetureRun())) {
						return call->getArgument(0);
					}

					if (core::analysis::isCallOf(call,ext.getTreetureGet())) {
						return call->getArgument(0);
					}

					if (core::analysis::isCallOf(call,ext.getTreetureCombine())) {
						auto arg0 = removeTreetures(call->getArgument(0)).as<core::ExpressionPtr>();
						auto arg1 = removeTreetures(call->getArgument(1)).as<core::ExpressionPtr>();
						return builder.callExpr(call->getArgument(2),arg0,arg1);
					}

					// not of interest either
					return node;
				}).as<core::CompoundStmtPtr>();


				// replace all treeture types by their value types
				body = removeTreetures(body).as<core::CompoundStmtPtr>();

				// also remove the treeture wrapper from the result type
				resType = lang::TreetureType(resType).getValueType();
			}

			// build resulting function type
			auto funType = builder.functionType(
				in->getFunctionType()->getParameterType(0),
				resType
			);

			// build up resulting function
			return builder.lambdaExpr(funType,{ in->getParameterList()[0] }, body);
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
			auto stepFun = inlineStep(fun.getStepCases()[0],recFun,true);

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

		core::ExpressionPtr getParallelImplementation(const string& wi_name, const lang::PrecOperation& op) {
			auto& mgr = op.getFunction().getBaseCaseTest()->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			auto& ext2 = mgr.getLangExtension<AllScaleBackendModule>();

			// -- build up the sequential implementation of this function --

			assert_eq(1,op.getFunctions().size())
				<< "Mutual recursive functions not yet supported!";

			// get the function to be encoded
			const auto& fun = op.getFunction();

			// get the type of the resulting function
			auto funType = builder.functionType(op.getParameterType(), op.getTreetureType().toIRType());

			// create the recursive function reference
			auto recFun =
					builder.callExpr(
						ext2.getRecSpawnWorkItem(),
						builder.callExpr(
							ext2.getCreateWorkItemDescriptionReference(),
							builder.getIdentifierLiteral(wi_name),
							builder.getTypeLiteral(builder.tupleType({ op.getParameterType() })),
							builder.getTypeLiteral(op.getResultType())
						)
					);

			// get the in-parameter
			auto in = builder.variable(builder.refType(fun.getParameterType()));
			auto inVal = builder.deref(in);

			// get instantiated step implementation
			auto stepFun = inlineStep(fun.getStepCases()[0],recFun,false);

			// create the body of the lambda
			auto body = builder.compoundStmt(
				builder.ifStmt(
					// check the base case test
					builder.callExpr(fun.getBaseCaseTest(), inVal),
					// if in the base case => run base case
					builder.returnStmt(builder.callExpr(ext.getTreetureDone(), builder.callExpr(fun.getBaseCases()[0],inVal))),
					// else run step case
					builder.returnStmt(builder.callExpr(stepFun,inVal))
				)
			);

			// build the resulting lambda
			return builder.lambdaExpr(funType,{in},body);
		}



		// TODO: remove this

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
				args.push_back(builder.accessComponent(builder.deref(param),i));
//				args.push_back(builder.deref(builder.refComponent(param,i)));
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

		WorkItemVariant getSplitVariant(const std::string& wi_name, const lang::PrecOperation& op) {

			// pick the base case implementation
			// TODO: implement a tool converting a bind into a function
			auto impl = getParallelImplementation(wi_name,op);

			// convert into a lambda, making captured parameters explicit
			core::LambdaExprPtr lambda = convertToLambda(impl);

			// use this lambda for creating the work item variant
			return WorkItemVariant(lambda);
		}


		core::ExpressionList getCapturedValues(const core::ExpressionPtr& expr) {

			// nothing is captured by lambda expressions
			if (expr.isa<core::LambdaExprPtr>()) return {};

			// binds capture values
			if (auto bind = expr.isa<core::BindExprPtr>()) {
				core::ExpressionList res;
				for(const auto& cur : core::analysis::getFreeVariables(bind)) {
					res.push_back(cur);
				}
				return res;
			}

			// this should not be reachable
			assert_fail() << "Only lambdas and binds are supported, got: " << expr->getNodeType();
			return {};
		}


		core::FunctionTypePtr convertToFunctionTypeWithClosureParameter(const core::FunctionTypePtr& funType, const core::TypePtr& closureType) {
			core::NodeManager& mgr = funType.getNodeManager();
			core::IRBuilder builder(mgr);

			// also make sure it has a valid number of parameters
			auto numParameters = funType->getParameterTypeList().size();
			assert_le(1,numParameters);
			assert_le(numParameters,2);

			// start by creating the new function type
			core::TypeList paramTypes;
			paramTypes.push_back(closureType);
			if (numParameters > 1) {
				auto secondParameter = funType->getParameterTypeList()[1];

				// this second parameter is the tuple of recursive function call references
				auto tupleType = secondParameter.as<core::TupleTypePtr>();
				assert_eq(1,tupleType->size())
					<< "Mutual recursive functions not yet supported!";

				// this has to be a rec-fun type
				assert_true(lang::isRecFun(tupleType[0]));

				// update this rec-fun type to accept the closure type as an argument
				lang::RecFunType recFunType(tupleType[0]);
				recFunType.setParamType(paramTypes[0]);

				// make this the new type of the second parameter
				paramTypes.push_back(builder.tupleType({ recFunType.toIRType() }));
			}

			// construct the new function type
			return builder.functionType(paramTypes, funType->getReturnType());
		}


		/**
		 * Convert the given lambda or bind into a lambda extracting all the captured values and the first parameters value from a single closure parameter.
		 */
		core::LambdaExprPtr convertToLambdaWithClosureParameter(const core::ExpressionPtr& expr, const core::VariablePtr& parameter, const core::NodeMap& captureReplacements) {
			core::NodeManager& mgr = expr.getNodeManager();
			core::IRBuilder builder(mgr);

			// make sure the input is a lambda or bind
			assert_true(expr.isa<core::LambdaExprPtr>() || expr.isa<core::BindExprPtr>())
				<< "Unsupported variant implementation of type " << expr->getNodeType() << " encountered:\n" << dumpPretty(expr);

			// make sure the function is not recursive
			assert_true(!expr.isa<core::LambdaExprPtr>() || !expr.as<core::LambdaExprPtr>()->isRecursive())
				<< "Unable to support recursive functions here!";

			// get the input function type
			auto inputFunType = expr->getType().as<core::FunctionTypePtr>();

			// also make sure it has a valid number of parameters
			auto numParameters = inputFunType->getParameterTypeList().size();
			assert_le(1,numParameters);
			assert_le(numParameters,2);

			// construct the new function type
			auto closureType = core::lang::ReferenceType(parameter).getElementType();
			auto funType = convertToFunctionTypeWithClosureParameter(inputFunType, closureType);

			// extract the list of parameter types
			auto paramTypes = funType->getParameterTypes();

std::cout << "Converting " << *inputFunType << "\n"
			 "        to " << *funType << "\n\n";


			// assemble new parameter list
			core::VariableList params;
			params.push_back(parameter);

			core::VariablePtr recFunParam;
			if (numParameters > 1) {
				recFunParam = builder.variable(builder.refType(paramTypes[1]));
				params.push_back(recFunParam);
			}


			core::CompoundStmtPtr inputBody;
			if (auto lambda = expr.isa<core::LambdaExprPtr>()) {
				inputBody = lambda->getBody();
			} else if (auto bind = expr.isa<core::BindExprPtr>()) {

				// get the call expression
				auto call = bind->getCall();

				// and the function it is calling
				auto fun = call->getFunctionExpr().as<core::LambdaExprPtr>();

				// we inline its body here
				inputBody = fun->getBody();

				// by replacing the parameters by their arguments
				auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
				inputBody = core::transform::transformBottomUp(inputBody, [&](const core::NodePtr& node)->core::NodePtr {

					// we are interested in code reading parameters
					if (!core::analysis::isCallOf(node,refExt.getRefDeref())) return node;

					auto var = node.as<core::CallExprPtr>()->getArgument(0).isa<core::VariablePtr>();
					if (!var) return node;

					// check whether the variable is a parameter
					const auto& params = fun->getParameterList();
					auto pos = std::find(params.begin(),params.end(), var);
					if (pos == params.end()) return node;

					// replace this parameter read by the corresponding argument
					return call->getArgument(pos - params.begin());

				}).as<core::CompoundStmtPtr>();

				// replace captured values by access to the parameter tuple
				inputBody = core::transform::replaceAll(mgr, inputBody, captureReplacements, core::transform::localReplacement).as<core::CompoundStmtPtr>();

			}

			// get the old first parameter
			auto firstParam = (expr.isa<core::LambdaExprPtr>())
					? expr.as<core::LambdaExprPtr>()->getParameterList()[0]
					: expr.as<core::BindExprPtr>()->getParameters()[0];

			// add initialization of old parameter variable to body
			auto body = core::transform::insert(mgr,
				core::CompoundStmtAddress(inputBody),
				builder.declarationStmt(
						firstParam,
						(core::lang::isReference(firstParam))
							?               builder.refComponent(parameter,0)
							: builder.deref(builder.refComponent(parameter,0))
					),
				0
			).as<core::CompoundStmtPtr>();

std::cout << "Old Body:\n" << dumpPretty(inputBody) << "\n";
std::cout << "New Body:\n" << dumpPretty(body) << "\n";

			// get the number of elements in the closure
			auto closureSize = paramTypes[0].as<core::TupleTypePtr>().size();

			// update recursive calls
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			body = core::transform::transformBottomUp(body, [&](const core::NodePtr& node)->core::NodePtr {

				// only interested in calls
				auto call = node.isa<core::CallExprPtr>();
				if (!call) return node;

				// check that the target function is a call itself
				auto trgFunCall = call->getFunctionExpr().isa<core::CallExprPtr>();
				if (!trgFunCall) return node;

				// check that this target function is a call to recfun_2_fun
				if (!core::analysis::isCallOf(trgFunCall,ext.getRecfunToFun())) return node;

				// got a recursive call
				std::cout << "Found: " << dumpPretty(node) << "\n";

				// create a new argument
				core::ExpressionList args;
				args.push_back(call->getArgument(0));
				for(unsigned i = 1; i < closureSize; ++i) {
					args.push_back(builder.deref(builder.refComponent(parameter,i)));
				}
				auto newArg = builder.tupleExpr(args);

				// create a new call
				return builder.callExpr(
						builder.callExpr(
								ext.getRecfunToFun(),
								builder.deref(builder.refComponent(recFunParam,0))
						),
						newArg
				);

			}).as<core::CompoundStmtPtr>();


			// build new lambda
			auto res = builder.normalize(builder.lambdaExpr(funType, params, body));

std::cout << "Old Expr:\n" << dumpPretty(expr) << "\n";
std::cout << "New Expr:\n" << dumpPretty(res) << "\n";

			assert_true(core::checks::check(res).empty())
				<< core::checks::check(res);

			return builder.normalize(builder.lambdaExpr(funType, params, body));


//			// we have to treat lambdas and binds separately
//
//			// check for lambda expressions
//			if (auto lambda = expr.isa<core::LambdaExprPtr>()) {
//				return convertToLambdaWithClosureParameter(lambda,parameter);
//			}
//
//			// check for bind expressions
//			if (auto bind = expr.isa<core::BindExprPtr>()) {
//				return convertToLambdaWithClosureParameter(bind,parameter,captureReplacements);
//			}
//
//			// nothing else is supported
//			assert_fail() << "Unsupported variant implementation of type " << expr->getNodeType() << " encountered: " << *expr;
//			return core::LambdaExprPtr();
		}

		/**
		 * Returns a prec operation where all implementations are pure functions passing along closures,
		 * and the list of expressions to be captured in its initialization.
		 */
		std::pair<lang::PrecOperation,core::ExpressionList>
		convertToPureFunctionOp(const lang::PrecOperation& op) {

			core::NodeManager& mgr = op.getFunction().getBaseCaseTest().getNodeManager();
			core::IRBuilder builder(mgr);

			// retrieve the function
			const lang::PrecFunction& function = op.getFunction();

			// collect the closure of all tests and step cases
			core::ExpressionList captured;

			// a utility to record captured values
			auto recordCaptured = [&](const core::ExpressionPtr& val) {
				for(const auto& cur : captured) {
					if (cur == val) return;
				}
				captured.push_back(val);
			};

			// collect captured values
			for(auto val : getCapturedValues(function.getBaseCaseTest())) {
				recordCaptured(val);
			}
			for(auto cur : function.getBaseCases()) {
				for(auto val : getCapturedValues(cur)) {
					recordCaptured(val);
				}
			}
			for(auto cur : function.getStepCases()) {
				for(auto val : getCapturedValues(cur)) {
					recordCaptured(val);
				}
			}

			// create the closure type
			core::TypeList closureElements;
			closureElements.push_back(op.getParameterType());
			for(const auto& cur : captured) {
				if (core::lang::isReference(cur.getType())) {
					closureElements.push_back(core::lang::ReferenceType(cur.getType()).getElementType());
				} else {
					closureElements.push_back(cur.getType());
				}
			}


			// create the closure type to be passed along all functions
			auto closureType = builder.tupleType(closureElements);
std::cout << dumpColor(closureType) << "\n";

			// create a parameter for the closure type
			auto param = builder.variable(builder.refType(closureType));

			// create expressions accessing captured values in the form of a substitution map
			core::NodeMap capturedValueReplacements;
			for(unsigned i = 0; i < captured.size(); i++) {
				capturedValueReplacements[captured[i]] = builder.refComponent(param,i+1);
			}

			// replace implementations with implementations processing the closure
			auto baseCaseTest = convertToLambdaWithClosureParameter(
					function.getBaseCaseTest(), param, capturedValueReplacements
			);

			core::ExpressionList baseCases = ::transform(function.getBaseCases(), [&](const core::ExpressionPtr& cur)->core::ExpressionPtr {
				return convertToLambdaWithClosureParameter(cur,param,capturedValueReplacements);
			});

			core::ExpressionList stepCases = ::transform(function.getStepCases(), [&](const core::ExpressionPtr& cur)->core::ExpressionPtr {
				return convertToLambdaWithClosureParameter(cur,param,capturedValueReplacements);
			});

			// build up PreOperation with pure functions
			return {
				lang::PrecOperation({
					lang::PrecFunction(
						baseCaseTest, baseCases, stepCases
					)
				}),
				captured
			};
		}



		core::NodePtr convertPrecOperator(const be::Converter& converter, const core::NodePtr& code) {

			// only interested in prec operators
			if (!lang::PrecOperation::isPrecOperation(code)) return code;


			// parse the pre operator
			lang::PrecOperation op = lang::PrecOperation::fromIR(code.as<core::ExpressionPtr>());
			assert_eq(1,op.getFunctions().size())
				<< "Mutual recursive definitions not yet supported!";

			// convert the prec operator into something that passed captured state in closures
			auto preprocessed = convertToPureFunctionOp(op);
			auto closedOp = preprocessed.first;
			auto captured = preprocessed.second;


			// get build utilities
			core::NodeManager& mgr = code.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

			// get a name for the work item
			const auto& name = converter.getNameManager().getName(code,"wi");

			// parse prec operation
//			lang::PrecOperation op = lang::PrecOperation::fromIR(code.as<core::ExpressionPtr>());

			// extract a sequential implementation of the prec operation
			auto process = getProcessVariant(closedOp);

			// extract a parallel implementation of the prec operation
			auto split = getSplitVariant(name,closedOp);

			// wrap it up in a work item
			WorkItemDescription desc(name,process,split);

			std::cout << "Building top-level wrapper!\n";

			// create a function wrapping the spawn call (need for bind)
			core::VariableList params;

			// add the input parameter
			params.push_back(builder.variable(builder.refType(op.getFunction().getParameterType())));

			// add captured closure parameters
			for(const auto& cur : captured) {
				params.push_back(builder.variable(cur->getType()));
			}

			core::ExpressionList args;
			args.push_back(desc.toIR(mgr));

			args.push_back(builder.tupleExpr(::transform(params,[&](const core::ExpressionPtr& cur)->core::ExpressionPtr {
				return builder.deref(cur);
			})));

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

			std::cout << "Inner call done, processing bind ...\n";

			// create a bind spawning the work item
			auto param = builder.variable(op.getParameterType());
			core::ExpressionList bindArgs;
			bindArgs.push_back(param);
			for(const auto& cur : captured) {
				bindArgs.push_back(builder.deref(cur));
			}
			return builder.bindExpr({ param }, builder.callExpr(nestedLambda, bindArgs));
		}

	}



	core::NodePtr PrecConverter::process(const be::Converter& converter, const core::NodePtr& code) {

		// replace all prec calls with actual lambdas
		auto res = core::transform::transformBottomUp(code, [&](const core::NodePtr& cur){
			return convertPrecOperator(converter,cur);
		}, core::transform::globalReplacement);

		// check that the result is properly typed
		assert_true(core::checks::check(res).empty())
			<< dumpPretty(res) << "\n"
			<< core::checks::check(res);

		// return result
		return res;

	}



	insieme::core::NodePtr CppLambdaToBindConverter::process(const insieme::backend::Converter&, const insieme::core::NodePtr& code) {

		core::NodeManager& mgr = code.getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

		// transform all lambda_to_closure calls
		auto res = core::transform::transformBottomUp(code,[&](const insieme::core::NodePtr& node)->insieme::core::NodePtr {

			// only interested in lambda_to_closure calls
			auto call = node.isa<core::CallExprPtr>();
			if (!call || !core::analysis::isCallOf(call, ext.getLambdaToClosure())) {
				return node;
			}

			// TODO: combine constructor call and call operator call into a single function to form the body of the bind

			// get the nested struct
			auto cppLambdaType = core::lang::ReferenceType(call->getArgument(0)->getType()).getElementType();
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

			// get resulting function type
			auto funType = call->getType().as<core::FunctionTypePtr>();

			// create parameters
			core::VariableList params;
			core::ExpressionList args;
			args.push_back(call->getArgument(0));
			for(const auto& cur : funType->getParameterTypeList()) {
				auto param = builder.variable(cur);
				params.push_back(param);
				args.push_back(param);
			}

			// build call to member function
			auto body = builder.callExpr(callOperator->getImplementation(),args);

			// replace by a bind
			return builder.bindExpr(funType, params, body);

		}, core::transform::globalReplacement);

		// check the result
		assert_true(core::checks::check(res).empty())
			<< core::checks::check(res);

		// return result
		return res;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
