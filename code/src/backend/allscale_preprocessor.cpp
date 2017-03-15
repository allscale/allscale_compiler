#include "allscale/compiler/backend/allscale_preprocessor.h"

#include <map>
#include <string>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/printer/error_printer.h"

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

			// check that what has been build is properly composed
			assert_correct_ir(prog)
				<< "Invalid input program for EntryPointWrapper\n";

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
				args.push_back(builder.deref(builder.refComponent(param,i)));
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
				<< dumpPretty(main) << "\n"
				<< insieme::core::printer::dumpErrors(core::checks::check(main));

			// convert this main into a work item
			WorkItemVariant impl(main);
			WorkItemDescription desc("main", impl);


			// create a new main entry point
			core::ExpressionList entryPointArgs;
			entryPointArgs.push_back(desc.toIR(mgr));
			for(const auto& cur : oldMain->getParameterList()->getParameters()) {
				entryPointArgs.push_back(builder.deref(cur));
			}

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
				<< dumpColor(main) << "\n"
				<< core::checks::check(main);

			// wrap up into a program, and be done
			return builder.program(core::ExpressionList{ main });
		}

	}

	namespace {

		core::ExpressionPtr inlineStep(const core::ExpressionPtr& stepCase, const core::ExpressionPtr& recFun, bool serialize) {
			auto& mgr = stepCase->getNodeManager();
			core::IRBuilder builder(stepCase->getNodeManager());
			auto& basic = mgr.getLangBasic();

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

				// replace arguments of call to recFun by unpacking the tuple, if necessary
				if (core::analysis::isCallOf(node,recFun)) {
					// NOTE: this is necessary since the runtime interface requests those parameters unpacked

					// check some expected properties
					auto call = node.as<core::CallExprPtr>();
					assert_eq(1,call->getNumArguments());
					assert_true(call->getArgument(0).isa<core::TupleExprPtr>());

					// extract the type argument list
					core::TypeList types;
					for(const auto& cur : call->getArgumentList()) {
						types.push_back(cur->getType());
					}

					// check whether this call is properly typed
					if (core::types::deduceReturnType(recFun->getType().as<core::FunctionTypePtr>(), types, false)) return node;

					// otherwise unpack the argument type
					core::ExpressionList args;
					for(const auto& cur : call->getArgument(0).as<core::TupleExprPtr>()->getExpressions()) {
						args.push_back(cur);
					}

					// create new call
					return builder.callExpr(call->getFunctionExpr(),args);
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
					}, core::transform::globalReplacement);
				};

				// get body, replace treeture operations and recFun calls
				body = core::transform::transformBottomUp(body, [&](const core::NodePtr& node)->core::NodePtr {

					// only interested in calls
					auto call = node.isa<core::CallExprPtr>();
					if (!call) return node;

					if (core::analysis::isCallOf(call,ext.getTreetureDone())) {
						return builder.callExpr(basic.getId(), call->getArgument(0));
					}

					if (core::analysis::isCallOf(call,ext.getTreetureRun())) {
						return call->getArgument(0);
					}

					if (core::analysis::isCallOf(call,ext.getTreetureGet())) {
						return call->getArgument(0);
					}

					if (core::analysis::isCallOf(call,ext.getTreetureCombine())) {
						auto arg0 = removeTreetures(call->getArgument(1)).as<core::ExpressionPtr>();
						auto arg1 = removeTreetures(call->getArgument(2)).as<core::ExpressionPtr>();
						return builder.callExpr(call->getArgument(3),arg0,arg1);
					}

					// not of interest either
					return node;
				}, core::transform::globalReplacement).as<core::CompoundStmtPtr>();


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


		core::LambdaExprPtr getSequentialImplementation(const lang::PrecOperation& op) {
			core::IRBuilder builder(op.getFunction().getBaseCaseTest()->getNodeManager());

			// -- build up the sequential implementation of this function --

			assert_eq(1,op.getFunctions().size())
				<< "Mutual recursive functions not yet supported!";

			// get the function to be encoded
			const auto& fun = op.getFunction();

			// get the in-parameter
			auto in = builder.variable(fun.getParameterType());
			auto inVal = builder.deref(in);

			// get the type of the resulting function (same as the base case type)
			auto funType = builder.functionType({ in->getType() }, op.getResultType());

			// create the recursive function reference
			auto recFun = builder.lambdaReference(funType,"rec");

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

		core::LambdaExprPtr getParallelImplementation(const string& wi_name, const lang::PrecOperation& op) {
			auto& mgr = op.getFunction().getBaseCaseTest()->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			auto& ext2 = mgr.getLangExtension<AllScaleBackendModule>();

			// -- build up the sequential implementation of this function --

			assert_eq(1,op.getFunctions().size())
				<< "Mutual recursive functions not yet supported!";

			// get the function to be encoded
			const auto& fun = op.getFunction();

			// get the in-parameter
			auto in = builder.variable(fun.getParameterType());

			// get the type of the resulting function
			auto funType = builder.functionType(in->getType(), op.getTreetureType().toIRType());

			// create the recursive function reference
			auto recFun =
					builder.callExpr(
						ext2.getRecSpawnWorkItem(),
						builder.callExpr(
							ext2.getCreateWorkItemDescriptionReference(),
							builder.getIdentifierLiteral(wi_name),
							builder.getTypeLiteral(op.getParameterType()),
							builder.getTypeLiteral(op.getResultType())
						)
					);

			// get instantiated step implementation
			auto stepFun = inlineStep(fun.getStepCases()[0],recFun,false);

			// create the body of the lambda
			auto body = builder.compoundStmt(
				builder.ifStmt(
					// check the base case test
					builder.callExpr(fun.getBaseCaseTest(), in),
					// if in the base case => run base case
					builder.returnStmt(builder.callExpr(ext.getTreetureDone(), builder.callExpr(fun.getBaseCases()[0],in))),
					// else run step case
					builder.returnStmt(builder.callExpr(stepFun,in))
				)
			);

			// build the resulting lambda
			return builder.lambdaExpr(funType,{in},body);
		}


		WorkItemVariant getProcessVariant(const lang::PrecOperation& op) {

			// pick the base case implementation
			auto lambda = getSequentialImplementation(op);

			core::NodeManager& mgr = lambda.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

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
			auto lambda = getParallelImplementation(wi_name,op);

			// use this lambda for creating the work item variant
			return WorkItemVariant(lambda);
		}


		// --- utilities to convert PrecOperators with bind expressions into an Equivalent PrecOperator with lambdas ---

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
		core::LambdaExprPtr convertToLambdaWithClosureParameter(const core::ExpressionPtr& expr, const core::VariablePtr& parameter, const core::NodeMap& captureReplacements, bool debug) {

			core::NodeManager& mgr = expr.getNodeManager();
			core::IRBuilder builder(mgr);

			// make sure the input is a lambda or bind
			assert_true(expr.isa<core::LambdaExprPtr>() || expr.isa<core::BindExprPtr>())
				<< "Unsupported variant implementation of type " << expr->getNodeType() << " encountered:\n" << dumpPretty(expr);

			// make sure the function is not recursive
			assert_true(!expr.isa<core::LambdaExprPtr>() || !expr.as<core::LambdaExprPtr>()->isRecursive())
				<< "Unable to support recursive functions here!";

			// print initial function to be converted
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "Started converting expression to lambda:\n";
				std::cout << dumpReadable(expr) << "\n";
				std::cout << "Provided parameter: " << *parameter << " : " << *parameter->getType() << "\n\n";
			}

			// get the input function type
			auto inputFunType = expr->getType().as<core::FunctionTypePtr>();

			// also make sure it has a valid number of parameters
			auto numParameters = inputFunType->getParameterTypeList().size();
			assert_le(1,numParameters);
			assert_le(numParameters,2);

			// make sure the new parameter type is a cpp reference containing a tuple
			assert_pred1(core::lang::isCppReference, parameter->getType());
			assert_true(core::analysis::getReferencedType(parameter).isa<core::TupleTypePtr>());

			// construct the new function type
			auto paramType = parameter->getType();
			auto closureType = core::analysis::getReferencedType(paramType).as<core::TupleTypePtr>();
			auto funType = convertToFunctionTypeWithClosureParameter(inputFunType, paramType);

			// extract the list of parameter types
			auto paramTypes = funType->getParameterTypes();

			// print type debug message
			if (debug) {
				std::cout << "Converting expression of type\n" << dumpReadable(inputFunType) << "\n"
							 "       to lambda expr of type\n" << dumpReadable(funType) << "\n\n";
			}

			// assemble new parameter list
			core::VariableList params;
			params.push_back(parameter);

			core::VariablePtr recFunParam;
			if (numParameters > 1) {
				recFunParam = builder.variable(builder.refType(paramTypes[1]));
				params.push_back(recFunParam);
			}

			// get the number of elements in the closure
			auto closureSize = closureType->size();

			// create the operator to update recursive calls
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			auto fixRecursiveCalls = [&](const core::CompoundStmtPtr& body) {
				return core::transform::transformBottomUp(body, [&](const core::NodePtr& node)->core::NodePtr {

					// only interested in calls
					auto call = node.isa<core::CallExprPtr>();
					if (!call) return node;

					// check that the target function is a call itself
					auto trgFunCall = call->getFunctionExpr().isa<core::CallExprPtr>();
					if (!trgFunCall) return node;

					// check that this target function is a call to recfun_2_fun
					if (!core::analysis::isCallOf(trgFunCall,ext.getRecfunToFun())) return node;

					// create a new argument
					core::ExpressionList args;

					// add recursive parameter
					if (core::lang::isReference(call->getArgument(0))) {
						args.push_back(builder.deref(call->getArgument(0)));
					} else {
						args.push_back(call->getArgument(0));
					}

					// forward captured values
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
			};


			core::CompoundStmtPtr inputBody;
			if (auto lambda = expr.isa<core::LambdaExprPtr>()) {
				inputBody = fixRecursiveCalls(lambda->getBody());
			} else if (auto bind = expr.isa<core::BindExprPtr>()) {

				// get the call expression
				auto call = bind->getCall();

				// inline functions
				if (auto fun = call->getFunctionExpr().isa<core::LambdaExprPtr>()) {

					// we inline its body here
					inputBody = fixRecursiveCalls(fun->getBody());

					// get the list of free parameters of the body
					auto freeParams = core::analysis::getFreeVariables(inputBody);

					// start by declaring all free parameters of the body as local variables
					core::StatementList newBodyStmts;
					auto paramList = fun->getParameterList();
					auto argList = call->getArgumentList();
					for(unsigned i=0; i<paramList.size(); ++i) {
						if (contains(freeParams, paramList[i])) {
							newBodyStmts.push_back(builder.declarationStmt(paramList[i], argList[i]));
						}
					}

					// build the new body
					newBodyStmts.push_back(inputBody);
					inputBody = builder.compoundStmt(newBodyStmts);

				} else {

					// otherwise just wrap the call expression
					inputBody = fixRecursiveCalls(builder.compoundStmt(builder.returnStmt(call)));

				}

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

			// print updated bodies
			if (debug) {
				std::cout << "Old Body:\n" << dumpReadable(inputBody) << "\n";
				std::cout << "New Body:\n" << dumpReadable(body) << "\n";
			}

			// build new lambda
			auto res = builder.normalize(builder.lambdaExpr(funType, params, body));

			// print conversion result
			if (debug) {
				std::cout << "Old Expr:\n" << dumpReadable(expr) << "\n";
				std::cout << "New Expr:\n" << dumpReadable(res) << "\n";
			}

			// check that everything is composed correctly
			assert_true(core::checks::check(res).empty())
				<< "Errors:\n" << core::checks::check(res)
				<< "Code:\n" << core::printer::dumpErrors(core::checks::check(res));

			// done
			return res;
		}

		/**
		 * Returns a prec operation where all implementations are pure functions passing along closures,
		 * and the list of expressions to be captured in its initialization.
		 */
		std::pair<lang::PrecOperation,core::ExpressionList>
		convertToPureFunctionOp(const lang::PrecOperation& op, bool debug) {

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

			// log the list of captured values
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "captured values:";
				if (captured.empty()) {
					std::cout << " - none - \n";
				} else {
					std::cout << "\n\t" << join("\n\t", captured, [](std::ostream& out, const core::ExpressionPtr& expr) {
						out << *expr << " : " << *expr->getType();
					}) << "\n";
				}
			}


			// create the closure type
			core::TypeList closureElements;

			// the parameter must always be captured by value
			auto paramType = op.getParameterType();
			if (core::lang::isReference(paramType)) {
				closureElements.push_back(core::lang::ReferenceType(paramType).getElementType());
			} else {
				closureElements.push_back(paramType);
			}

			// add captured values
			for(const auto& cur : captured) {
				closureElements.push_back(cur.getType());
			}

			// create the closure type to be passed along all functions
			auto closureType = builder.tupleType(closureElements);

			// log the closure type
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "closure type: " << *closureType << "\n";
			}


			// create a parameter for the closure type
			auto param = builder.variable(builder.refType(closureType, true, false, core::lang::ReferenceType::Kind::CppReference));

			// create expressions accessing captured values in the form of a substitution map
			core::NodeMap capturedValueReplacements;
			for(unsigned i = 0; i < captured.size(); i++) {
				capturedValueReplacements[captured[i]] = builder.deref(builder.refComponent(param,i+1));
			}

			// replace implementations with implementations processing the closure
			auto baseCaseTest = convertToLambdaWithClosureParameter(
					function.getBaseCaseTest(), param, capturedValueReplacements, debug
			);

			core::ExpressionList baseCases = ::transform(function.getBaseCases(), [&](const core::ExpressionPtr& cur)->core::ExpressionPtr {
				return convertToLambdaWithClosureParameter(cur,param,capturedValueReplacements,debug);
			});

			core::ExpressionList stepCases = ::transform(function.getStepCases(), [&](const core::ExpressionPtr& cur)->core::ExpressionPtr {
				return convertToLambdaWithClosureParameter(cur,param,capturedValueReplacements,debug);
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
			const bool debug = false;

			// -- Step 1: Preparation --

			// make sure the given code is a prec operator invocation
			assert_true(lang::PrecOperation::isPrecOperation(code));

			// print initial state of conversion
			if (debug) {
				std::cout << "\n================ prec conversion :: begin =========================\n";
				std::cout << " - Input -\n" << dumpReadable(code) << "\n";
			}

			// get build utilities
			core::NodeManager& mgr = code.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

			// parse the pre operator
			lang::PrecOperation op = lang::PrecOperation::fromIR(code.as<core::ExpressionPtr>());
			assert_eq(1,op.getFunctions().size())
				<< "Mutual recursive definitions not yet supported!";



			// -- Step 1: Convert bindings into lambdas, collect captured values --

			// convert the prec operator into something that passed captured state in closures
			auto preprocessed = convertToPureFunctionOp(op,debug);
			auto closedOp = preprocessed.first;
			auto captured = preprocessed.second;

			// print result of conversion step
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - lambdas only version -\n" << dumpReadable(closedOp.toIR(mgr)) << "\n";
			}

			// check for errors in the lambda conversion
			assert_true(core::checks::check(closedOp.toIR(mgr)).empty())
				<< core::printer::dumpErrors(core::checks::check(closedOp.toIR(mgr)));


			// -- Step 2: create process and split version --

			// get a name for the work item
			const auto& name = converter.getNameManager().getName(code,"wi");

			// extract a sequential implementation of the prec operation
			auto process = getProcessVariant(closedOp);

			// extract a parallel implementation of the prec operation
			auto split = getSplitVariant(name,closedOp);


			// log the process and split variants
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - process variant -\n" << dumpReadable(process.getImplementation()) << "\n";
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - split variant -\n" << dumpReadable(split.getImplementation()) << "\n";
			}

			// check consistency of process variant
			assert_true(core::checks::check(process.getImplementation()).empty())
				<< core::printer::dumpErrors(core::checks::check(process.getImplementation()));

			// check consistency of split variant
			assert_true(core::checks::check(split.getImplementation()).empty())
				<< core::printer::dumpErrors(core::checks::check(split.getImplementation()));


			// -- Step 3: work item conversion --

			// wrap it up in a work item
			WorkItemDescription desc(name,process,split);

			// create a function wrapping the spawn call (need for bind)
			core::VariableList params;

			// add the input parameter
			params.push_back(builder.variable(builder.refType(op.getFunction().getParameterType())));

			// add captured closure parameters
			for(const auto& cur : captured) {
				params.push_back(builder.variable(builder.refType(cur->getType())));
			}

			// assemble arguments for inner call
			core::ExpressionList args;
			args.push_back(desc.toIR(mgr));
			args.push_back(builder.deref(params[0]));
			for(unsigned i=1; i<params.size(); ++i) {
				args.push_back(builder.deref(params[i]));
			}

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

			// create a bind spawning the work item
			auto param = builder.variable(op.getParameterType());
			core::ExpressionList bindArgs;
			bindArgs.push_back(param);
			for(const auto& cur : captured) {
				bindArgs.push_back(cur);
			}
			auto res = builder.bindExpr({ param }, builder.callExpr(nestedLambda, bindArgs));

			// report final call
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - work item creation call -\n" << dumpReadable(res) << "\n";
			}

			// check result
			assert_true(core::checks::check(res).empty())
				<< core::printer::dumpErrors(core::checks::check(res));

			// report end of conversion
			if (debug) {
				std::cout << "================ prec conversion :: done ==========================\n\n";
			}

			// done
			return res;
		}

	}



	core::NodePtr PrecConverter::process(const be::Converter& converter, const core::NodePtr& code) {

		// replace all prec calls with actual lambdas
		auto res = core::transform::transformBottomUp(code, [&](const core::NodePtr& cur){

			// interested in recfun_to_fun call enclosing a prec operator
			if (!lang::isRecFunUnwrapperCall(cur)) return cur;

			// convert to a call expression
			auto call = cur.as<core::CallExprPtr>();
			assert_false(call.empty()) << "Invalid IR composition - there has to be one argument!";

			// extract the first argument
			auto arg = cur.as<core::CallExprPtr>()->getArgument(0);

			// only interested in prec operators
			if (!lang::PrecOperation::isPrecOperation(arg)) return cur;

			// trigger conversion
			return convertPrecOperator(converter,arg);

		}, core::transform::globalReplacement);

		// check that the result is properly typed
		assert_true(core::checks::check(res).empty())
			<< core::printer::dumpErrors(core::checks::check(res));

		// return result
		return res;

	}



	namespace {

		insieme::core::LambdaExprPtr getCallOperatorImpl(const insieme::core::ExpressionPtr& lambda) {

			auto cppLambdaType = core::lang::ReferenceType(lambda->getType()).getElementType();
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

			// extract a lambda
			auto impl = callOperator->getImplementation().isa<core::LambdaExprPtr>();

			assert_true(impl) << "Lambda implementation must not be abstract!";

			return impl;
		}

		insieme::core::BindExprPtr convertToBind(const insieme::core::CallExprPtr& call) {
			core::NodeManager& mgr = call->getNodeManager();
			core::IRBuilder builder(mgr);

			assert_pred2(core::analysis::isCallOf, call, mgr.getLangExtension<lang::AllscaleModule>().getCppLambdaToClosure());

			// TODO: combine constructor call and call operator call into a single function to form the body of the bind

			// get the call operator of the passed lambda
			auto callOperator = getCallOperatorImpl(call->getArgument(0));

			// get resulting function type
			auto resFunType = call->getType().as<core::FunctionTypePtr>();

			// create parameters
			core::VariableList params;
			core::ExpressionList args;
			args.push_back(call->getArgument(0));

			// need to align in and out parameter types
			auto resFunParamTypes = resFunType->getParameterTypeList();
			auto opFunParamTypes = callOperator->getFunctionType()->getParameterTypeList();
			assert_eq(resFunParamTypes.size() + 1, opFunParamTypes.size());

			// map incoming parameters to outgoing parameters
			for(unsigned i = 0; i < resFunParamTypes.size(); ++i) {

				auto inParamType = resFunParamTypes[i];
				auto outParamType = opFunParamTypes[i+1];

				auto param = builder.variable(inParamType);
				params.push_back(param);

				// add a deref if necessary
				if (inParamType == outParamType) {
					args.push_back(param);
				} else {
					args.push_back(builder.deref(param));
				}
			}

			// build call to member function
			auto body = builder.callExpr(callOperator,args);

			// replace by a bind
			auto res = builder.bindExpr(resFunType, params, body);

			// a final correctness check
			assert_correct_ir(res);

			// replace by a bind
			return res;

		}

		insieme::core::LambdaExprPtr convertToLambda(const insieme::core::CallExprPtr& call) {
			core::NodeManager& mgr = call->getNodeManager();
			core::IRBuilder builder(mgr);

			assert_pred2(core::analysis::isCallOf, call, mgr.getLangExtension<lang::AllscaleModule>().getCppLambdaToLambda());

			// get the call operator of the passed lambda
			auto callOperator = getCallOperatorImpl(call->getArgument(0));

			assert_false(callOperator->isRecursive())
				<< "Recursive functions are not supported here!";

			auto opParams = callOperator->getParameterList();
			core::VariableList params(opParams.begin()+1,opParams.end());

			return builder.lambdaExpr(call->getType().as<core::FunctionTypePtr>(), params, callOperator->getBody());
		}

	}



	insieme::core::NodePtr CppLambdaToIRConverter::process(const insieme::backend::Converter&, const insieme::core::NodePtr& code) {

		core::NodeManager& mgr = code.getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<lang::AllscaleModule>();

		// transform all lambda_to_closure calls
		auto res = core::transform::transformBottomUp(code,[&](const insieme::core::NodePtr& node)->insieme::core::NodePtr {

			// only interested in calls ..
			auto call = node.isa<core::CallExprPtr>();
			if (!call) return node;

			// .. to the lambda_to_closure
			if (core::analysis::isCallOf(call,ext.getCppLambdaToClosure())) {
				return convertToBind(call);
			}

			// .. or the lambda_to_lambda
			if (core::analysis::isCallOf(call,ext.getCppLambdaToLambda())) {
				return convertToLambda(call);
			}

			// not interested in the rest
			return node;

		}, core::transform::globalReplacement);

		// check the result
		assert_correct_ir(res);

		// return result
		return res;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
