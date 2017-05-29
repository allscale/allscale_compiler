#include "allscale/compiler/backend/allscale_preprocessor.h"

#include <map>
#include <string>

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/analysis/type_utils.h"

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
					assert_true(call->getArgument(0).isa<core::InitExprPtr>());

					// extract the type argument list
					core::TypeList types;
					for(const auto& cur : call->getArgumentList()) {
						types.push_back(cur->getType());
					}

					// check whether this call is properly typed
					if (core::types::deduceReturnType(recFun->getType().as<core::FunctionTypePtr>(), types, false)) return node;

					// otherwise unpack the argument type
					core::ExpressionList args;
					const auto& refDeref = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefDeref();
					for(const auto& cur : call->getArgument(0).as<core::InitExprPtr>()->getInitExprList()) {
						// remove unnecessary ref-deref calls
						auto arg = cur;
						if (!core::lang::isReference(arg) && core::analysis::isCallOf(arg,refDeref)) {
							arg = cur.as<core::CallExprPtr>()->getArgument(0);
						}

						if(core::lang::isCppReference(arg)) {
							core::lang::ReferenceType refType(arg);
							if (!refType.isConst()) {
								refType.setConst(true);
								arg = core::lang::buildRefCast(arg,refType.toType());
							}
						}

						args.push_back(arg);
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
				auto treetureConnector = builder.parseExpr("lit(\"connect\":('a,'b)->unit)");
				assert_true(treetureConnector);
				body = core::transform::transformBottomUp(body, [&](const core::NodePtr& node)->core::NodePtr {

					// check whether it is a return of a former treeture combinator
					if (auto ret = node.isa<core::ReturnStmtPtr>()) {
						if (core::analysis::isCallOf(ret->getReturnExpr(),treetureConnector)) {
							// replace this one by a list of statements
							auto connectorCall = ret->getReturnExpr().as<core::CallExprPtr>();
							return builder.compoundStmt(
								connectorCall->getArgument(0),
								connectorCall->getArgument(1),
								builder.returnStmt()
							);
						}
					}

					// for the rest: only interested in calls
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

					if (core::analysis::isCallOf(call,ext.getTreetureParallel()) || core::analysis::isCallOf(call,ext.getTreetureSequential())) {
						auto arg0 = removeTreetures(call->getArgument(1)).as<core::ExpressionPtr>();
						auto arg1 = removeTreetures(call->getArgument(2)).as<core::ExpressionPtr>();
						return builder.callExpr(treetureConnector,arg0,arg1);
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


		// -- Extraction of captured values ----------------------------------------------------------------------------

		struct CapturedValue {

			core::TagTypeReferencePtr lambdaTypeName;

			core::FieldPtr field;

			core::ExpressionPtr value;

			friend std::ostream& operator<<(std::ostream& out, const CapturedValue& value) {
				return out << *value.lambdaTypeName << "::" << *value.field->getName() << " : " << *value.field->getType() << " = " << *value.value;
			}
		};

		class CapturedValueIndex {

			std::vector<CapturedValue> values;

		public:

			void append(const CapturedValue& value) {
				values.push_back(value);
			}

			std::size_t getIndex(const core::TagTypeReferencePtr& lambda, const core::ExpressionPtr& fieldIdentifier) const {
				assert_true(fieldIdentifier.isa<core::LiteralPtr>());

				auto name = fieldIdentifier.as<core::LiteralPtr>()->getValue();
				for(std::size_t i = 0; i<values.size(); ++i) {
					// check current entry
					if (values[i].lambdaTypeName == lambda && values[i].field->getName() == name) return i;
				}
				assert_fail() << "Unable to obtain field " << *lambda << "::" << *fieldIdentifier << " from list of captured values: " << *this;
				return -1;
			}

			bool empty() const {
				return values.empty();
			}

			std::size_t size() const {
				return values.size();
			}

			const CapturedValue& operator[](std::size_t index) const {
				return values[index];
			}

			auto begin() const {
				return values.begin();
			}

			auto end() const {
				return values.end();
			}

			friend std::ostream& operator<<(std::ostream& out, const CapturedValueIndex& index) {
				return out << index.values;
			}
		};

		CapturedValueIndex collectedCapturedValues(const core::NodePtr& code) {
			assert_pred1(lang::PrecOperation::isPrecOperation,code);

			// start index of captured values
			CapturedValueIndex res;

			// locate all top-level cpp_lambda_to_closure calls
			core::NodeManager& mgr = code.getNodeManager();
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			core::visitDepthFirstOncePrunable(code,[&](const core::CallExprPtr& call){

				// check whether it is a cpp_lambda_to_closure call
				if (!core::analysis::isCallOf(call, ext.getCppLambdaToClosure())) return core::Descent;

				// make sure that the first argument is a constructor call
				auto initExpr = call->getArgument(0).isa<core::InitExprPtr>();

				// get the lambda type
				auto lambdaType = core::analysis::getReferencedType(initExpr->getType());

				// make sure it is a tag type -- TODO: add tag type reference lookup
				core::TagTypePtr lambdaClass = lambdaType.isa<core::TagTypePtr>();
				assert_true(lambdaClass);

				// get fields
				auto fields = lambdaClass->getFields();

				// get init values
				auto initValues = initExpr->getInitExprList();
				assert_eq(fields.size(),initValues.size());

				// link fields and init values
				for(std::size_t i = 0; i<fields.size(); i++) {
					auto field = fields[i];
					auto init = initValues[i];
					res.append(CapturedValue{lambdaClass->getTag(),field,init});
				}

				// done -- do not descent further
				return core::Prune;
			});

			// obtain result
			return res;
		}


		// -- Conversion of CppLambdas to Lambdas ----------------------------------------------------------------------

		core::LambdaExprPtr getCallOperatorImplementation(const insieme::core::ExpressionPtr& lambda) {

			auto cppLambdaType = lambda->getType();
			if (core::lang::isReference(cppLambdaType)) {
				cppLambdaType = core::lang::ReferenceType(cppLambdaType).getElementType();
			}
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

		/**
		 * Converts a given C++ lambda, forming one of the functions bening passed to the prec operator,
		 * into an equivalent function accepting the given type of closure parameter type.
		 */
		core::ExpressionPtr convertToLambdaAcceptingClosure(const core::TypePtr& closureParamType,const CapturedValueIndex& index,const core::ExpressionPtr& lambdaExpr) {
			auto& mgr = closureParamType.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& asExt = mgr.getLangExtension<lang::AllscaleModule>();

			// -- extract lambda --

			// check whether it is a lambda already
			auto lambda = lambdaExpr.isa<core::LambdaExprPtr>();

			// if not, it may be a c++ lambda, from which the call operator member function needs to be extracted
			if (!lambda) {

				// check some pre-conditions
				assert_pred2(core::analysis::isCallOf,lambdaExpr,asExt.getCppLambdaToClosure());

				// unpack the constructor of the lambda
				auto cppLambda = lambdaExpr.as<core::CallExprPtr>()->getArgument(0);

				// extract call operator implementation from the given lambda
				lambda = getCallOperatorImplementation(cppLambda);

			}

			// -- start conversion --
			auto oldImpl = lambda;

			// determine whether it is a member function (form a C++ lambda) or not
			bool isMember = oldImpl->getFunctionType()->isMemberFunction();

			// create the new parameter of the closure type
			auto param = builder.variable(closureParamType);

			// get the (only for the step case present) recursive call parameter
			auto recFunParam = (oldImpl->getParameterList().size() == (isMember ? 3 : 2)) ? oldImpl->getParameterList()[(isMember ? 2 : 1)] : core::VariablePtr();

			// alter recFunParam to fit new parameter type
			auto newRecFunParam = recFunParam;
			if (recFunParam) {
				lang::RecFunType funType(core::analysis::getReferencedType(recFunParam->getType()));
				funType.setParamType(core::analysis::getReferencedType(closureParamType));
				newRecFunParam = builder.variable(
					builder.refType(
							builder.tupleType({funType.toIRType()}),
							false,false,core::lang::ReferenceType::Kind::Plain
					)
				);
			}

			// define the new function type
			auto funType = (newRecFunParam)
				? builder.functionType({ closureParamType, core::analysis::getReferencedType(newRecFunParam->getType()) }, oldImpl->getFunctionType()->getReturnType())
				: builder.functionType({ closureParamType }, oldImpl->getFunctionType()->getReturnType());


			// harvest some material from the old implementation
			auto thisValue = (isMember) ? builder.deref(oldImpl->getParameterList()[0]) : core::ExpressionPtr();
			auto lambdaType = (isMember) ? core::analysis::getReferencedType(lambdaExpr.as<core::CallExprPtr>()->getArgument(0)).as<core::TagTypePtr>()->getTag() : core::TagTypeReferencePtr();
			auto oldParam = oldImpl->getParameterList()[ isMember ? 1 : 0 ];

			// build the replacement for the original parameter access
			core::ExpressionPtr paramAccess = builder.refComponent(param,0);
			if (paramAccess->getType() != oldParam->getType()) {
				paramAccess = core::lang::buildRefCast(paramAccess,oldParam->getType());
			}

			// build a new body
			auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
			auto newBody = core::transform::transformBottomUpGen(lambda->getBody(),[&](const core::NodePtr& node)->core::NodePtr {
				// exchange parameter
				if (*node == *oldParam) return paramAccess;

				// exchange recursive calls
				if (auto call = node.isa<core::CallExprPtr>()) {
					auto trg = call->getFunctionExpr();
					if(asExt.isCallOfRecfunToFun(trg) || asExt.isCallOfRecfunToDepFun(trg)) {

						// TODO: support this by forwarding an additional dependency parameter before the closure parameter
						assert_false(asExt.isCallOfRecfunToDepFun(trg))
							<< "Passing of dependencies not supported yet.";

						// pack the recursive argument
						core::ExpressionList closureValues;
						closureValues.push_back(node.as<core::CallExprPtr>()->getArgument(0));

						// add the captured values
						for(std::size_t i = 0; i<index.size(); ++i) {
							// get the value from the closure
							core::ExpressionPtr forward = builder.refComponent(param,i+1);

							// if the captured values is a reference value
							if (core::lang::isReference(index[i].field->getType())) {
								// we need to de-ref the captured value
								forward = builder.deref(forward);
							}

							// add result to parameter list
							closureValues.push_back(forward);
						}

						// build recursive call
						core::ExpressionList args;
						args.push_back(builder.initExprTemp(closureParamType,closureValues));
						return builder.callExpr(lang::buildRecfunToFun(builder.accessComponent(builder.deref(newRecFunParam),0)),args);
					}
				}

				// replace other accesses to captured values
				if (isMember && core::analysis::isCallOf(node,refExt.getRefMemberAccess())) {

					// check that it is accessing this
					auto call = node.as<core::CallExprPtr>();
					if (*call->getArgument(0) == *thisValue) {

						// get the index within the closure tuple
						auto pos = index.getIndex(lambdaType,call->getArgument(1));

						// replace by access to closure element
						core::ExpressionPtr res = builder.refComponent(param,pos+1);

						// if necessary, cast to required target type (e.g. adding or removing const)
						if (*call->getType() != *res->getType()) {
							res = core::lang::buildRefCast(res,call->getType());

							// if we are unpacking a captured reference, adapt the type
							if (core::lang::isReference(core::analysis::getReferencedType(res))) {
								auto& ext = mgr.getLangExtension<AllScaleBackendModule>();
								res = builder.callExpr(ext.getRefRefPlainToRefRefCpp(),res);
							}
						}

						// make sure the type of the replacement is the same as of the original
						assert_eq(*call->getType(),*res->getType());

						// done
						return res;
					}
				}

				// otherwise, do nothing
				return node;
			});

			// build the new lambda
			auto res = (recFunParam)
					? builder.lambdaExpr(funType,{param,newRecFunParam},newBody)
					: builder.lambdaExpr(funType,{param},newBody);

			// check the result
			assert_correct_ir(res);

			// done
			return res;
		}


		// -- Conversion of test/base/step functions to recursive function ---------------------------------------------


		core::LambdaExprPtr getSequentialImplementation(const lang::PrecFunction& function) {
			core::IRBuilder builder(function.getBaseCaseTest()->getNodeManager());

			// -- build up the sequential implementation of this function --

			// get the in-parameter
			auto in = builder.variable(function.getParameterType());
			auto inVal = builder.deref(in);

			// get the type of the resulting function (same as the base case type)
			auto funType = builder.functionType({ in->getType() }, function.getResultType());

			// create the recursive function reference
			auto recFun = builder.lambdaReference(funType,"rec");

			// get instantiated step implementation
			auto stepFun = inlineStep(function.getStepCases().back(),recFun,true);

			// create the body of the lambda
			auto body = builder.compoundStmt(
				builder.ifStmt(
					// check the base case test
					builder.callExpr(function.getBaseCaseTest(), inVal),
					// if in the base case => run base case
					builder.returnStmt(builder.callExpr(function.getBaseCases()[0],inVal)),
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
			auto res = builder.lambdaExpr(recFun,lambdaDef);

			// check for errors
			assert_correct_ir(res);

			// done
			return res;
		}

		core::LambdaExprPtr getParallelImplementation(const string& wi_name, const lang::PrecFunction& function) {
			auto& mgr = function.getBaseCaseTest()->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			auto& ext2 = mgr.getLangExtension<AllScaleBackendModule>();

			// -- build up the sequential implementation of this function --

			// get the in-parameter
			auto in = builder.variable(function.getParameterType());

			// get the type of the resulting function
			auto funType = builder.functionType(in->getType(), function.getTreetureType().toIRType());

			// create the recursive function reference
			auto recFun =
					builder.callExpr(
						ext2.getRecSpawnWorkItem(),
						lang::buildNoDependencies(mgr),
						builder.callExpr(
							ext2.getCreateWorkItemDescriptionReference(),
							builder.getIdentifierLiteral(wi_name),
							builder.getTypeLiteral(function.getParameterType()),
							builder.getTypeLiteral(function.getResultType())
						)
					);

			// get instantiated step implementation
			auto stepFun = inlineStep(function.getStepCases()[0],recFun,false);

			// create the body of the lambda
			auto body = builder.compoundStmt(
				builder.ifStmt(
					// check the base case test
					builder.callExpr(function.getBaseCaseTest(), in),
					// if in the base case => run base case
					builder.returnStmt(builder.callExpr(ext.getTreetureDone(), builder.callExpr(function.getBaseCases()[0],in))),
					// else run step case
					builder.returnStmt(builder.callExpr(stepFun,in))
				)
			);

			// build the resulting lambda
			auto res = builder.lambdaExpr(funType,{in},body);

			// check for errors
			assert_correct_ir(res);

			// done
			return res;
		}


		WorkItemVariant getProcessVariant(const lang::PrecFunction& function) {

			// pick the base case implementation
			auto lambda = getSequentialImplementation(function);

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

			// create the resulting function type
			auto funType = builder.functionType(
				lambda->getFunctionType()->getParameterType(0),
				lang::TreetureType(lambda->getFunctionType()->getReturnType(),false).toIRType()
			);

			// use this lambda for creating the work item variant
			return WorkItemVariant(builder.lambdaExpr(funType, lambda->getParameterList(), body));
		}

		WorkItemVariant getSplitVariant(const std::string& wi_name, const lang::PrecFunction& function) {

			// pick the base case implementation
			auto lambda = getParallelImplementation(wi_name,function);

			// use this lambda for creating the work item variant
			return WorkItemVariant(lambda);
		}




		// -- Full Prec-to-WorkItem Conversion Procedure ---------------------------------------------------------------


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
			lang::PrecOperation original = lang::PrecOperation::fromIR(code.as<core::ExpressionPtr>());
			assert_eq(1,original.getFunctions().size())
				<< "Mutual recursive definitions not yet supported!";


			// -- Step 2: Collect closure type --

			// collect all captured values
			auto capturedValues = collectedCapturedValues(code);

			// print some debug information
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "Values captured in lambdas:\n";
				for(const auto& cur : capturedValues) {
					std::cout << "\t" << cur << "\n";
				}
				if (capturedValues.empty()) std::cout << "\t - none -\n";
			}


			// -- Step 3: Build the work-item closure tuple --

			// start list with recursive parameter
			core::TypeList elements;

			// add value version of parameter
			auto paramType = original.getParameterType();
			if (core::lang::isReference(paramType)) {
				paramType = core::analysis::getReferencedType(paramType);
			}
			elements.push_back(paramType);

			// add captured values
			for(const auto& cur : capturedValues) {
				auto type = cur.field->getType();

				// capture C++ references as plain references, thus as pointer
				if (core::lang::isReference(type)) {
					core::lang::ReferenceType refType(type);
					refType.setKind(core::lang::ReferenceType::Kind::Plain);
					type = refType.toType();
				}

				// add this element to the capture tuple
				elements.push_back(type);
			}

			// and pack types into a single tuple
			auto closureType = builder.tupleType(elements);
			auto parameterType = core::lang::buildRefType(closureType,true,false,core::lang::ReferenceType::Kind::CppReference);

			// print debug information
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "WorkItem closure type: " << *closureType << "\n";
			}


			// -- Step 4: Re-Build all C++ lambdas into functions accepting the work-item tuple

			// starting from this part, only a single function is supported so far
			assert_eq(1,original.getFunctions().size());

			auto toLambda = [&](const core::ExpressionPtr& expr) {
				return convertToLambdaAcceptingClosure(parameterType,capturedValues,expr);
			};

			// get the original function
			auto originalFunction = original.getFunction();

			// convert to a lambda-only function
			lang::PrecFunction function(
				toLambda(originalFunction.getBaseCaseTest()),
				::transform(originalFunction.getBaseCases(),toLambda),
				::transform(originalFunction.getStepCases(),toLambda)
			);

			// print debug information
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "Converted to Lambdas:\n" << dumpReadable(function.toIR(mgr));
			}


			// -- Step 5: Create process and split versions --

			// get a name for the work item
			const auto& name = converter.getNameManager().getName(code,"wi");

			auto process = getProcessVariant(function);
			auto split = getSplitVariant(name,function);


			// print debug information
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - process variant -\n" << dumpReadable(process.getImplementation()) << "\n";
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - split variant -\n" << dumpReadable(split.getImplementation()) << "\n";
			}


			// -- Step 6: work item spawn function --

			// wrap it up in a work item
			WorkItemDescription desc(name,process,split);

			// create a function wrapping the spawn call (need for bind)
			core::VariableList params;

			// add the dependency parameter
			auto& irExt = mgr.getLangExtension<lang::AllscaleModule>();
			auto depsParam = builder.variable(builder.refType(irExt.getDependenciesType(),true,false,core::lang::ReferenceType::Kind::CppReference));
			params.push_back(depsParam);

			// add the closure parameter
			auto closureParam = builder.variable(builder.refType(closureType,true,false,core::lang::ReferenceType::Kind::CppReference));
			params.push_back(closureParam);

			// assemble arguments for spawn call
			core::ExpressionList spawnArgs;
			spawnArgs.push_back(builder.deref(depsParam));
			spawnArgs.push_back(desc.toIR(mgr));

			spawnArgs.push_back(builder.refComponent(params[1],0));
			for(std::size_t i = 0; i<capturedValues.size(); i++) {
				spawnArgs.push_back(builder.refComponent(params[1],i+1));
			}

			// build function type
			auto funType = builder.functionType({ depsParam->getType(), closureParam->getType() }, function.getTreetureType().toIRType());

			// create the function spawning the work item
			auto spawnFunction = builder.lambdaExpr(
				funType,
				params,
				builder.compoundStmt(
					builder.returnStmt(
						builder.callExpr(ext.getSpawnWorkItem(), spawnArgs)
					)
				)
			);

			// check that everything is alright
			assert_correct_ir(spawnFunction);

			// print debug information
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << "Work-Item spawn function:\n" << dumpReadable(spawnFunction) << "\n";
			}


			// -- Step 7: create a bind capturing values and spawning the work item --

			// get the argument of the original function
			auto param = builder.variable(originalFunction.getParameterType());

			// collect the values to be captured for the closure
			core::ExpressionList closureValues;
			for(const CapturedValue& cur : capturedValues) {
				auto capture = cur.value;
				if (core::lang::isCppReference(cur.field->getType())) {
					capture = core::lang::buildRefKindCast(cur.value,core::lang::ReferenceType::Kind::Plain);
				}
				closureValues.push_back(capture);
			}

			// build a tuple type only covering the captured values
			core::TypeList closureTypes;
			for(std::size_t i=1; i<closureType->getElementTypes().size(); ++i) {
				closureTypes.push_back(closureType->getElement(i));
			}
			auto closureTuple = builder.tupleType(closureTypes);

			// build the initialization of the captured values
			core::ExpressionPtr closure = builder.initExprTemp(closureTuple,closureValues);

			// make it a const CppReference
			closure = core::lang::buildRefCast(closure,core::lang::ReferenceType::create(closureTuple,true,false,core::lang::ReferenceType::Kind::CppReference));

			// wrap up closure into a prec operator instance
			auto res = builder.callExpr(ext.getPrecFunCreate(),closure,spawnFunction);

			// check the result
			assert_correct_ir(res);

			// report final call
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - work item creation call -\n" << dumpReadable(res) << "\n";
				std::cout << "================ prec conversion :: done ==========================\n\n";
			}

			// done
			return res;
		}

	}



	core::NodePtr PrecConverter::process(const be::Converter& converter, const core::NodePtr& code) {

		// replace all prec calls with prec_operations and strip prec operator unwrapper
		auto& mgr = code->getNodeManager();
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();
		core::IRBuilder builder(mgr);
		auto res = core::transform::transformBottomUp(code, [&](const core::NodePtr& cur)->core::NodePtr {

			// strip unwrapper
			if (lang::isPrecFunUnwrapperCall(cur)) {
				auto call = cur.as<core::CallExprPtr>();
				auto precOp = call->getArgument(0);
				if (lang::isPrecFunToFunCall(cur)) {
					return builder.callExpr(ext.getPrecFunToFun(),precOp);
				} else if (lang::isPrecFunToDepFunCall(cur)) {
					return builder.callExpr(ext.getPrecFunToDepFun(),precOp);
				}
				assert_fail() << "Unsupported prec-fun wrapper encountered: " << *call->getFunctionExpr();
			}

			// for the rest: only interested in calls producing precfun<'a,'b>
			auto call = cur.isa<core::CallExprPtr>();
			if (!call) return cur;

			// check the type
			if (!lang::isPrecFun(call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getReturnType())) return cur;

			// first inline call
			auto res = core::transform::tryInlineToExpr(mgr,cur.as<core::CallExprPtr>());

			// test whether it is a prec operator call
			if (!lang::PrecOperation::isPrecOperation(res)) return res;

			// convert the prec operator call
			return convertPrecOperator(converter,res);

		}, core::transform::globalReplacement);

		// check that the result is properly typed
		assert_true(core::checks::check(res).empty())
			<< core::printer::dumpErrors(core::checks::check(res));

		// return result
		return res;

	}



	namespace {


		insieme::core::LambdaExprPtr convertToLambda(const insieme::core::CallExprPtr& call) {
			core::NodeManager& mgr = call->getNodeManager();
			core::IRBuilder builder(mgr);

			assert_pred2(core::analysis::isCallOf, call, mgr.getLangExtension<lang::AllscaleModule>().getCppLambdaToLambda());

			// get the call operator of the passed lambda
			auto callOperator = getCallOperatorImplementation(call->getArgument(0));

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

			// .. to the lambda_to_lambda
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


	insieme::core::NodePtr ClosureDefaultConstructorEnforcer::process(const insieme::backend::Converter&, const insieme::core::NodePtr& code) {

		core::NodeManager& mgr = code.getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();

		// -- Step 8: extra step: make all (nested) structs in the closure default-constructible

		// search all structs
		core::NodeMap recordSubstitutes;
		core::visitDepthFirstOnce(code,[&](const core::CallExprPtr& call){

			// only interested in call to PrecFunCreate operator
			if (!core::analysis::isCallOf(call,ext.getPrecFunCreate())) return;

			// collect all structs in the closure type
			core::visitDepthFirstOnce(call->getArgument(0)->getType(),[&](const core::RecordPtr& record){

				// check whether there is a field of a reference type
				bool hasReferenceField = any(record->getFields(),[](const core::FieldPtr& field) {
					return core::lang::isReference(field->getType());
				});

				// only interested if there is a reference field
				if (!hasReferenceField) return;

				// Step 1: re-write the default to force code generation (it is no longer defaulted)

				// search default constructor
				core::LambdaExprAddress defaultConstructor;
				for(const core::ExpressionAddress& cur : core::RecordAddress(record)->getConstructors()) {
					if (auto lambda = cur.isa<core::LambdaExprAddress>()) {
						if (lambda->getParameterList().size() == 1) {
							defaultConstructor = lambda;
						}
					}
				}

				// make sure it has been found
				assert_true(defaultConstructor)
					<< "Unable to locate pre-existing default constructor!";

				// replace body of this constructor
				auto newRecord = core::transform::replaceNode(mgr,defaultConstructor->getBody(),builder.compoundStmt(
						builder.stringLit("Forced auto-generated default constructor.")
				)).as<core::RecordPtr>();


				// Step 2: add a all-field forward constructor to support initializer expressions

				core::VariablePtr thisVar = defaultConstructor->getParameterList()[0];
				core::VariableList parameters;
				core::StatementList initStmts;

				// the this pointer is the first parameter
				parameters.push_back(thisVar);

				auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
				for(const core::FieldPtr& field : record->getFields()) {

					// get field type
					auto fieldType = field->getType();

					// get parameter
					auto parameter = builder.variable(core::transform::materialize(fieldType));

					// create the field access
					auto fieldAccess = builder.callExpr(
							refExt.getRefMemberAccess(),
							builder.deref(thisVar),
							builder.getIdentifierLiteral(field->getName()),
							builder.getTypeLiteral(field->getType())
					);

					// create the init expression
					auto initExpr = builder.initExpr(fieldAccess,parameter);

					// TODO: actually call constructor of copied elements if necessary
					//  - so fare everything is initialized through an init expression

					// add to lists
					parameters.push_back(parameter);
					initStmts.push_back(initExpr);

				}

				// build the new constructor
				auto returnType = defaultConstructor.getAddressedNode()->getFunctionType()->getReturnType();
				auto ctor = builder.lambdaExpr(returnType,parameters,builder.compoundStmt(initStmts),"_",core::FK_CONSTRUCTOR);

				// insert into resulting record
				core::ExpressionList constructors = newRecord->getConstructors()->getExpressions();
				constructors.push_back(ctor);
				newRecord = core::transform::replaceNode(mgr,
						core::RecordAddress(newRecord)->getConstructors(),
						builder.expressions(constructors)
				).as<core::RecordPtr>();

				// -- Done: register for replacement

				// add replacement to substitution
				recordSubstitutes[record] = newRecord;
			});

			// --- Parameter Type ---

			// also: if the recursive parameter type is a struct, make sure it has a default constructor
			auto funParamType = call->getArgument(1)->getType().as<core::FunctionTypePtr>()->getParameterType(1);

			// remove references
			if (core::lang::isReference(funParamType)) {
				funParamType = core::analysis::getReferencedType(funParamType);
			}

			// take the first parameter in the passed tuple
			funParamType = funParamType.as<core::TupleTypePtr>()->getElement(0);

			// if this is a tag type, it needs a default constructor
			if (auto tagType = funParamType.isa<core::TagTypePtr>()) {
				auto record = tagType->getRecord();

				// search default constructor
				core::ExpressionAddress defaultConstructor;
				for(const core::ExpressionAddress& cur : core::RecordAddress(record)->getConstructors()) {

					// check whether this is the default constructor
					if (cur->getType().as<core::FunctionTypePtr>()->getParameterTypes()->size() == 1) {

						// if it is a lambda, we are done
						if (cur.isa<core::LambdaExprPtr>()) return;

						// this is the one we need to replace
						defaultConstructor = cur;
					}
				}

				// make sure it has been found
				assert_true(defaultConstructor)
					<< "Unable to locate pre-existing default constructor!";

				// build new default constructor
				auto thisType = defaultConstructor.getAddressedNode()->getType().as<core::FunctionTypePtr>()->getParameterType(0);
				auto newDefaultCtor = builder.getDefaultConstructor(thisType,core::ParentsPtr(),record->getFields());

				// replace default constructor
				auto newRecord = core::transform::replaceNode(mgr,defaultConstructor,newDefaultCtor).as<core::RecordPtr>();

				// add to replacements
				recordSubstitutes[record] = newRecord;
			}

		},true);

		// apply replacements on substitutes
		for(auto& cur : recordSubstitutes) {
			cur.second = core::transform::replaceAllGen(mgr,cur.second,recordSubstitutes,core::transform::globalReplacement);
		}

		// apply replacement
		auto res = core::transform::replaceAllGen(mgr,code,recordSubstitutes,core::transform::globalReplacement);

		// check the result
		assert_correct_ir(res);

		// return result
		return res;
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
