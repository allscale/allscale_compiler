#include "allscale/compiler/core/prec_to_work_item_conversion.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/lang/list.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/dump/json_dump.h"

#include "insieme/utils/name_mangling.h"

#include "allscale/compiler/analysis/diagnostics.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/backend/allscale_runtime_entities.h"
#include "allscale/compiler/allscale_utils.h"

#include "allscale/compiler/analysis/data_item_access.h"
#include "allscale/compiler/analysis/data_requirement.h"
#include "allscale/compiler/env_vars.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	namespace {

		using namespace allscale::compiler::backend;

		core::NodePtr serializeCode(const core::NodePtr& node) {
			core::NodeManager& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);

			// special case for lambdas (to cross scope limits of transform-bottom-up)
			if (auto lambda = node.isa<core::LambdaExprPtr>()) {
				return builder.lambdaExpr(
					serializeCode(lambda->getReference()).as<core::LambdaReferencePtr>(),
					serializeCode(lambda->getDefinition()).as<core::LambdaDefinitionPtr>()
				);
			}

			auto& basic = mgr.getLangBasic();
			auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
			auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();

			// a utility to remove treeture types
			auto removeTreetures = [](const core::NodePtr& node) {
				return core::transform::transformBottomUp(node, [](const core::NodePtr& node)->core::NodePtr {

					// check whether it is a treeture type
					if (node.isa<core::TypePtr>() && lang::isTreeture(node)) {
						return lang::TreetureType(node).getValueType();
					}

					// not of interest either
					return node;
				}, core::transform::localReplacement);
			};

			auto extractArg = [&](const core::ExpressionPtr& expr) {
				auto arg = removeTreetures(expr);
				if(refExt.isCallOfRefDeref(arg)) {
					return core::lang::toCppReference(core::analysis::getArgument(arg, 0));
				}
				return arg.as<core::ExpressionPtr>();
			};

			// get body, replace treeture operations and recFun calls
			auto treetureConnector = builder.parseExpr("lit(\"connect\":('a,'b)->unit)");
			assert_true(treetureConnector);
			return removeTreetures(core::transform::transformBottomUp(node, [&](const core::NodePtr& node)->core::NodePtr {

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
					auto arg0 = extractArg(call->getArgument(1));
					auto arg1 = extractArg(call->getArgument(2));
					return builder.callExpr(call->getArgument(3),arg0,arg1);
				}

				if (core::analysis::isCallOf(call,ext.getTreetureParallel()) || core::analysis::isCallOf(call,ext.getTreetureSequential())) {
					auto arg0 = extractArg(call->getArgument(1));
					auto arg1 = extractArg(call->getArgument(2));
					return builder.callExpr(treetureConnector,arg0,arg1);
				}

				// if it is a call to a lambda that returns a treeture, serialize this lambda too
				if (lang::isTreeture(call) && call->getFunctionExpr().isa<core::LambdaExprPtr>()) {

					// get the function
					auto newFun = serializeCode(call->getFunctionExpr()).as<core::ExpressionPtr>();

					// also create new argument list
					core::ExpressionList newArgs;
					for(const auto& cur : call->getArgumentList()) {
						newArgs.push_back(extractArg(cur));
					}

					// return the call to the cleaned code
					return builder.callExpr(newFun,newArgs);

				}

				// not of interest either
				return node;
			}, core::transform::localReplacement));
		}




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
			auto& ext2 = mgr.getLangExtension<backend::AllScaleBackendModule>();
			bool isDepFun = false;
			body = core::transform::transformBottomUpGen(body, [&](const core::NodePtr& node)->core::NodePtr {

				// In a first step, we replace calls to RecfunToFun and RecfunToDepFun with the passed callee
				if (ext.isCallOfRecfunToFun(node) || ext.isCallOfRecfunToDepFun(node)) {
					isDepFun = ext.isCallOfRecfunToDepFun(node);
					return recFun;
				}

				// and in the next step we have to fix the passed callee again.
				// replace arguments of call by unpacking the tuple, if necessary
				if (core::analysis::isCallOf(node,recFun)) {
					// NOTE: this is necessary since the runtime interface requests those parameters unpacked

					// check some expected properties and extract the (packed) argument
					auto call = node.as<core::CallExprPtr>();
					assert_eq(isDepFun ? 2 : 1, call->getNumArguments());
					auto arg = call->getArgument(isDepFun ? 1 : 0);

					// now we collect all the stuff needed to build the resulting call
					core::ExpressionPtr targetCallee;
					core::ExpressionList args;

					// if we should serialize
					if(serialize) {
						// we just take the passed callee as is
						targetCallee = recFun;

						// if we are not serializing
					} else {
						// the calle is actually the spawning of a new work item
						targetCallee = ext2.getSpawnWorkItem();
						// if the user specified dependencies
						if(isDepFun) {
							// we use those
							args.push_back(call->getArgument(0));

							// otherwise we don't have any dependencies here
						} else {
							args.push_back(lang::buildNoDependencies(mgr));
						}
						// as a second argument, we append the work item description we got as recfun
						args.push_back(recFun);
					}

					// now we extract the argument types and append the type of the actual argument to the type list for checking
					core::TypeList argTypes = core::extractTypes(args);
					argTypes.push_back(arg->getType());

					// check whether this call would properly typed
					if (serialize && core::types::deduceReturnType(targetCallee->getType().as<core::FunctionTypePtr>(), argTypes, false)) {
						// if this is the case, we can use the argument as is
						args.push_back(arg);

						// otherwise we have to unpack it
					} else {
						arg = core::lang::removeSurroundingRefCasts(arg);
						assert_true(arg.isa<core::InitExprPtr>());
						for(const auto& cur : arg.as<core::InitExprPtr>()->getInitExprList()) {
							auto arg = cur;
							if(core::lang::isCppReference(arg)) {
								core::lang::ReferenceType refType(arg);
								if (!refType.isConst()) {
									refType.setConst(true);
									arg = core::lang::buildRefCast(arg,refType.toType());
								}
							}

							args.push_back(arg);
						}
					}

					// now we can return the final call
					return builder.callExpr(targetCallee, args);
				}

				// everything else remains untouched
				return node;
			});


			// if serialization should be applied, do so
			if (serialize) {

				// serialize the body ...
				body = serializeCode(body).as<core::CompoundStmtPtr>();

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

		struct ClosureField {

			core::TagTypeReferencePtr lambdaTypeName;

			core::FieldPtr field;

			friend std::ostream& operator<<(std::ostream& out, const ClosureField& value) {
				return out << *value.lambdaTypeName << "::" << *value.field->getName() << " : " << *value.field->getType();
			}
		};

		struct CapturedValue {

			ClosureField field;

			core::ExpressionPtr value;
		};

		class CapturedValueIndex {

			// we index captured value by the init expression to avoid
			// capturing the same value more than once
			std::map<core::ExpressionPtr, std::vector<ClosureField>> values;

		public:

			void append(const CapturedValue& value) {
				values[value.value].push_back(value.field);
				// make sure all captured field with the same init value have the same type
				assert_true(all(values[value.value], [&](const auto& cur){
					return *values[value.value].front().field->getType() == *cur.field->getType();
				}));
			}

			std::size_t getIndex(const core::TagTypeReferencePtr& lambda, const core::ExpressionPtr& fieldIdentifier) const {
				assert_true(fieldIdentifier.isa<core::LiteralPtr>());

				auto name = fieldIdentifier.as<core::LiteralPtr>()->getValue();

				std::size_t i = 0;
				for(const auto& cur : values) {
					for(const auto& field : cur.second) {
						// check current entry
						if (field.lambdaTypeName == lambda && field.field->getName() == name) return i;
					}
					i++;
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

			CapturedValue operator[](std::size_t index) const {
				auto it = values.begin();
				for(std::size_t i=0; i<index; i++) {
					assert_true(it != values.end());
					++it;
				}
				return CapturedValue {
					it->second.front(),
					it->first
				};
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
					res.append(CapturedValue{{lambdaClass->getTag(), field}, init});
				}

				// done -- do not descent further
				return core::Prune;
			});

			// obtain result
			return res;
		}


		// -- Conversion of CppLambdas to Lambdas ----------------------------------------------------------------------

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

			// if not ...
			if (!lambda) {

				// ... it may be a c++ lambda, from which the call operator member function needs to be extracted
				if(asExt.isCallOfCppLambdaToClosure(lambdaExpr)) {

					// unpack the constructor of the lambda
					auto cppLambda = lambdaExpr.as<core::CallExprPtr>()->getArgument(0);

					// extract call operator implementation from the given lambda
					lambda = utils::getCallOperatorImplementation(cppLambda);

				// ... it may also be a lambda, which has been packed into a closure by using the to_closure operator.
				} else if(mgr.getLangBasic().isCallOfToClosure(lambdaExpr)) {
					// unpack the argument
					lambda = core::analysis::getArgument(lambdaExpr, 0).as<core::LambdaExprPtr>();
				}

				assert_true(lambda) << "Can't handle lambdaExpr " << dumpReadable(lambdaExpr);
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

						bool isDepFun = asExt.isCallOfRecfunToDepFun(trg);

						// pack the recursive argument
						core::DeclarationList closureDecls;
						auto arg = call->getArgument(isDepFun ? 1 : 0);
						arg = core::lang::removeSurroundingRefCasts(arg);
						closureDecls.push_back(utils::buildPassByValueDeclaration(arg));

						// add the captured values
						for(std::size_t i = 0; i<index.size(); ++i) {
							// get the value from the closure
							core::ExpressionPtr forward = builder.refComponent(param,i+1);
							closureDecls.push_back(utils::buildPassByValueDeclaration(forward));
						}

						// build recursive call
						core::ExpressionList args;
						if(isDepFun) {
							args.push_back(call->getArgument(0));
						}
						assert_true(core::lang::isConstCppReference(closureParamType));
						const auto& closureParamElementType = core::analysis::getReferencedType(closureParamType);
						args.push_back(core::lang::buildRefCast(builder.initExprTemp(closureParamElementType, closureDecls), closureParamType));

						auto param = builder.accessComponent(builder.deref(newRecFunParam), 0);
						auto callTarget = isDepFun ? lang::buildRecfunToDepFun(param) : lang::buildRecfunToFun(param);

						return builder.callExpr(callTarget, args);
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
								auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();
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
			auto inVal = in;

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
			auto& ext2 = mgr.getLangExtension<backend::AllScaleBackendModule>();

			// -- build up the sequential implementation of this function --

			// get the in-parameter
			auto in = builder.variable(function.getParameterType());

			// get the type of the resulting function
			auto funType = builder.functionType(in->getType(), function.getTreetureType().toIRType());

			// create the recursive function reference
			auto recFun = builder.callExpr(
					ext2.getCreateWorkItemDescriptionReference(),
					builder.getIdentifierLiteral(wi_name),
					builder.getTypeLiteral(function.getParameterType()),
					builder.getTypeLiteral(function.getResultType())
			);

			// get instantiated step implementation
			auto stepFun = inlineStep(function.getStepCases()[0],recFun,false);

			// create the body of the lambda
			auto body = builder.compoundStmt(
				// run step case
				builder.returnStmt(builder.callExpr(stepFun,in))
			);

			// build the resulting lambda
			auto res = builder.lambdaExpr(funType,{in},body);

			// check for errors
			assert_correct_ir(res);

			// done
			return res;
		}

		LambdaExprPtr getCanSplitFunction(const lang::PrecFunction& function) {
			auto baseCaseTest = function.getBaseCaseTest();
			auto& mgr = baseCaseTest.getNodeManager();
			auto& basic = mgr.getLangBasic();
			core::IRBuilder builder(mgr);

			// get closure parameter
			auto p = builder.variable(function.getParameterType());

			// build lambda
			return builder.lambdaExpr(basic.getBool(), { p },
					builder.returnStmt(
							builder.negateExpr(
									builder.callExpr(baseCaseTest, p)
							)
					)
			);
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

			core::TypePtr funReturnType = lambda->getFunctionType()->getReturnType();
			// we mark treeture<unit> return types specially as treeture<art_unused_type>, since these should be written out differently by the backend, as by the runtime's request.
			if(builder.getLangBasic().isUnit(funReturnType)) {
				const auto& backendExt = mgr.getLangExtension<backend::AllScaleBackendModule>();
				funReturnType = backendExt.getUnusedType();

				// the body is also different
				body = builder.compoundStmt(
						builder.callExpr(lambda,lambda->getParameterList()[0]),
						builder.returnStmt(builder.callExpr(ext.getTreetureDone(), builder.callExpr(backendExt.getMakeUnusedType())))
				);
			}

			// create the resulting function type
			auto funType = builder.functionType(lambda->getFunctionType()->getParameterType(0), lang::TreetureType(funReturnType, false).toIRType());

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

		core::ExpressionPtr convertPrecOperator(int index, const core::ExpressionPtr& code) {
			const bool debug = false;

			// -- Step 1: Preparation --

			// make sure the given code is a prec operator invocation
			assert_true(lang::PrecOperation::isPrecOperation(code));

			// make sure the input is correct
			assert_correct_ir(code);

			// print initial state of conversion
			if (debug) {
				std::cout << "\n================ prec conversion :: begin =========================\n";
				std::cout << " - Input -\n" << dumpReadable(code) << "\n";
			}

			// get build utilities
			core::NodeManager& mgr = code.getNodeManager();
			core::IRBuilder builder(mgr);
			auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();

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
				auto type = cur.second.front().field->getType();

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


			// -- Step 5: Create can_split test, process and split versions --

			// get a name for the work item
			std::string name = format("allscale_wi_%d",index);

			auto can_split = getCanSplitFunction(function);
			auto process = getProcessVariant(function);
			auto split = getSplitVariant(name,function);


			// print debug information
			if (debug) {
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - can_split test -\n" << dumpReadable(can_split) << "\n";
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - process variant -\n" << dumpReadable(process.getImplementation()) << "\n";
				std::cout << "-------------------------------------------------------------------\n";
				std::cout << " - split variant -\n" << dumpReadable(split.getImplementation()) << "\n";
			}


			// -- Step 6: work item spawn function --

			// wrap it up in a work item
			WorkItemDescription desc(name,can_split,process,split);

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

			spawnArgs.push_back(core::lang::buildRefKindCast(builder.refComponent(params[1],0), core::lang::ReferenceType::Kind::CppReference));
			for(std::size_t i = 0; i<capturedValues.size(); i++) {
				spawnArgs.push_back(core::lang::buildRefKindCast(builder.refComponent(params[1],i+1), core::lang::ReferenceType::Kind::CppReference));
			}

			// build function type
			auto funType = builder.functionType({ depsParam->getType(), closureParam->getType() }, function.getTreetureType().toIRType());

			// create the function spawning the work item
			auto spawnFunction = builder.lambdaExpr(
				funType,
				params,
				builder.compoundStmt(
					builder.returnStmt(
						builder.callExpr(ext.getSpawnFirstWorkItem(), spawnArgs)
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

			// collect the values to be captured for the closure
			core::DeclarationList closureDecls;
			for(const auto& cur : capturedValues) {
				auto capture = core::lang::removeSurroundingRefCasts(cur.first);
				if (core::lang::isCppReference(cur.second.front().field->getType())) {
					capture = core::lang::buildRefKindCast(capture,core::lang::ReferenceType::Kind::Plain);
					closureDecls.push_back(builder.declaration(core::transform::materialize(capture->getType()), capture));
				} else {
					closureDecls.push_back(utils::buildPassByValueDeclaration(capture));
				}
			}

			// build a tuple type only covering the captured values
			core::TypeList closureTypes;
			for(std::size_t i=1; i<closureType->getElementTypes().size(); ++i) {
				closureTypes.push_back(closureType->getElement(i));
			}
			auto closureTuple = builder.tupleType(closureTypes);

			// build the initialization of the captured values
			core::ExpressionPtr closure = builder.initExprTemp(closureTuple,closureDecls);

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

		namespace {


			core::ExpressionPtr removeDataItemGet(const core::ExpressionPtr& dataItemRef) {

				auto& mgr = dataItemRef->getNodeManager();
				const auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();

				return core::transform::transformBottomUpGen(dataItemRef,[&](const CallExprPtr& call)->core::ExpressionPtr {
					// filter out getDataItem calls
					if (ext.isCallOfGetDataItem(call)) {
						return call->getArgument(0);
					}
					return call;
				});

			}

			core::ExpressionPtr cleanSymbolicValue(const core::ExpressionPtr& value) {

				auto& mgr = value->getNodeManager();
				IRBuilder builder(mgr);
				const auto& ext = mgr.getLangExtension<core::lang::ReferenceExtension>();

				auto isSomeCast = [&](const ExpressionPtr& expr) {
					return ext.isCallOfRefCast(expr) || ext.isCallOfRefKindCast(expr) || ext.isCallOfRefKindCast(expr);
				};

				// some simple cleanup steps
				auto res = core::transform::transformBottomUpGen(value,[&](CallExprPtr call)->core::ExpressionPtr {

					// filter out ref decl calls
					if (ext.isCallOfRefDecl(call)) {
						// replace by ref temp and reference cast call
						return core::lang::buildRefTemp(call->getType());
					}

					// strip of temporary init expressions
					if (ext.isCallOfRefTempInit(call)) {
						auto arg = call->getArgument(0);
						if (ext.isCallOfRefDeref(arg)) {
							return arg.as<CallExprPtr>()->getArgument(0);
						}
					}

					// handle special case of nested casts and field accesses
					if (ext.isCallOfRefCast(call)) {
						auto access = call->getArgument(0);
						if (ext.isCallOfRefMemberAccess(access)) {
							auto inner = access.as<CallExprPtr>()->getArgument(0);
							auto field = access.as<CallExprPtr>()->getArgument(1).as<LiteralPtr>()->getValue();
							if (ext.isCallOfRefCast(inner)) {
								auto tuple = inner.as<CallExprPtr>()->getArgument(0);

								// skip all those casts
								return builder.refMember(tuple,field);
							}
						}
					}

					// re-process kind casts to get updated types
					if (ext.isCallOfRefKindCast(call)) {
						auto arg = call->getArgument(0);
						auto kind = call->getArgument(1);
						call = builder.callExpr(ext.getRefKindCast(),arg,kind);
					}

					// aggregate ref-casts
					if (isSomeCast(call)) {

						auto cur = call->getArgument(0);
						auto start = cur;
						while(isSomeCast(cur)) {
							cur = cur.as<CallExprPtr>()->getArgument(0);
						}

						// if they are not nested => leaf it
						if (cur == start) {
							return call;
						}

						// aggregate casts
						return core::lang::buildRefCast(cur,call->getType());
					}


					// skip creation of memory if immediately dereferenced
					if (ext.isCallOfRefDeref(call)) {
						auto arg = call->getArgument(0);

						// skip ref-temp-init calls
						if (ext.isCallOfRefTempInit(arg)) {
							return arg.as<CallExprPtr>()->getArgument(0);
						}

					}

					return call;
				});

				return res;
			}

			/**
			 * Instruments all work item accesses.
			 */
			void instrumentDataItemAccesses(reporting::ConversionReport& report, analysis::AnalysisContext& context, const CallExprAddress& precCall, backend::WorkItemVariant& variant, const std::string& variantId) {

				auto& mgr = precCall->getNodeManager();
				IRBuilder builder(mgr);

				const auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
				const auto& beExt = mgr.getLangExtension<backend::AllScaleBackendModule>();

				auto impl = variant.getImplementation();
				auto body = impl->getBody();

				// step 1: get all data item accesses
				auto optional_accesses = analysis::getDataItemAccesses(context,body);

				// check the result
				if (!optional_accesses || optional_accesses->isUniversal()) {
					// unable to obtain (accurate) list of accesses, skipping instrumentation
					report.addMessage(precCall, variantId, reporting::Issue(precCall, reporting::ErrorCode::UnableToInstrumentVariantForDataItemAccessCheck));
					return;
				}

				// extract real accesses
				auto accesses = std::move(*optional_accesses);

				// step 2: replace accesses by checked accesses
				for(const auto& cur : accesses) {

					// update the target
					auto trg = cur.switchRoot(body).as<ExpressionAddress>();

					// add instrumentation
					core::CallExprPtr call = trg.getAddressedNode().as<CallExprPtr>();

					// redirect the target to the addressed memory location
					trg = trg.as<CallExprAddress>()->getArgument(0);
					NodePtr newTrg;
					if (refExt.isCallOfRefDeref(call)) {
						newTrg = builder.callExpr(beExt.getDataItemCheckReadAccess(),trg.getAddressedNode());
					} else if (refExt.isCallOfRefAssign(call)) {
						newTrg = builder.callExpr(beExt.getDataItemCheckWriteAccess(),trg.getAddressedNode());
					} else {
						assert_fail() << "Unsupported case: neighter read nor write?";
					}

					// apply replacement
					body = core::transform::replaceNode(mgr,trg,newTrg).as<CompoundStmtPtr>();
				}

				// update implementation
				variant.setImplementation(core::transform::replaceNode(
						mgr, LambdaExprAddress(impl)->getBody(),body
					).as<LambdaExprPtr>());

				// check correctness
				assert_correct_ir(variant.getImplementation());

				// report successful instrumentation
				report.addMessage(precCall, variantId,
						reporting::Issue(
							precCall,
							reporting::ErrorCode::InstrumentedVariantForDataItemAccessCheck,
							format("Total number of accesses: %d", accesses.size())
						)
				);

			}

		}

		namespace {

			const std::vector<std::string> INVALID_FUNCTIONS = {
				"IMP_printf", "IMP_fprintf", "IMP_sprintf", "IMP_snprintf",
				"IMP_vprintf", "IMP_vfprintf", "IMP_vsprintf", "IMP_vsnprintf",

				"IMP_puts", "IMP_putchar",
			};

			const std::vector<std::string> VALID_GLOBALS = {
				"IMP_std_colon__colon_cerr",
			};

			bool checkForRefOrPtrCaptures(const CallExprAddress& precCall, const std::string& variantId, const backend::WorkItemVariant& variant, reporting::ConversionReport& report) {
				bool valid = true;

				visitDepthFirstOncePrunable(variant.getClosureType(), [&](const NodePtr& node) {
					if(node.isa<LambdaExprPtr>()) {
						return Action::Prune;
					}

					auto type = node.isa<TypePtr>();
					if(!type) {
						return Action::Continue;
					}

					bool invalid_capture_by_ref = core::lang::isReference(type)
					                            && !core::lang::isQualifiedReference(type)
					                            && !backend::isDataItemReference(insieme::core::analysis::getReferencedType(type)); // data items are captured by reference, this is ok

					if(!invalid_capture_by_ref && !core::lang::isPointer(type)) {
						return Action::Continue;
					}

					valid = false;

					std::string msg = "Variable capture by ";
					{
						std::string type_string;

						if(core::lang::isReference(type)) {
							msg += "reference to ";
							type_string = toString(*core::analysis::getReferencedType(type));
						} else {
							msg += "pointer pointing to ";
							type_string = toString(*core::lang::PointerType{type}.getElementType());
						}

						msg += type_string;
					}

					report.addMessage(precCall, variantId, reporting::Issue(precCall, reporting::ErrorCode::RefOrPtrFoundInCaptureList, msg));

					return Action::Continue;
				});

				return valid;
			}

			bool validForDistributedMemory(const CallExprAddress& precCall, const std::string& variantId, const backend::WorkItemVariant& variant, reporting::ConversionReport& report) {
				bool valid = true;

				auto target = variant.getImplementation().getBody();

				// check for invalid functions
				{
					std::vector<CallExprPtr> invalid_calls;
					visitDepthFirstOnce(target, [&](const CallExprPtr& call) {
						auto fun = call->getFunctionExpr();
						if(fun.isa<LiteralPtr>()) {
							if (::contains(INVALID_FUNCTIONS, fun.as<LiteralPtr>().getStringValue())) {
								valid = false;
								invalid_calls.push_back(call);
							}
						}
					});
					for(const auto& invalid_call : invalid_calls) {
						visitDepthFirstOnceInterruptible(precCall, [&](const CallExprAddress& node) {
							if(node.getAddressedNode() == invalid_call) {
								auto msg = "Use of blacklisted function " + insieme::utils::demangle(invalid_call->getFunctionExpr().as<LiteralPtr>()->getValue()->getValue());
								report.addMessage(precCall, variantId, reporting::Issue(node, reporting::ErrorCode::CallToInvalidFunctionForDistributedMemory, msg));
								return Action::Interrupt;
							}
							return Action::Continue;
						});
					}
				}

				// check for use of global variables
				{
					std::vector<LiteralPtr> uses_of_global;
					visitDepthFirstOnce(target, [&](const LiteralPtr& lit) {
						// ignore strings
						std::string value = lit.getValue().getValue();
						if(!value.empty() && value[0] == '"') {
							return;
						}

						if(::contains(VALID_GLOBALS, value)) {
							return;
						}

						if(!lit.getType().isa<FunctionTypePtr>() && core::lang::isReference(lit)) {
							valid = false;
							uses_of_global.push_back(lit);
						}
					});
					for(const auto& use : uses_of_global) {
						visitDepthFirstOnceInterruptible(precCall, [&](const LiteralAddress& node) {
							if(node.getAddressedNode() == use) {
								std::string msg = "Use of global " + use->getValue()->getValue();
								report.addMessage(precCall, variantId, reporting::Issue(node, reporting::ErrorCode::InvalidUseOfGlobalForDistributedMemory, msg));
								return Action::Interrupt;
							}
							return Action::Continue;
						});
					}
				}

				valid &= checkForRefOrPtrCaptures(precCall, variantId, variant, report);

				if(valid) {
					report.addMessage(precCall, variantId, reporting::Issue(precCall, reporting::ErrorCode::ValidForDistributedMemory));
				}

				return valid;
			}

		}

		ExpressionPtr integrateDataRequirements(const ConversionConfig& config, const ExpressionPtr& precFun, reporting::ConversionReport& report, const CallExprAddress& precCall, std::size_t precIndex) {
			static const bool debug = std::getenv(ALLSCALE_DEBUG_ANALYSIS);

			// this feature may be skipped for now
			if(std::getenv(ALLSCALE_SKIP_ANALYSIS)) {
				return precFun;
			}

			// get some transformation essentials
			auto& mgr = precFun->getNodeManager();
			core::IRBuilder builder(mgr);

			// locate outer-most work item description
			ExpressionPtr workItemDesc;
			visitDepthFirstOnceInterruptible(precFun,[&](const CallExprPtr& call) {
				if (backend::WorkItemDescription::isEncoding(call)) {
					workItemDesc = call;
					return Action::Interrupt;
				}
				return Action::Continue;
			});

			// make sure a work item description has been found
			assert_true(workItemDesc);

			// de-code work item description
			auto desc = backend::WorkItemDescription::fromIR(workItemDesc);
			int counter = 0;
			for(auto& variant : desc.getVariants()) {
				counter++;

				// produce an id for this variant
				auto variantId = format("%d_%s", counter,
					(counter == 1) ? "progress" :
					(counter == 2) ? "split"    :
								     "custom"
				);

				auto target = variant.getImplementation()->getBody();

				if(!validForDistributedMemory(precCall, variantId, variant, report) && !config.allowSharedMemoryOnly) {
					report.addMessage(precCall, variantId, reporting::Issue{precCall, reporting::ErrorCode::InvalidForDistributedMemory});
					continue;
				}

				// obtaining data requirements for the body of this variant
				analysis::AnalysisContext context;
				auto requirements = analysis::getDataRequirements(context,target);

				// integrate data requirement into variant
				if (requirements && !requirements->isUniverse()) {
					// create a requirement function
					auto param = variant.getImplementation().getParameterList()[0];

					ExpressionList reqs;
					for(const auto& cur : *requirements) {

						// get the data item reference
						auto ref = cleanSymbolicValue(removeDataItemGet(cur.getDataItem()));

						// get the range
						auto range = cleanSymbolicValue(cur.getRange().toIR(mgr));

						// add a new requirement
						reqs.push_back(backend::createDataItemRequirement(ref,range,cur.getMode()));
					}

					// get the list of requirements
					auto requirementTuple = builder.tupleExpr(reqs);

					// create the function type
					auto funType = builder.functionType({ param->getType() }, requirementTuple->getType());

					// create the body
					auto body = builder.compoundStmt(builder.returnStmt(requirementTuple));

					// build the lambda
					auto dataRequirementFun = builder.lambdaExpr(funType,{param},body);

					// add requirement function
					variant.setDataRequirements(dataRequirementFun);
				}

				// add summary to report
				if (!requirements) {
					// a time-out occurred
					report.addMessage(precCall, variantId, reporting::Issue::timeout(precCall));
				} else if (requirements->isUniverse()) {
					// if dependencies could not be narrowed down => report a warning
					report.addMessage(precCall, variantId, reporting::Issue(precCall, reporting::ErrorCode::UnobtainableDataRequirement));
				} else {
					// otherwise report a summary info
					auto issue = reporting::Issue(precCall, reporting::ErrorCode::ObtainedDataRequirement);
					auto issueDetail = std::make_shared<analysis::DataRequirements>(*requirements);
					issue.setDetail(issueDetail);
					report.addMessage(precCall, variantId, issue);
				}

				// run diagnosis
				auto issues = analysis::runDiagnostics(context, NodeAddress(target));
				report.addMessages(precCall, variantId, issues);

				// integrate data item access instrumentation
				if (config.checkDataItemAccesses) {
					instrumentDataItemAccesses(report, context, precCall, variant, variantId);
				}

				// print some debug information
				if (debug) {

					std::cout << "\n";
					std::cout << "------------------------------------------------------------------------------------\n";
					std::cout << "                        Prec #" << precIndex << " - variant " << variantId << "\n";
					std::cout << "------------------------------------------------------------------------------------\n";

					// print the analysed target code
					std::cout << dumpReadable(target) << "\n";

					// dump analysis statistics
					context.dumpStatistics();

					// dump prec operator code in json format
					core::dump::json::dumpIR(format("analysis_p%d_v%d_code.json",precIndex, counter),target);

					// dump analysis solution as json meta file
					context.dumpSolution(format("analysis_p%d_v%d_solution", precIndex, counter));
				}

			}

			// update work item description
			auto newWorkItemDesc = desc.toIR(mgr);

			// if nothing changed, skip this step
			if (newWorkItemDesc == workItemDesc) return precFun;

			// replace work item description
			return core::transform::replaceAllGen(mgr, precFun, workItemDesc, newWorkItemDesc, core::transform::globalReplacement);
		}

	} // end namespace


	/**
	 * Converts prec calls in the given input program to work item constructs.
	 */
	ConversionResult convertPrecToWorkItem(const ConversionConfig& config, const NodePtr& code, const ProgressCallback& callback) {

		// replace all prec calls with prec_operations and strip prec operator unwrapper
		auto& mgr = code->getNodeManager();
		const auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();
		core::IRBuilder builder(mgr);

		// 1) collect prec operators
		std::vector<NodePtr> precCalls;
		visitDepthFirstOnce(code,[&](const CallExprPtr& call){

			// check the type
			if (!lang::isPrecFun(call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getReturnType())) return;

			// first inline call
			auto cur = core::transform::tryInlineToExpr(mgr,call);

			// test whether it is a prec operator call
			if (lang::PrecOperation::isPrecOperation(cur)) {
				precCalls.push_back(call); 		// found one
			}

		},false,true);

		// check whether there is something to do
		reporting::ConversionReport report;
		if (precCalls.empty()) return ConversionResult { report, code };


		// provide some user info
		callback(ProgressUpdate(format("Start processing %d parallel regions ...",precCalls.size())));

		// tag prec calls with first address reaching them (and keep it over transformations)
		struct FirstAddressTag : public core::value_annotation::copy_on_migration {
			CallExprAddress addr;
			FirstAddressTag(const CallExprAddress& addr) : addr(addr) {}
			bool operator==(const FirstAddressTag& other) const {
				return addr == other.addr;
			}
		};

		visitDepthFirstOnce(NodeAddress(code),[&](const CallExprAddress& cur) {
			if (contains(precCalls,cur)) {
				cur->attachValue<FirstAddressTag>(cur);
			}
		});


		// 2) convert encountered prec operators bottom-up
		int index = 0;
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

			// provide progress reporting
			index++;
			callback(ProgressUpdate("Converting parallel region", index, precCalls.size()));

			// convert the prec operator call
			res = convertPrecOperator(index,res);

			// also add info to conversion report
			assert_true(call->hasAttachedValue<FirstAddressTag>());
			auto firstAddress = call->getAttachedValue<FirstAddressTag>().addr;
			report.addMessage(
					firstAddress,
					reporting::Issue(firstAddress, reporting::ErrorCode::ConvertParRegionToSharedMemoryParRuntimeCode)
			);

			// unless it is for shared memory only ..
			if (!config.sharedMemoryOnly) {
				// add data requirement dependencies
				res = integrateDataRequirements(config,res,report,firstAddress,index);
			}

			// done
			return res;

		}, core::transform::globalReplacement);

		// check that the result is properly typed
		assert_true(core::checks::check(res).empty())
			<< core::printer::dumpErrors(core::checks::check(res))
			<< core::checks::check(res);

		// return result
		return ConversionResult { std::move(report), res };

	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
