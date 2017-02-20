
#include "allscale/compiler/frontend/allscale_fe_extension_exprs.h"

#include <regex>

#include "insieme/frontend/converter.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/name_mangling.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/allscale_utils.h"


namespace allscale {
namespace compiler {
namespace frontend {
namespace detail {

	/// Mapping specification from C++ to IR used during call expression translation
	const static std::map<std::string, CallMapper> callMap = {
		// completed_tasks
		{"allscale::api::core::done", SimpleCallMapper("treeture_done")},
		{"allscale::api::core::.*::completed_task<.*>::operator treeture", SimpleCallMapper("treeture_run")},
		{"allscale::api::core::.*::completed_task<.*>::operator unreleased_treeture", NoopCallMapper()},
		// treeture
		{"allscale::api::core::impl::.*treeture.*::wait", SimpleCallMapper("treeture_wait", true)},
		{"allscale::api::core::impl::.*treeture.*::get", SimpleCallMapper("treeture_get", true)},
		{"allscale::api::core::impl::.*treeture.*::getLeft", SimpleCallMapper("treeture_left", true)},
		{"allscale::api::core::impl::.*treeture.*::getRight", SimpleCallMapper("treeture_right", true)},
		{"allscale::api::core::impl::.*treeture.*::getTaskReference", NoopCallMapper()},
		// task_reference
		{"allscale::api::core::impl::.*reference.*::wait", SimpleCallMapper("treeture_wait", true)},
		{"allscale::api::core::impl::.*reference::getLeft", SimpleCallMapper("treeture_left", true)},
		{"allscale::api::core::impl::.*reference::getRight", SimpleCallMapper("treeture_right", true)},
		// treeture aggregation
		{"allscale::api::core::combine", AggregationCallMapper("treeture_combine", true)},
		{"allscale::api::core::sequential", AggregationCallMapper("treeture_sequential", true)},
		{"allscale::api::core::parallel", AggregationCallMapper("treeture_parallel", true)},
		// dependencies
		{"allscale::api::core::after", AggregationCallMapper("dependency_after")},
		{"allscale::api::core::dependencies::add", AggregationCallMapper("dependency_add")},
		// recfun operations
		{R"(allscale::api::core::.*prec_operation<.*>::operator\(\))", RecFunCallMapper()},
		{R"(allscale::api::core::detail::callable<.*>::(Sequential|Parallel)Callable::operator\(\))", RecFunCallMapper()},
		// fun
		{"allscale::api::core::fun", FunConstructionMapper()},
		// prec
		{"allscale::api::core::prec", PrecMapper()},
	};

	core::ExpressionPtr mapExpr(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		// we handle certain calls specially, which we differentiate by their callee's name
		if(auto call = llvm::dyn_cast<clang::CallExpr>(expr)) {
			auto decl = call->getCalleeDecl();
			if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
				auto name = funDecl->getQualifiedNameAsString();
				std::cout << "N: " << name << std::endl;

				for(const auto& mapping : callMap) {
					std::regex pattern(mapping.first);
					if(std::regex_match(name, pattern)) {
						std::cout << "Matched " << mapping.first << std::endl;
						return mapping.second(call, converter);
					}
				}
			}
		}

		return nullptr;
	}


	//////// implementation details --------------------------------------------------------------------------------------------------------------------

	namespace {
		core::ExpressionPtr removeUndesiredRefCasts(const core::ExpressionPtr& input) {
			auto& refExt = input->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefCast(input) || refExt.isCallOfRefKindCast(input)) {
				return core::analysis::getArgument(input, 0);
			}
			return input;
		}

		core::ExpressionPtr derefOrDematerialize(const core::ExpressionPtr& argExprIn) {
			core::IRBuilder builder(argExprIn->getNodeManager());

			auto argExpr = removeUndesiredRefCasts(argExprIn);

			if(auto call = argExpr.isa<core::CallExprPtr>()) {
				if(core::lang::isPlainReference(call->getType())) {
					auto rawCallType = core::analysis::getReferencedType(call->getType());
					return builder.callExpr(rawCallType, call->getFunctionExpr(), call->getArgumentDeclarations());
				}
			}
			auto exprType = argExpr->getType();
			if(core::analysis::isRefType(exprType)) {
				return builder.deref(argExpr);
			}
			return argExpr;
		}

		core::FunctionTypePtr extractLambdaOperationType(const clang::Expr* clangExpr, insieme::frontend::conversion::Converter& converter, bool deref) {
			if(auto mat = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(clangExpr)) clangExpr = mat->GetTemporaryExpr();
			if(auto lambda = llvm::dyn_cast<clang::LambdaExpr>(clangExpr)) {
				auto ret = converter.convertType(lambda->getCallOperator()->getType()).as<core::FunctionTypePtr>();
				if(deref) {
					auto dereffedParamTypes = ::transform(ret->getParameterTypeList(), [](const core::TypePtr& t) {
						return core::analysis::isRefType(t) ? core::analysis::getReferencedType(t) : t;
					});
					ret = converter.getIRBuilder().functionType(dereffedParamTypes, ret->getReturnType(), ret->getKind());
				}
				return ret;
			}
			return {};
		}

		core::ExpressionPtr buildDependencyList(insieme::frontend::conversion::Converter& converter) {
			auto& allscaleExt = converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
			return converter.getIRBuilder().callExpr(allscaleExt.getDependencyAfter());
		}

		/**
		 * Removes all duplicate call operators in the passed type
		 */
		core::TagTypePtr removeDuplicateCallOperators(const core::TagTypePtr& tagType) {
			return core::transform::transformBottomUpGen(tagType, [](const core::MemberFunctionsPtr& memFuns) {
				core::MemberFunctionList newMemFuns;
				bool alreadyPresent = false;
				for(const auto& memFun : memFuns->getMembers()) {
					if(memFun->getNameAsString() == insieme::utils::getMangledOperatorCallName()) {
						if(alreadyPresent) {
							continue;
						}
						alreadyPresent = true;
					}
					newMemFuns.push_back(memFun);
				}
				return core::IRBuilder(memFuns->getNodeManager()).memberFunctions(newMemFuns);
			});
		}

		/**
		 * Generates a new call operator as a replacement for the given old one.
		 * The new operator will have the correct function type and a matching body which uses the passed recFun argument(s) correctly
		 */
		core::LambdaExprPtr fixCallOperator(const core::LambdaExprPtr& oldOperator, const core::TupleTypePtr& recFunTupleType) {

			auto& mgr = oldOperator->getNodeManager();
			core::IRBuilder builder(mgr);
			auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();
			const auto& oldFunType = oldOperator->getFunctionType();
			core::TypeList funTypeParamTypes(oldFunType->getParameterTypeList());
			core::VariableList params(oldOperator->getParameterList()->getParameters());

			// drop generated params and create mappings
			core::VariableList removedParams;
			for(unsigned i = 0; i < funTypeParamTypes.size() - 2; ++i) {
				funTypeParamTypes.pop_back();
				removedParams.insert(removedParams.begin(), params.back());
				params.pop_back();
			}
			// add recfun tuple type and param
			funTypeParamTypes.push_back(recFunTupleType);
			auto recfunTupleParam = builder.variable(core::transform::materialize(recFunTupleType));
			params.push_back(recfunTupleParam);

			// transform body and replace accesses to the dropped params with the desired accesses to the passed recfun tuple
			auto body = core::transform::transformBottomUpGen(oldOperator->getBody(), [&](const core::CallExprPtr& call) {
				if(refExt.isCallOfRefDeref(call)) {
					auto inner = call->getArgument(0);
					// here we need to replace the call
					// the index of our tuple access relates to the index of the dropped variable in our removedParams list
					auto index = std::find(removedParams.begin(), removedParams.end(), inner) - removedParams.begin();
					if((unsigned) index < removedParams.size()) {
						return builder.accessComponent(builder.deref(recfunTupleParam), index);
					}
				}
				return call;
			});

			auto functionType = builder.functionType(funTypeParamTypes, oldFunType->getReturnType(), core::FK_MEMBER_FUNCTION);
			auto ret = builder.lambdaExpr(functionType, params, body, oldOperator->getReference()->getNameAsString());
			return ret;
		}

		/**
		 * Replaces the call operator operatorLit inside the given tagType with newOperator
		 */
		core::TagTypePtr replaceCallOperator(const core::TagTypePtr& tagType, const core::LiteralPtr& operatorLit, const core::LambdaExprPtr& newOperator) {
			return core::transform::transformBottomUpGen(tagType, [&](const core::MemberFunctionsPtr& memFuns) {
				core::IRBuilder builder(memFuns->getNodeManager());
				core::MemberFunctionList newMemFuns;
				for(const auto& memFun : memFuns->getMembers()) {
					core::ExpressionPtr impl = memFun->getImplementation();
					if(impl == operatorLit) {
						impl = builder.literal(newOperator->getType(), operatorLit->getValue());
					}
					newMemFuns.push_back(builder.memberFunction(memFun->isVirtual(), memFun->getNameAsString(), impl));
				}
				return builder.memberFunctions(newMemFuns);
			});
		}
	}


	// NoopCallMapper
	core::ExpressionPtr NoopCallMapper::operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
		if(auto memCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(call)) {
			auto thisArg = converter.convertExpr(memCall->getImplicitObjectArgument());
			return thisArg;
		}
		return converter.convertExpr(call->getArg(0));
	}


	// SimpleCallMapper
	core::ExpressionPtr SimpleCallMapper::buildCallWithDefaultParamConversion(const core::ExpressionPtr& callee, const clang::CallExpr* call,
	                                                                          insieme::frontend::conversion::Converter& converter) {
		core::ExpressionList args;
		// if it was a member call, add the implicit this argument
		if(auto memCall = llvm::dyn_cast<clang::CXXMemberCallExpr>(call)) {
			auto thisArg = converter.convertExpr(memCall->getImplicitObjectArgument());
			if(derefThisArg) thisArg = derefOrDematerialize(thisArg);
			args.push_back(thisArg);
		}
		// add normal arguments
		for(const auto& arg : call->arguments()) {
			args.push_back(convertArgument(arg, converter));
		}
		return converter.getIRBuilder().callExpr(callee, postprocessArgumentList(args, converter));
	}

	core::ExpressionPtr SimpleCallMapper::convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) {
		return converter.convertExpr(clangArg);
	}
	core::ExpressionList SimpleCallMapper::postprocessArgumentList(const core::ExpressionList& args, insieme::frontend::conversion::Converter& converter) {
		return args;
	}
	core::ExpressionPtr SimpleCallMapper::generateCallee(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
		auto& allscaleExt = converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
		return converter.getIRBuilder().parseExpr(targetIRString, allscaleExt.getSymbols());
	}
	core::ExpressionPtr SimpleCallMapper::postprocessCall(const clang::CallExpr* call, const core::ExpressionPtr& translatedCall,
	                                                      insieme::frontend::conversion::Converter& converter) {
		return translatedCall;
	}

	core::ExpressionPtr SimpleCallMapper::operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
		auto callee = generateCallee(call, converter);
		auto translatedCall = buildCallWithDefaultParamConversion(callee, call, converter);
		return postprocessCall(call, translatedCall, converter);
	}


	// AggregationCallMapper
	core::ExpressionPtr AggregationCallMapper::convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) {
		auto ret = converter.convertExpr(clangArg);
		if(auto clangCall = llvm::dyn_cast<clang::CallExpr>(clangArg)) {
			if(auto namedDecl = llvm::dyn_cast_or_null<clang::NamedDecl>(clangCall->getCalleeDecl())) {
				if(namedDecl->getQualifiedNameAsString() == "std::move") {
					ret = converter.convertExpr(clangCall->getArg(0));
				}
			}
		}
		ret = derefOrDematerialize(ret);
		if(auto lambdaType = extractLambdaOperationType(clangArg, converter, true)) {
			ret = lang::buildCppLambdaToLambda(ret, lambdaType);
		}
		return ret;
	}
	core::ExpressionList AggregationCallMapper::postprocessArgumentList(const core::ExpressionList& args,
	                                                                            insieme::frontend::conversion::Converter& converter) {
		if(requiresDependencies && (args.size() == 0 || !lang::isDependencies(args[0]))) {
			core::ExpressionList ret;
			ret.push_back(buildDependencyList(converter));
			std::copy(args.cbegin(), args.cend(), std::back_inserter(ret));
			return ret;
		}
		return args;
	}


	// RecFunCallMapper
	core::ExpressionPtr RecFunCallMapper::generateCallee(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
		auto opCall = llvm::dyn_cast<clang::CXXOperatorCallExpr>(call);
		assert_true(opCall);
		auto recfunArg = converter.convertExpr(opCall->getArg(0));
		assert_true(recfunArg);
		return lang::buildRecfunToFun(derefOrDematerialize(recfunArg));
	}
	core::ExpressionList RecFunCallMapper::postprocessArgumentList(const core::ExpressionList& args,
	                                                               insieme::frontend::conversion::Converter& converter) {
		assert_ge(args.size(), 1);
		return core::ExpressionList(args.cbegin() + 1, args.cend());
	}
	core::ExpressionPtr RecFunCallMapper::postprocessCall(const clang::CallExpr* call, const core::ExpressionPtr& translatedCall,
	                                                      insieme::frontend::conversion::Converter& converter) {
		auto callType = converter.convertType(call->getType());

		lang::TreetureType callTreeture(callType);
		lang::TreetureType translatedTreeture(translatedCall);

		// add call to treeture_run if the translated IR treeture isn't released, but the clang treeture is
		if(callTreeture.isReleased() && !translatedTreeture.isReleased()) {
			return lang::buildTreetureRun(translatedCall);
		}
		return translatedCall;
	}


	// FunConstructionMapper
	core::ExpressionPtr FunConstructionMapper::operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
		assert_eq(call->getNumArgs(), 3) << "handleCoreFunCall expects 3 arguments";

		auto& builder = converter.getIRBuilder();
		auto& tMap = converter.getIRTranslationUnit().getTypes();

		// asserts if the structType (i.e. the lambda we are translating) created by the passed expression doesn't have a call operator -
		// i.e. the prec is never called
		auto checkForCallOperator = [&](const core::ExpressionPtr& expr) {
			auto genType = insieme::core::analysis::getReferencedType(expr->getType()).as<insieme::core::GenericTypePtr>();
			auto structType = tMap.at(genType)->getStruct();
			if(!utils::hasCallOperator(structType)) {
				assert_fail() << "Conversion of prec construct around lambda at \""
						<< insieme::frontend::utils::getLocationAsString(call->getLocStart(), converter.getSourceManager(), false)
				<< "\" failed, because the result is never actually called.";
			}
		};

		auto funType = lang::RecFunType(converter.convertType(call->getType()));

		// handle cutoff
		core::ExpressionPtr cutoffBind = nullptr;
		// simply convert the lambda
		{
			// first we translate the lambda
			auto cutoffIr = converter.convertExpr(call->getArg(0));
			// we check for the presence of a call operator
			checkForCallOperator(cutoffIr);

			// finally we create the closure type as well as the CppLambdaToClosure call
			auto cutoffClosureType = builder.functionType(funType.getParamType(), builder.getLangBasic().getBool(), insieme::core::FK_CLOSURE);
			cutoffBind = lang::buildCppLambdaToClosure(cutoffIr, cutoffClosureType);
		}

		// handle base case
		core::ExpressionPtr baseBind = nullptr;
		// simply convert the lambda
		{
			// first we translate the lambda
			auto baseIr = converter.convertExpr(call->getArg(1));
			// we check for the presence of a call operator
			checkForCallOperator(baseIr);

			// finally we create the closure type as well as the CppLambdaToClosure call
			auto baseClosureType = builder.functionType(funType.getParamType(), funType.getReturnType(), insieme::core::FK_CLOSURE);
			baseBind = lang::buildCppLambdaToClosure(baseIr, baseClosureType);
		}

		// handle step case
		core::ExpressionPtr stepBind = nullptr;
		// here we have to do a bit more work. We convert the lambda and afterwards have to modify it a bit
		{
			// first we translate the lambda
			auto stepIr = converter.convertExpr(call->getArg(2));
			// we check for the presence of a call operator
			checkForCallOperator(stepIr);

			// we create the closure type as well as the CppLambdaToClosure call
			auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr) funType));
			core::GenericTypePtr stepReturnType = lang::TreetureType(funType.getReturnType(), false);
			auto stepClosureType = builder.functionType(toVector<core::TypePtr>(funType.getParamType(), callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);
			stepBind = lang::buildCppLambdaToClosure(stepIr, stepClosureType);

			// we extract the generated struct tag type
			auto genType = insieme::core::analysis::getReferencedType(stepIr->getType()).as<insieme::core::GenericTypePtr>();
			auto tagType = tMap.at(genType);

			// and remove the duplicate call operators
			auto newTagType = removeDuplicateCallOperators(tagType);

			// now we fix the call operator and replace it with a correctly translated one
			auto oldOperatorLit = utils::extractCallOperator(newTagType->getStruct())->getImplementation().as<core::LiteralPtr>();

			// only fix the operator if it doesn't have the correct tuple type already
			if(!oldOperatorLit->getType().as<core::FunctionTypePtr>()->getParameterType(2).isa<core::TupleTypePtr>()) {
				auto newOperator = fixCallOperator(converter.getIRTranslationUnit()[oldOperatorLit], callableTupleType);
				newTagType = replaceCallOperator(newTagType, oldOperatorLit, newOperator);

				// finally we communicate the changes to the IR-TU
				converter.getIRTranslationUnit().removeFunction(oldOperatorLit);
				converter.getIRTranslationUnit().addFunction(builder.literal(newOperator->getType(), oldOperatorLit->getValue()), newOperator);
			}
			converter.getIRTranslationUnit().replaceType(genType, newTagType);

			// finally, we check for the presence of a call operator (the fixed one) again to make sure our conversion didn't lose it
			checkForCallOperator(stepIr);
		}

		// now that we have all three ingredients we can finally build the RecFun
		return lang::buildBuildRecFun(cutoffBind, toVector(baseBind), toVector(stepBind));
	}


	// PrecMapper
	core::ExpressionPtr PrecMapper::operator()(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
		assert_eq(call->getNumArgs(), 1) << "prec call only supports 1 argument";
		return lang::buildPrec(toVector(derefOrDematerialize(converter.convertExpr(call->getArg(0)))));
	}

} // end namespace detail
} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
