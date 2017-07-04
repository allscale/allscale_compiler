
#include "allscale/compiler/frontend/allscale_fe_extension_exprs.h"

#include <map>

#include "insieme/frontend/converter.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/list.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/utils/name_mangling.h"

#include "allscale/compiler/allscale_utils.h"
#include "allscale/compiler/frontend/allscale_fe_utils.h"
#include "allscale/compiler/lang/allscale_ir.h"


namespace allscale {
namespace compiler {
namespace frontend {
namespace detail {

	namespace fed = insieme::frontend::extensions::detail;

	/// Mapping specification from C++ to IR used during call expression translation
	const std::vector<fed::FilterMapper> exprMappings = {
		// callables
		{"allscale::api::core::fun_def.*fun_def", mapCopyAndMoveConstructor},                       // copy|move ctor call
		{"allscale::api::core::rec_defs.*rec_defs", mapCopyAndMoveConstructor},                     // copy|move ctor call
		{"allscale::api::core::detail::prec_operation.*prec_operation", mapCopyAndMoveConstructor}, // copy|move ctor call
		// completed_tasks
		{"allscale::api::core::done", 0, mapDoneCall},
		{"allscale::api::core::done", SimpleCallMapper("treeture_done", false, true)},
		{"allscale::api::core::.*::completed_task<.*>::operator treeture", SimpleCallMapper("treeture_run")},
		{"allscale::api::core::run", SimpleCallMapper("treeture_run")},
		{"allscale::api::core::.*::completed_task<.*>::operator unreleased_treeture", mapToFirstArgument}, // conversion operator
		{"allscale::api::core::.*::completed_task<.*>::completed_task", mapCopyAndMoveConstructor},        // copy|move ctor call
		// treeture
		{"allscale::api::core::impl::.*treeture.*::wait", SimpleCallMapper("treeture_wait", true)},
		{"allscale::api::core::impl::.*treeture.*::get", SimpleCallMapper("treeture_get", true)},
		{"allscale::api::core::impl::.*treeture.*::getLeft", SimpleCallMapper("treeture_left", true)},
		{"allscale::api::core::impl::.*treeture.*::getRight", SimpleCallMapper("treeture_right", true)},
		{"allscale::api::core::impl::.*treeture<void>::treeture", 0, mapToTreetureVoidCtor},    // default ctor call for void specialization - special mapping
		{"allscale::api::core::impl::.*treeture.*::.*treeture.*", mapCopyAndMoveConstructor},   // copy|move ctor call
		// task_reference
		{"allscale::api::core::impl::.*treeture.*::operator task_reference", SimpleCallMapper("treeture_to_task_ref", true)},
		{"allscale::api::core::impl::.*treeture.*::getTaskReference", SimpleCallMapper("treeture_to_task_ref", true)},
		{"allscale::api::core::impl::.*::task_reference::getLeft", SimpleCallMapper("task_ref_left", true)},
		{"allscale::api::core::impl::.*::task_reference::getRight", SimpleCallMapper("task_ref_right", true)},
		{"allscale::api::core::impl::.*::task_reference::wait", SimpleCallMapper("task_ref_wait", true)},
		{"allscale::api::core::impl::.*::task_reference::task_reference", 0, mapToTaskRefDone},        // default ctor call - special mapping
		{"allscale::api::core::impl::.*::task_reference::task_reference", mapCopyAndMoveConstructor},  // copy|move ctor call
		// treeture aggregation
		{"allscale::api::core::.*combine", AggregationCallMapper("treeture_combine", true)},
		{"allscale::api::core::.*sequential", AggregationCallMapper("treeture_sequential", true)},
		{"allscale::api::core::.*parallel", AggregationCallMapper("treeture_parallel", true)},
		// dependencies
		{"allscale::api::core::after", AfterCallMapper("dependency_after")},
		{"allscale::api::core::.*::dependencies<.*>::add", AggregationCallMapper("dependency_add")},
		{"allscale::api::core::no_dependencies::operator dependencies", mapToFirstArgument},        // conversion operator
		{"allscale::api::core::.*::dependencies<.*>::dependencies", mapCopyAndMoveConstructor},     // copy|move ctor call
		{"allscale::api::core::no_dependencies::no_dependencies", mapCopyAndMoveConstructor},       // copy|move ctor call
		// recfun operations
		{R"(allscale::api::core::.*prec_operation<.*>::operator\(\))", PrecFunCallMapper()},
		{R"(allscale::api::core::detail::callable<.*>::(Sequential|Parallel)Callable::operator\(\))", RecFunCallMapper()},
		// fun
		{"allscale::api::core::fun", mapToBuildRecFun},
		{"allscale::api::core::fun_def<.*>::fun_def", mapCopyAndMoveConstructor}, // copy|move ctor call
		// prec
		{"allscale::api::core::group", aggregateArgumentsToTuple},
		{"allscale::api::core::pick", aggregateArgumentsToList},
		{"allscale::api::core::prec", 1, mapPrecRecDefs},
		{"allscale::api::core::prec", 2, mapPrecFun},
		{"allscale::api::core::prec", 3, mapPrecDirect},
	};


	//////// implementation details --------------------------------------------------------------------------------------------------------------------

	namespace {
		core::ExpressionPtr removeUndesiredRefCasts(const core::ExpressionPtr& input) {
			auto& refExt = input->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefCast(input) || refExt.isCallOfRefKindCast(input)) {
				return core::analysis::getArgument(input, 0);
			}
			return input;
		}

		core::ExpressionPtr removeUndesiredDeref(const core::ExpressionPtr& input) {
			auto& refExt = input->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefDeref(input)) {
				return core::analysis::getArgument(input, 0);
			}
			return input;
		}

		core::ExpressionPtr derefOrDematerialize(const core::ExpressionPtr& argExprIn) {
			core::IRBuilder builder(argExprIn->getNodeManager());

			auto argExpr = removeUndesiredRefCasts(argExprIn);

			if(auto call = argExpr.isa<core::CallExprPtr>()) {
				// we don't dematerialize builtins
				if(!core::lang::isBuiltIn(call->getFunctionExpr())) {
					// if this call is a materializing call
					if(core::analysis::isMaterializingCall(call)) {
						// we dematerialize it by setting the type to the return type of the call's callee
						auto retType = call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getReturnType();
						return builder.callExpr(retType, call->getFunctionExpr(), call->getArgumentDeclarations());
					}
				}
			}
			auto exprType = argExpr->getType();
			if(core::analysis::isRefType(exprType)) {
				return builder.deref(argExpr);
			}
			return argExpr;
		}

		core::ExpressionPtr derefOrCopy(const core::ExpressionPtr& exprIn, insieme::frontend::conversion::Converter& converter) {
			core::IRBuilder builder(exprIn->getNodeManager());

			//get the inner type
			assert_true(core::lang::isReference(exprIn));
			auto innerType = core::analysis::getReferencedType(exprIn);

			// check whether it is a trivially copyable type. We need to look up the real TagType in the translation unit to do so
			auto& typeMap = converter.getIRTranslationUnit().getTypes();
			bool isTriviallyCopyable = true;
			if(const auto& genType = innerType.as<core::GenericTypePtr>()) {
				auto fullType = typeMap.find(genType);
				if(fullType != typeMap.end()) {
					isTriviallyCopyable = core::analysis::isTriviallyCopyable(fullType->second);
				}
			}

			// if the given expression is a plain reference and is trivially copyable, we need to deref it
			if(core::lang::isPlainReference(exprIn) && isTriviallyCopyable) return builder.deref(exprIn);

			// otherwise we need to cast it to const cpp_ref to encode copy construction
			return core::lang::buildRefCast(exprIn, core::lang::buildRefType(innerType, true, false, core::lang::ReferenceType::Kind::CppReference));
		}

		core::FunctionTypePtr extractLambdaOperationType(const clang::Expr* clangExpr, insieme::frontend::conversion::Converter& converter, bool deref) {
			if(auto mat = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(clangExpr)) clangExpr = mat->GetTemporaryExpr();
			if(auto lambda = llvm::dyn_cast<clang::LambdaExpr>(clangExpr)) {
				auto ret = converter.convertType(lambda->getCallOperator()->getType()).as<core::FunctionTypePtr>();
				if(deref) {
					auto dereffedParamTypes = ::transform(ret->getParameterTypeList(), [](const core::TypePtr& t) {
						return core::analysis::isRefType(t) ? core::transform::dematerialize(t) : t;
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

		const clang::Expr* skipStdMoveOnAllscaleTypes(const clang::Expr* clangExpr, insieme::frontend::conversion::Converter& converter) {
			if(auto clangCall = llvm::dyn_cast<clang::CallExpr>(clangExpr)) {
				if(auto namedDecl = llvm::dyn_cast_or_null<clang::NamedDecl>(clangCall->getCalleeDecl())) {
					if(namedDecl->getQualifiedNameAsString() == "std::move") {
						if(lang::isAllscaleType(converter.convertType(clangExpr->getType()))) {
							return clangCall->getArg(0);
						}
					}
				}
			}
			return clangExpr;
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

			// we need some special treatment for step cases with returns of non-treeture type
			return fixStepLambdaReturns(ret);
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



	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// Mappers

	// mapToFirstArgument
	core::ExpressionPtr mapToFirstArgument(const fed::ClangExpressionInfo& exprInfo) {
		if(auto thisArg = exprInfo.implicitObjectArgument) {
			return exprInfo.converter.convertExpr(thisArg);
		}
		assert_eq(exprInfo.numArgs, 1) << "Given sourceExpr " << dumpClang(exprInfo.sourceExpression, exprInfo.converter.getSourceManager())
				<< " has " << exprInfo.numArgs << " arguments";
		return exprInfo.converter.convertExpr(exprInfo.args[0]);
	}


	// mapCopyAndMoveConstructor
	core::ExpressionPtr mapCopyAndMoveConstructor(const fed::ClangExpressionInfo& exprInfo) {
		assert_eq(exprInfo.numArgs, 1) << "Given sourceExpr " << dumpClang(exprInfo.sourceExpression, exprInfo.converter.getSourceManager())
				<< " has " << exprInfo.numArgs << " arguments";
		return derefOrDematerialize(exprInfo.converter.convertExpr(exprInfo.args[0]));
	}


	// mapDoneCall
	core::ExpressionPtr mapDoneCall(const fed::ClangExpressionInfo& exprInfo) {
		return lang::buildTreetureDone(exprInfo.converter.getIRBuilder().getLangBasic().getUnitConstant());
	}


	// mapToTreetureVoidCtor
	core::ExpressionPtr mapToTreetureVoidCtor(const fed::ClangExpressionInfo& exprInfo) {
		return lang::buildTreetureRun(lang::buildTreetureDone(exprInfo.converter.getIRBuilder().getLangBasic().getUnitConstant()));
	}


	// SimpleCallMapper
	core::ExpressionPtr SimpleCallMapper::buildCallWithDefaultParamConversion(const core::ExpressionPtr& callee, const fed::ClangExpressionInfo& exprInfo) {
		core::ExpressionList args;
		auto& converter = exprInfo.converter;
		// if it was a member call, add the implicit this argument
		if(auto implicitArg = exprInfo.implicitObjectArgument) {
			auto thisArg = converter.convertExpr(implicitArg);
			if(derefThisArg) thisArg = derefOrDematerialize(thisArg);
			args.push_back(thisArg);
		}
		// add normal arguments
		for(const auto& arg : exprInfo.args) {
			auto convertedArg = convertArgument(arg, converter);
			if(derefOtherArgs) convertedArg = derefOrDematerialize(convertedArg);
			args.push_back(convertedArg);
		}
		return converter.getIRBuilder().callExpr(callee, postprocessArgumentList(args, converter));
	}

	core::ExpressionPtr SimpleCallMapper::convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) {
		return converter.convertExpr(skipStdMoveOnAllscaleTypes(clangArg, converter));
	}
	core::ExpressionList SimpleCallMapper::postprocessArgumentList(const core::ExpressionList& args, insieme::frontend::conversion::Converter& converter) {
		return args;
	}
	core::ExpressionPtr SimpleCallMapper::generateCallee(const fed::ClangExpressionInfo& exprInfo) {
		auto& allscaleExt = exprInfo.converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
		return exprInfo.converter.getIRBuilder().parseExpr(targetIRString, allscaleExt.getSymbols());
	}
	core::ExpressionPtr SimpleCallMapper::postprocessCall(const fed::ClangExpressionInfo& exprInfo, const core::ExpressionPtr& translatedCall) {
		return translatedCall;
	}

	core::ExpressionPtr SimpleCallMapper::operator()(const fed::ClangExpressionInfo& exprInfo) {
		auto callee = generateCallee(exprInfo);
		auto translatedCall = buildCallWithDefaultParamConversion(callee, exprInfo);
		return postprocessCall(exprInfo, translatedCall);
	}


	// TaskRefDoneCallMapper
	core::ExpressionPtr mapToTaskRefDone(const fed::ClangExpressionInfo& exprInfo) {
		auto& allS = exprInfo.converter.getNodeManager().getLangExtension<lang::AllscaleModule>();
		return exprInfo.converter.getIRBuilder().callExpr(allS.getTaskRefDone(), core::ExpressionList());
	}


	// AggregationCallMapper
	core::ExpressionPtr AggregationCallMapper::convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) {
		auto ret = SimpleCallMapper::convertArgument(clangArg, converter);
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


	// AfterCallMapper
	core::ExpressionPtr AfterCallMapper::convertArgument(const clang::Expr* clangArg, insieme::frontend::conversion::Converter& converter) {
		auto ret = AggregationCallMapper::convertArgument(clangArg, converter);
		// the arguments need to be task_ref objects. If they are not, we need to convert them
		auto retType = ret->getType();
		if(core::analysis::isRefType(retType)) retType = core::lang::ReferenceType(retType).getElementType();
		if(!lang::isTaskReference(retType)) {
			if(auto genType = retType.isa<core::GenericTypePtr>()) {
				assert_true(converter.getIRTranslationUnit().getTypes().find(genType) != converter.getIRTranslationUnit().getTypes().end()) << "Can't find type " << genType << " in irTU";
				// we have to lookup the record type from the irTU in order to look up the conversion opereators
				auto tagType = converter.getIRTranslationUnit().getTypes().at(genType);
				auto memFuns = tagType->getRecord()->getMemberFunctions();
				core::MemberFunctionPtr conversionOperator;
				for(const auto& memFun : memFuns) {
					auto name = memFun->getNameAsString();
					if(boost::starts_with(name, insieme::utils::getMangledOperatorConversionPrefix())
							&& lang::isTaskReference(memFun->getImplementation()->getType().as<core::FunctionTypePtr>()->getReturnType())) {
						conversionOperator = memFun;
						break;
					}
				}
				assert_true(conversionOperator) << "Could not find conversion operator to task_ref in type " << genType;

				// now that we found the correct conversion operator, we return a call to it
				ret = converter.getIRBuilder().callExpr(conversionOperator->getImplementation(), insieme::frontend::utils::prepareThisExpr(converter, ret));
			}
		}
		return ret;
	}


	// RecOrPrecFunCallMapper
	core::ExpressionPtr RecOrPrecFunCallMapper::generateCallee(const fed::ClangExpressionInfo& exprInfo) {
		assert_true(exprInfo.isOperatorCall);
		auto recfunArg = exprInfo.converter.convertExpr(exprInfo.args[0]);
		assert_true(recfunArg);
		// 2 arguments (this and parameters) means no dependencies
		if(exprInfo.numArgs == 2) {
			return buildWrapper(derefOrDematerialize(recfunArg));
		}
		// 3 arguments means we have dependencies
		else {
			return buildDepWrapper(derefOrDematerialize(recfunArg));
		}
	}
	core::ExpressionList RecOrPrecFunCallMapper::postprocessArgumentList(const core::ExpressionList& args,
	                                                                     insieme::frontend::conversion::Converter& converter) {
		assert_ge(args.size(), 1);
		core::ExpressionList newArgs(args.cbegin() + 1, args.cend());
		// if we are generating a call with dependencies, we need to deref the dependency argument
		if(newArgs.size() == 2) {
			newArgs.front() = derefOrDematerialize(newArgs.front());
		}
		// we need to correctly handle the argument passing here, as the C++ method always takes a const cpp_ref
		if(!newArgs.empty() && core::lang::isPlainReference(newArgs.back())) {
			newArgs.back() = derefOrCopy(newArgs.back(), converter);
		}
		return newArgs;
	}
	core::ExpressionPtr RecOrPrecFunCallMapper::postprocessCall(const fed::ClangExpressionInfo& exprInfo, const core::ExpressionPtr& translatedCall) {
		auto callType = exprInfo.converter.convertType(exprInfo.clangType);

		lang::TreetureType callTreeture(callType);
		lang::TreetureType translatedTreeture(translatedCall);

		// add call to treeture_run if the translated IR treeture isn't released, but the clang treeture is
		if(callTreeture.isReleased() && !translatedTreeture.isReleased()) {
			return lang::buildTreetureRun(translatedCall);
		}
		return translatedCall;
	}

	core::ExpressionPtr RecFunCallMapper::buildWrapper(const core::ExpressionPtr& expr) {
		return lang::buildRecfunToFun(expr);
	}
	core::ExpressionPtr RecFunCallMapper::buildDepWrapper(const core::ExpressionPtr& expr) {
		return lang::buildRecfunToDepFun(expr);
	}

	core::ExpressionPtr PrecFunCallMapper::buildWrapper(const core::ExpressionPtr& expr) {
		return lang::buildPrecfunToFun(expr);
	}
	core::ExpressionPtr PrecFunCallMapper::buildDepWrapper(const core::ExpressionPtr& expr) {
		return lang::buildPrecfunToDepFun(expr);
	}


	namespace {
		bool checkSameLambdaReturnType(const core::ExpressionList& exprs,
		                               const insieme::utils::map::PointerMap<core::GenericTypePtr, core::TagTypePtr>& tMap) {
			assert_false(exprs.empty());
			// get the return type from the call operator of the lambda referenced by the passed expression
			auto extractType = [&tMap](const core::ExpressionPtr& node) {
				auto genType = core::analysis::getReferencedType(node->getType()).as<core::GenericTypePtr>();
				assert_true(genType);
				assert_true(tMap.find(genType) != tMap.end());
				auto extractedNode = tMap.at(genType);
				return utils::extractCallOperatorType(extractedNode)->getReturnType();
			};

			// compare all types with the first one
			auto targetType = extractType(exprs.front());
			return all(exprs, [&targetType, &extractType](const auto& expr) { return extractType(expr) == targetType; });
		}

		core::ExpressionPtr doFunConstructionMapping(const clang::QualType clangType, const clang::SourceLocation locStart,
		                                             const clang::Expr* cutoffArg, const clang::Expr* baseCaseArg, const clang::Expr* stepCaseArg,
		                                             insieme::frontend::conversion::Converter& converter) {
			auto& builder = converter.getIRBuilder();
			auto& tMap = converter.getIRTranslationUnit().getTypes();

			// asserts if the structType (i.e. the lambda we are translating) created by the passed expression doesn't have a call operator -
			// i.e. the prec is never called
			auto checkForCallOperator = [&](const core::ExpressionPtr& expr) {
				auto genType = insieme::core::analysis::getReferencedType(expr->getType()).as<insieme::core::GenericTypePtr>();
				auto structType = tMap.at(genType)->getStruct();
				if(!utils::hasCallOperator(structType)) {
					assert_fail() << "Conversion of prec construct around lambda at \""
							<< insieme::frontend::utils::getLocationAsString(locStart, converter.getSourceManager(), false)
					<< "\" failed, because the result is never actually called.";
				}
			};

			auto funType = lang::RecFunType(converter.convertType(clangType));

			// handle cutoff
			core::ExpressionPtr cutoffBind = nullptr;
			// simply convert the lambda
			{
				// first we translate the lambda
				auto cutoffIr = removeUndesiredDeref(converter.convertExpr(cutoffArg));
				// we check for the presence of a call operator
				checkForCallOperator(cutoffIr);

				// finally we create the closure type as well as the CppLambdaToClosure call
				auto cutoffClosureType = builder.functionType(funType.getParamType(), builder.getLangBasic().getBool(), insieme::core::FK_CLOSURE);
				cutoffBind = lang::buildCppLambdaToClosure(cutoffIr, cutoffClosureType);
			}


			// handle base case(s)
			core::ExpressionList baseBinds;

			// simply convert the lambda
			auto convertForBaseCase = [&](const core::ExpressionPtr& baseIr) {
				// we check for the presence of a call operator
				checkForCallOperator(baseIr);

				// finally we create the closure type as well as the CppLambdaToClosure call
				auto baseClosureType = builder.functionType(funType.getParamType(), funType.getReturnType(), insieme::core::FK_CLOSURE);
				return lang::buildCppLambdaToClosure(baseIr, baseClosureType);
			};

			// first we translate the lambda(s)
			core::ExpressionList originalInputBaseCases;
			auto inputBaseCase = converter.convertExpr(baseCaseArg);
			// then handle lists and single lambdas accordingly
			if(core::lang::isList(inputBaseCase)) {
				for(const auto& expr : core::lang::parseListOfExpressions(inputBaseCase)) {
					auto arg = removeUndesiredDeref(expr);
					originalInputBaseCases.push_back(arg);
					baseBinds.push_back(convertForBaseCase(arg));
				}
			} else {
				auto arg = removeUndesiredDeref(inputBaseCase);
				originalInputBaseCases.push_back(arg);
				baseBinds.push_back(convertForBaseCase(arg));
			}
			// ensure all elements in the list have the same type
			if(!checkSameLambdaReturnType(originalInputBaseCases, tMap)) {
				assert_fail() << "Conversion of prec construct around lambda at \""
						<< insieme::frontend::utils::getLocationAsString(locStart, converter.getSourceManager(), false)
						<< "\" failed, because not all the base case implementations return the same type";
			}


			// handle step case(s)
			core::ExpressionList stepBinds;

			// here we have to do a bit more work. We convert the lambda and afterwards have to modify it a bit
			auto convertForStepCase = [&](const core::ExpressionPtr& stepIr) {
				// we check for the presence of a call operator
				checkForCallOperator(stepIr);

				// we create the closure type as well as the CppLambdaToClosure call
				auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr) funType));
				core::GenericTypePtr stepReturnType = lang::TreetureType(funType.getReturnType(), false);
				auto stepClosureType = builder.functionType(toVector<core::TypePtr>(funType.getParamType(), callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);
				auto ret = lang::buildCppLambdaToClosure(stepIr, stepClosureType);

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

				return ret;
			};

			// first we translate the lambda(s)
			core::ExpressionList originalInputStepCases;
			auto inputStepCase = converter.convertExpr(stepCaseArg);
			// then handle lists and single lambdas accordingly
			if(core::lang::isList(inputStepCase)) {
				for(const auto& expr : core::lang::parseListOfExpressions(inputStepCase)) {
					auto arg = removeUndesiredDeref(expr);
					originalInputStepCases.push_back(arg);
					stepBinds.push_back(convertForStepCase(arg));
				}
			} else {
				auto arg = removeUndesiredDeref(inputStepCase);
				originalInputStepCases.push_back(arg);
				stepBinds.push_back(convertForStepCase(arg));
			}
			// ensure all elements in the list have the same type
			if(!checkSameLambdaReturnType(originalInputStepCases, tMap)) {
				assert_fail() << "Conversion of prec construct around lambda at \""
						<< insieme::frontend::utils::getLocationAsString(locStart, converter.getSourceManager(), false)
						<< "\" failed, because not all the step case implementations return the same type";
			}

			// now that we have all three ingredients we can finally build the RecFun
			return lang::buildBuildRecFun(cutoffBind, baseBinds, stepBinds);
		}
	}

	// FunConstructionMapper
	core::ExpressionPtr mapToBuildRecFun(const fed::ClangExpressionInfo& exprInfo) {
		assert_eq(exprInfo.numArgs, 3) << "handleCoreFunCall expects 3 arguments";
		// the actual work is outlined in a function, as we need it in the PrecFunMapper also
		return doFunConstructionMapping(exprInfo.clangType, exprInfo.locStart, exprInfo.args[0], exprInfo.args[1], exprInfo.args[2], exprInfo.converter);
	}


	// TupleAggregationMapper
	core::ExpressionPtr aggregateArgumentsToTuple(const fed::ClangExpressionInfo& exprInfo) {
		core::ExpressionList elements;
		for(const auto& arg : exprInfo.args) {
			elements.push_back(derefOrDematerialize(exprInfo.converter.convertExpr(arg)));
		}
		return exprInfo.converter.getIRBuilder().tupleExpr(elements);
	}


	// ListAggregationMapper
	core::ExpressionPtr aggregateArgumentsToList(const fed::ClangExpressionInfo& exprInfo) {
		core::ExpressionList elements;
		for(const auto& arg : exprInfo.args) {
			elements.push_back(derefOrDematerialize(exprInfo.converter.convertExpr(arg)));
		}
		return core::lang::buildListOfExpressions(elements);
	}


	// PrecRecDefsMapper
	core::ExpressionPtr mapPrecRecDefs(const fed::ClangExpressionInfo& exprInfo) {
		assert_eq(exprInfo.numArgs, 1) << "prec call with rec_defs only supports 1 argument";
		return lang::buildPrec(derefOrDematerialize(exprInfo.converter.convertExpr(exprInfo.args[0])));
	}
	// PrecFunMapper
	core::ExpressionPtr mapPrecFun(const fed::ClangExpressionInfo& exprInfo) {
		assert_eq(exprInfo.numArgs, 1) << "we don't support mutual recursion";
		return lang::buildPrec(exprInfo.converter.getIRBuilder().tupleExpr(derefOrDematerialize(exprInfo.converter.convertExpr(exprInfo.args[0]))));
	}
	// PrecDirectMapper
	core::ExpressionPtr mapPrecDirect(const fed::ClangExpressionInfo& exprInfo) {
		assert_eq(exprInfo.numArgs, 3) << "direct prec call only supports 3 arguments";
		// here we 'emulate' a fun in between
		auto funRes = doFunConstructionMapping(exprInfo.clangType, exprInfo.locStart, exprInfo.args[0], exprInfo.args[1], exprInfo.args[2], exprInfo.converter);
		return lang::buildPrec(exprInfo.converter.getIRBuilder().tupleExpr(derefOrDematerialize(funRes)));
	}

} // end namespace detail
} // end namespace frontend
} // end namespace compiler
} // end namespace allscale
