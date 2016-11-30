
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <boost/algorithm/string.hpp>

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/utils/name_mangling.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/utils.h"

namespace allscale {
namespace compiler {
namespace frontend {

	void TranslationStateManager::pushState(const TranslationState translationState) {
		translationStates.push_back(translationState);
	}
	void TranslationStateManager::popState() {
		assert_false(translationStates.empty()) << "No TranslationState to pop";
		translationStates.pop_back();
	}
	TranslationState TranslationStateManager::getState() {
		assert_false(translationStates.empty()) << "No TranslationState to pop";
		return translationStates.back();
	}
	ClangIrTypeMap& TranslationStateManager::getTypeMappings() {
		return typeMappings;
	}

	namespace {

		static const char* ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER = "__AllScale__Dependent_AutoType";


		core::ExpressionPtr removeUndesiredRefCasts(const core::ExpressionPtr& input) {
			auto& refExt = input->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefCast(input) || refExt.isCallOfRefKindCast(input)) {
				return core::analysis::getArgument(input, 0);
			}
			return input;
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

		core::ExpressionPtr handlePrecCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter,
			                               TranslationStateManager& translationState) {
			assert_eq(call->getNumArgs(), 1) << "handlePrecCall expects 1 argument only";
			auto ret = lang::buildPrec(toVector(converter.convertCxxArgExpr(call->getArg(0))));
			// register the resulting type in our type mappings for later lookpu
			translationState.getTypeMappings()[call->getCallReturnType()->getUnqualifiedDesugaredType()] = ret->getType();
			return ret;
		}

		core::ExpressionPtr handleCoreFunCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter,
		                                      TranslationStateManager& translationStateManager) {

			assert_eq(call->getNumArgs(), 3) << "handleCoreFunCall expects 3 arguments";

			auto& builder = converter.getIRBuilder();
			auto& tMap = converter.getIRTranslationUnit().getTypes();

			// Handle Cutoff

			core::TypePtr paramType = nullptr;
			core::ExpressionPtr cutoffBind = nullptr;
			// simply convert the lambda
			{
				auto cutoffFunClang = call->getArg(0);
				auto cutoffIr = converter.convertExpr(cutoffFunClang);

				auto genType = insieme::core::analysis::getReferencedType(cutoffIr->getType()).as<insieme::core::GenericTypePtr>();
				auto structType = tMap.at(genType)->getStruct();
				paramType = utils::extractCallOperatorType(structType)->getParameterType(1);

				auto cutoffClosureType = builder.functionType(paramType, builder.getLangBasic().getBool(), insieme::core::FK_CLOSURE);

				cutoffBind = lang::buildLambdaToClosure(cutoffIr, cutoffClosureType);
			}

			// Handle Base Case

			core::TypePtr returnType = nullptr;
			core::ExpressionPtr baseBind = nullptr;
			// simply convert the lambda
			{
				auto baseFunClang = call->getArg(1);
				auto baseIr = converter.convertExpr(baseFunClang);

				auto genType = insieme::core::analysis::getReferencedType(baseIr->getType()).as<insieme::core::GenericTypePtr>();
				auto structType = tMap.at(genType)->getStruct();
				returnType = utils::extractCallOperatorType(structType)->getReturnType();

				auto baseClosureType = builder.functionType(paramType, returnType, insieme::core::FK_CLOSURE);

				baseBind = lang::buildLambdaToClosure(baseIr, baseClosureType);
			}

			translationStateManager.pushState({paramType, returnType});

			// Handle step case

			core::ExpressionPtr stepBind = nullptr;
			// Here we have to do a bit more work. We convert the lambda and afterwards have to modify it a bit
			{
				auto stepFunClang = call->getArg(2);
				auto stepIr = converter.convertExpr(stepFunClang);

				auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr)lang::RecFunType(paramType, returnType)));

				core::GenericTypePtr stepReturnType = lang::TreetureType(returnType, false);

				auto stepClosureType = builder.functionType(toVector<core::TypePtr>(paramType, callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);

				stepBind = lang::buildLambdaToClosure(stepIr, stepClosureType);

				// first we extract the generated struct
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
			}

			translationStateManager.popState();

			return lang::buildBuildRecFun(cutoffBind, toVector(baseBind), toVector(stepBind));
		}

		core::ExpressionPtr handleCoreDoneCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
			return lang::buildTreetureDone(converter.convertCxxArgExpr(call->getArg(0)));
		}

	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		// we handle certain calls specially, which we differentiate by their callee's name
		if(auto call = llvm::dyn_cast<clang::CallExpr>(expr)) {
			auto decl = call->getCalleeDecl();
			if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
				auto name = funDecl->getQualifiedNameAsString();
				if(name == "allscale::api::core::prec") {
					return handlePrecCall(call, converter, getTranslationStateManager());
				}
				if(name == "allscale::api::core::fun") {
					return handleCoreFunCall(call, converter, getTranslationStateManager());
				}
				if(name == "allscale::api::core::done") {
					return handleCoreDoneCall(call, converter);
				}
			}
		}

		return nullptr;
	}

	insieme::core::ExpressionPtr AllscaleExtension::Visit(const clang::CastExpr* castExpr,
	                                                      insieme::core::ExpressionPtr& irExpr, insieme::core::TypePtr& irTargetType,
	                                                      insieme::frontend::conversion::Converter& converter) {
		auto& allscaleExt = irExpr->getNodeManager().getLangExtension<lang::AllscaleModule>();

		// treeture_get has different semantics
		if(castExpr->getCastKind() == clang::CK_LValueToRValue) {
			if(allscaleExt.isCallOfTreetureGet(irExpr)) {
				return irExpr;
			}
		}
		return nullptr;
	}

	insieme::frontend::stmtutils::StmtWrapper AllscaleExtension::Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter) {
		return {};
	}

	insieme::core::TypePtr AllscaleExtension::Visit(const clang::QualType& typeIn, insieme::frontend::conversion::Converter& converter) {
		const clang::Type* type = typeIn->getUnqualifiedDesugaredType();

		// Certain type-mappings may have already been determined. Here we do the lookup
		auto& typeMappings = getTranslationStateManager().getTypeMappings();
		if(typeMappings.find(type) != typeMappings.end()) {
			return typeMappings.at(type);
		}

		// if the passed type is an AutoType and is dependent, we can't really translate it correctly.
		// we create a dummy replacement type to move forward in the translation and assert this replacement doesn'T survive in the final IR
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			if(autoType->isDependentType()) { return converter.getIRBuilder().genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER); }
		}

		return {};
	}

	insieme::core::ExpressionPtr AllscaleExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
		                                                      insieme::frontend::conversion::Converter& converter) {
		auto& mgr = irExpr->getNodeManager();
		core::IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();
		auto& allscaleExt = mgr.getLangExtension<lang::AllscaleModule>();

		if(auto call = irExpr.isa<core::CallExprPtr>()) {
			auto funExpr = call->getFunctionExpr();

			// find prec type instantiation and fix it
			if(auto calleeCall = funExpr.isa<core::CallExprPtr>()) {
				auto innerFunExpr = calleeCall->getFunctionExpr();
				if(basic.isTypeInstantiation(innerFunExpr) && call->getNumArguments() >= 2) {
					auto thisArg = call->getArgument(0);
					if(allscaleExt.isCallOfPrec(thisArg)) {
						return builder.callExpr(thisArg, call->getArgument(1));
					}
				}
			}

			// replace calls to operator() on intercepted types
			{
				auto funTy = funExpr->getType().as<core::FunctionTypePtr>();
				auto funParms = funTy->getParameterTypeList();
				if(funTy->isMemberFunction()) {
					// on prec return value (closure) with simple call
					auto thisType = core::analysis::getReferencedType(funParms[0]);
					if(auto calleeFunType = thisType.isa<core::FunctionTypePtr>()) {
						core::IRBuilder builder(irExpr->getNodeManager());
						return builder.callExpr(builder.deref(core::analysis::getArgument(call, 0)), core::analysis::getArgument(call, 1));
					}
					// on call operator of recfun
					if(auto lit = funExpr.isa<core::LiteralPtr>()) {
						if(lit->getStringValue() == std::string("recfun::") + insieme::utils::getMangledOperatorCallName()) {
							const auto& originalThis = core::analysis::getArgument(call, 0);
							return builder.callExpr(lang::buildRecfunToFun(builder.deref(originalThis)), core::analysis::getArgument(call, 1));
						}
					}
				}
			}

			// for already translated functions, if they were semantically mapped, don't materialize
			if(core::analysis::isRefType(call->getType())) {
				if(auto referencedType = core::analysis::getReferencedType(call->getType())) {
					if(lang::isTreeture(referencedType) || lang::isRecFun(referencedType)) {
						return builder.callExpr(referencedType, funExpr, call->getArgumentDeclarations());
					}
				}
			}

			// unwrap type instantiation
			if(auto calleeCall = funExpr.isa<core::CallExprPtr>()) {
				auto innerFunExpr = calleeCall->getFunctionExpr();
				if(basic.isTypeInstantiation(innerFunExpr)) {
					funExpr = core::analysis::getArgument(calleeCall, 1);
				}
			}

			// certain other calls identified by the callee's name get treated differently
			if(auto lit = funExpr.isa<core::LiteralPtr>()) {
				auto name = lit->getStringValue();
				if(name == "treeture::IMP_get") {
					return lang::buildTreetureGet(builder.deref(core::analysis::getArgument(call, 0)));
				}
				if(name == insieme::utils::mangle("allscale::api::core::run")) {
					return lang::buildTreetureRun(removeUndesiredRefCasts(core::analysis::getArgument(call, 0)));
				}
			}
		}
		return irExpr;
	}

	insieme::core::TypePtr AllscaleExtension::PostVisit(const clang::QualType& typeIn, const insieme::core::TypePtr& irType,
	                                                    insieme::frontend::conversion::Converter& converter) {
		// extract relevant type
		const clang::Type* type = typeIn.getTypePtr();
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			if(autoType->isDependentType()) { return irType; }
			if(autoType->isSugared()) { type = autoType->desugar().getTypePtr(); }
			if(autoType->isDeduced()) { type = autoType->getDeducedType().getTypePtr(); }
		}
		if(auto injectedType = llvm::dyn_cast<clang::InjectedClassNameType>(type)) {
			type = injectedType->getInjectedSpecializationType().getTypePtr();
		}
		type = type->getUnqualifiedDesugaredType();

		// extract type name from clang type
		std::string typeName;
		if(auto tempSpecType = llvm::dyn_cast<clang::TemplateSpecializationType>(type)) {
			if(auto templateDecl = tempSpecType->getTemplateName().getAsTemplateDecl()) {
				typeName = templateDecl->getQualifiedNameAsString();
			}
		}
		if(auto tagType = llvm::dyn_cast<clang::TagType>(type)) {
			typeName = tagType->getDecl()->getQualifiedNameAsString();
		}

		// handle conversion
		if(typeName == "allscale::api::core::detail::completed_task" || typeName == "allscale::api::core::impl::reference::treeture"
				|| typeName == "allscale::api::core::impl::reference::unreleased_treeture"
				|| typeName == "allscale::api::core::impl::sequential::lazy_unreleased_treeture") {
			auto innerType = irType.as<core::GenericTypePtr>()->getTypeParameter(0);
			if(core::analysis::isRefType(innerType)) { innerType = core::analysis::getReferencedType(innerType); }
			return (core::GenericTypePtr) lang::TreetureType(innerType, false);
		}
		if(boost::starts_with(typeName, "allscale::api::core::detail::callable")) {
			auto translationState = getTranslationStateManager().getState();
			return (core::GenericTypePtr) lang::RecFunType(translationState.first, translationState.second);
		}

		return irType;
	}

	std::pair<core::VariablePtr, core::ExpressionPtr> AllscaleExtension::PostVisit(const clang::VarDecl* varDecl, const core::VariablePtr& var,
		                                                                           const core::ExpressionPtr& varInit,
		                                                                           insieme::frontend::conversion::Converter& converter) {
		core::IRBuilder builder(var->getNodeManager());

		// not all VarDecls also have an initialization
		if(varInit) {
			// Change variable type for calls to AllScale API functions
			auto initT = varInit->getType();
			if(core::analysis::isRefType(initT)) initT = core::analysis::getReferencedType(initT);

			// handle prec call result
			if(auto funT = initT.isa<core::FunctionTypePtr>()) {
				if(funT->getKind() == core::FK_CLOSURE) {
					auto retT = funT->getReturnType();
					if(lang::isTreeture(retT)) {
						auto varT = var->getType();
						assert_true(core::analysis::isRefType(varT));
						auto varRefT = core::lang::ReferenceType(varT);
						varRefT.setElementType(initT);
						auto ret = std::make_pair(builder.variable((core::GenericTypePtr)varRefT), varInit);
						return ret;
					}
				}
			}

			// handle treetures
			if(lang::isTreeture(initT)) {
				return {var, removeUndesiredRefCasts(varInit)};
			}
		}
		return {var, varInit};
	}

	insieme::core::ProgramPtr AllscaleExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		core::IRBuilder builder(prog->getNodeManager());

		// temporarily dump the generated IR in a readable format
		//dumpReadable(prog);

		// make sure that we don't have the dummy dependent type replacement type in the program anywhere anymore
		assert_eq(core::analysis::countInstances(prog, builder.genericType(ALLSCALE_DEPENDENT_TYPE_PLACEHOLDER), false), 0);

		return prog;
	}
}
}
}

