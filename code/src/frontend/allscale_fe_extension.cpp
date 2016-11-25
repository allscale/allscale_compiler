
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <boost/algorithm/string.hpp>

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_replacer.h"
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

		core::TagTypePtr removeDuplicateCallOperators(const core::TagTypePtr& tagType) {
			return core::transform::transformBottomUpGen(tagType, [](const core::MemberFunctionsPtr& memFuns) {
				core::MemberFunctionList newMemFuns;
				bool alreadyPresent = false;
				for(const auto& memFun : memFuns) {
					if(memFun->getNameAsString() == insieme::utils::mangle("operator()")) {
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

		core::ExpressionPtr handlePrecCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter,
			                               TranslationStateManager& translationState) {
			assert_eq(call->getNumArgs(), 1) << "handlePrecCall expects 1 argument only";
			auto ret = lang::buildPrec(toVector(converter.convertCxxArgExpr(call->getArg(0))));
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

			{
				auto stepFunClang = call->getArg(2);
				auto stepIr = converter.convertExpr(stepFunClang);

				auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr)lang::RecFunType(paramType, returnType)));

				core::GenericTypePtr stepReturnType = lang::TreetureType(returnType, false);

				auto stepClosureType = builder.functionType(toVector<core::TypePtr>(paramType, callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);

				stepBind = lang::buildLambdaToClosure(stepIr, stepClosureType);

				auto genType = insieme::core::analysis::getReferencedType(stepIr->getType()).as<insieme::core::GenericTypePtr>();
				auto tagType = tMap.at(genType);
				auto newTagType = removeDuplicateCallOperators(tagType);
				converter.getIRTranslationUnit().replaceType(genType, newTagType);
			}

			translationStateManager.popState();

			return lang::buildBuildRecFun(cutoffBind, toVector(baseBind), toVector(stepBind));
		}

		core::ExpressionPtr handleCoreDoneCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
			return lang::buildTreetureDone(converter.convertCxxArgExpr(call->getArg(0)));
		}

		//clang::CallExpr* extractCallExpr(clang::Expr* node) {
		//	if(auto call = llvm::dyn_cast<clang::CallExpr>(node)) {
		//		return call;
		//	} else if(auto materialize = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(node)) {
		//		return extractCallExpr(materialize->GetTemporaryExpr());
		//	} else if(auto construct = llvm::dyn_cast<clang::CXXConstructExpr>(node)) {
		//		return extractCallExpr(construct->getArg(0));
		//	}
		//	return nullptr;
		//}
	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {

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
		auto& typeMappings = getTranslationStateManager().getTypeMappings();
		if(typeMappings.find(type) != typeMappings.end()) {
			return typeMappings.at(type);
		}
		return {};
	}

	insieme::core::ExpressionPtr AllscaleExtension::PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
		                                                      insieme::frontend::conversion::Converter& converter) {
		core::IRBuilder builder(irExpr->getNodeManager());
		if(auto call = irExpr.isa<core::CallExprPtr>()) {
			// replace call to operator() on prec return value (closure) with simple call
			auto funExpr = call->getFunctionExpr();
			auto funTy = funExpr->getType().as<core::FunctionTypePtr>();
			auto funParms = funTy->getParameterTypeList();
			if(funTy->isMemberFunction()) {
				auto thisType = core::analysis::getReferencedType(funParms[0]);
				if(auto calleeFunType = thisType.isa<core::FunctionTypePtr>()) {
					core::IRBuilder builder(irExpr->getNodeManager());
					return builder.callExpr(builder.deref(core::analysis::getArgument(call, 0)), core::analysis::getArgument(call, 1));
				}
			}

			if(auto lit = funExpr.isa<core::LiteralPtr>()) {
				auto name = lit->getStringValue();
				if(name == "treeture::IMP_get") {
					return lang::buildTreetureGet(builder.deref(core::analysis::getArgument(call, 0)));
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
		if(typeName == "allscale::api::core::detail::completed_task" || typeName == "allscale::api::core::impl::reference::treeture") {
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
		auto& refExt = var->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();

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
				auto irInit = varInit;
				// remove unnecessary refCasts
				if(refExt.isCallOfRefCast(irInit)) {
					irInit = core::analysis::getArgument(irInit, 0);
				}
				return {var, irInit};
			}
		}
		return {var, varInit};
	}

	insieme::core::ProgramPtr AllscaleExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		dumpReadable(prog);
		return prog;
	}
}
}
}

