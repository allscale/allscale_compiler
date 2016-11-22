
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

	void AllscaleExtension::TranslationStateManager::pushState(const TranslationState translationState) {
		translationStates.push_back(translationState);
	}

	AllscaleExtension::TranslationState AllscaleExtension::TranslationStateManager::getState() {
		assert_false(translationStates.empty()) << "No TranslationState to pop";
		return translationStates.back();
	}

	void AllscaleExtension::TranslationStateManager::popState() {
		assert_false(translationStates.empty()) << "No TranslationState to pop";
		translationStates.pop_back();
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

		core::ExpressionPtr handlePrecCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
			assert_eq(call->getNumArgs(), 1) << "handlePrecCall expects 1 argument only";
			return lang::buildPrec(toVector(converter.convertCxxArgExpr(call->getArg(0))));
		}

		core::ExpressionPtr handleCoreFunCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter,
		                                      AllscaleExtension::TranslationStateManager& translationStateManager) {

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

		clang::CallExpr* extractCallExpr(clang::Expr* node) {
			if(auto call = llvm::dyn_cast<clang::CallExpr>(node)) {
				return call;

			} else if(auto materialize = llvm::dyn_cast<clang::MaterializeTemporaryExpr>(node)) {
				return extractCallExpr(materialize->GetTemporaryExpr());

			} else if(auto construct = llvm::dyn_cast<clang::CXXConstructExpr>(node)) {
				return extractCallExpr(construct->getArg(0));
			}
			return nullptr;
		}
	}

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {

		if(auto call = llvm::dyn_cast<clang::CallExpr>(expr)) {
			auto decl = call->getCalleeDecl();
			if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
				auto name = funDecl->getQualifiedNameAsString();
				if(name == "allscale::api::core::prec") {
					return handlePrecCall(call, converter);
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

	insieme::frontend::stmtutils::StmtWrapper AllscaleExtension::Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter) {
		// for variable declarations which are inited with a call to prec, store the type of the prec call in our mapping to replace the variable type
		if(auto declStmt = llvm::dyn_cast<clang::DeclStmt>(stmt)) {
			for(auto decl : declStmt->decls()) {
				if(auto varDecl = llvm::dyn_cast<clang::VarDecl>(decl)) {
					if(auto init = varDecl->getInit()) {
						if(auto call = extractCallExpr(init)) {
							auto decl = call->getCalleeDecl();
							if(auto funDecl = llvm::dyn_cast_or_null<clang::FunctionDecl>(decl)) {
								auto name = funDecl->getQualifiedNameAsString();
								if(name == "allscale::api::core::prec") {
									auto precCall = handlePrecCall(call, converter);
									typeMappings[varDecl->getType()->getUnqualifiedDesugaredType()] = precCall->getType();
								}
							}
						}
					}
				}
			}
		}
		return {};
	}

	insieme::core::TypePtr AllscaleExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		if(typeMappings.find(type->getUnqualifiedDesugaredType()) != typeMappings.end()) {
			return typeMappings.at(type->getUnqualifiedDesugaredType());
		}
		return {};
	}

	insieme::core::TypePtr AllscaleExtension::PostVisit(const clang::QualType& typeIn, const insieme::core::TypePtr& irType,
	                                                    insieme::frontend::conversion::Converter& converter) {
		const clang::Type* type = typeIn->getUnqualifiedDesugaredType();
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			type = autoType->getDeducedType().getTypePtr();
		}
		if(auto tagType = llvm::dyn_cast<clang::TagType>(type)) {
			auto typeName = tagType->getDecl()->getQualifiedNameAsString();
			if(typeName == "allscale::api::core::detail::completed_task") {
				return (core::GenericTypePtr) lang::TreetureType(core::analysis::getReferencedType(irType.as<core::GenericTypePtr>()->getTypeParameter(0)), false);
			}
			if(boost::starts_with(typeName, "allscale::api::core::detail::callable")) {
				auto translationState = getTranslationStateManager().getState();
				return (core::GenericTypePtr) lang::RecFunType(translationState.first, translationState.second);
			}
		}

		return irType;
	}

	insieme::core::ProgramPtr AllscaleExtension::IRVisit(insieme::core::ProgramPtr& prog) {
		dumpReadable(prog);
		return prog;
	}
}
}
}

