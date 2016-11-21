
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/core/analysis/ir_utils.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/utils.h"

namespace allscale {
namespace compiler {
namespace frontend {

	namespace {

		core::ExpressionPtr handlePrecCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
			assert_eq(call->getNumArgs(), 1) << "handlePrecCall expects 1 argument only";
			return lang::buildPrec(toVector(converter.convertCxxArgExpr(call->getArg(0))));
		}

		core::ExpressionPtr handleCoreFunCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {

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

			// Handle step case

			core::ExpressionPtr stepBind = nullptr;

			{
				auto stepFunClang = call->getArg(2);
				auto stepIr = converter.convertExpr(stepFunClang);

				auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr)lang::RecFunType(paramType, returnType)));

				core::GenericTypePtr stepReturnType = lang::TreetureType(returnType, false);

				auto stepClosureType = builder.functionType(toVector<core::TypePtr>(paramType, callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);

				stepBind = lang::buildLambdaToClosure(stepIr, stepClosureType);
			}

			return lang::buildBuildRecFun(cutoffBind, toVector(baseBind), toVector(stepBind));
		}

		core::ExpressionPtr handleCoreDoneCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {
			return lang::buildTreetureDone(converter.convertCxxArgExpr(call->getArg(0)));
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
					return handleCoreFunCall(call, converter);
				}
				if(name == "allscale::api::core::done") {
					return handleCoreDoneCall(call, converter);
				}
			}
		}

		return nullptr;
	}

	insieme::core::TypePtr AllscaleExtension::PostVisit(const clang::QualType& typeIn, const insieme::core::TypePtr& irType,
	                                                    insieme::frontend::conversion::Converter& converter) {
		const clang::Type* type = typeIn.getTypePtr();
		if(auto autoType = llvm::dyn_cast<clang::AutoType>(type)) {
			type = autoType->getDeducedType().getTypePtr();
		}
		if(auto tagType = llvm::dyn_cast<clang::TagType>(type)) {
			auto typeName = tagType->getDecl()->getQualifiedNameAsString();
			if(typeName == "allscale::api::core::detail::completed_task") {
				return (core::GenericTypePtr) lang::TreetureType(core::analysis::getReferencedType(irType.as<core::GenericTypePtr>()->getTypeParameter(0)), false);
			}
		}

		return irType;
	}

}
}
}

