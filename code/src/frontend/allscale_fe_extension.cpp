
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/core/analysis/ir_utils.h"

#include "allscale/compiler/lang/allscale_ir.h"

namespace allscale {
namespace compiler {
namespace frontend {

	namespace {

		core::FunctionTypePtr extractCallOperatorType(const core::StructPtr& sourceLambda) {
			auto mems = sourceLambda->getMemberFunctions();
			for(const auto& mem : mems) {
				if(mem->getNameAsString() == insieme::utils::mangle("operator()")) {
					return mem->getType().as<core::FunctionTypePtr>();
				}
			}

			assert_fail() << "Could not extract type from callable lambda:\n" << dumpPretty(sourceLambda);
			return {};
		}


		core::ExpressionPtr handlePrecCall(const clang::CallExpr* call, insieme::frontend::conversion::Converter& converter) {

			return converter.convertExpr(call->getArg(0));

			assert_eq(call->getNumArgs(), 3) << "handlePrecCall expects 3 arguments";

			auto cutoffFunClang = call->getArg(0);
			auto baseFunClang = call->getArg(1);
			auto stepFunClang = call->getArg(2);

			std::cout << "\n CUTOFF ====================\n ";
			cutoffFunClang->dumpColor();
			std::cout << "\n BASEFUN ====================\n ";
			baseFunClang->dumpColor();
			std::cout << "\n STEP ====================\n ";
			stepFunClang->dumpColor();

			return nullptr;
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
				paramType = extractCallOperatorType(structType)->getParameterType(1);

				auto cutoffClosureType = builder.functionType(paramType, builder.getLangBasic().getBool(), insieme::core::FK_CLOSURE);

				cutoffBind = lang::buildLambdaToClosure(cutoffIr, cutoffClosureType);
			}

			std::cout << "Cutoff: " << dumpPretty(cutoffBind->getType()) << "\n";

			// Handle Base Case

			core::TypePtr returnType = nullptr;
			core::ExpressionPtr baseBind = nullptr;

			{
				auto baseFunClang = call->getArg(1);
				auto baseIr = converter.convertExpr(baseFunClang);

				auto genType = insieme::core::analysis::getReferencedType(baseIr->getType()).as<insieme::core::GenericTypePtr>();
				auto structType = tMap.at(genType)->getStruct();
				returnType = extractCallOperatorType(structType)->getReturnType();

				auto baseClosureType = builder.functionType(paramType, returnType, insieme::core::FK_CLOSURE);

				baseBind = lang::buildLambdaToClosure(baseIr, baseClosureType);
			}

			std::cout << "Base: " << dumpPretty(baseBind->getType()) << "\n";

			// Handle step case
			// ('a, (recfun<'a,'b>, 'c...)) => treeture<'b,f>

			core::ExpressionPtr stepBind = nullptr;

			{
				auto stepFunClang = call->getArg(2);
				auto stepIr = converter.convertExpr(stepFunClang);

				auto callableTupleType = builder.tupleType(toVector<core::TypePtr>((core::GenericTypePtr)lang::RecFun(paramType, returnType)));

				core::GenericTypePtr stepReturnType = lang::Treeture(returnType, false);

				auto stepClosureType = builder.functionType(toVector<core::TypePtr>(paramType, callableTupleType), stepReturnType, insieme::core::FK_CLOSURE);

				std::cout << "Step T: " << dumpPretty(stepClosureType) << "\n";

				//dumpColor(converter.getIRTranslationUnit().resolve(stepIr));

			}

			exit(0);
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
					return handleCoreFunCall(call, converter);
				}
			}
		}

		return nullptr;
	}

	core::TypePtr AllscaleExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

}
}
}

