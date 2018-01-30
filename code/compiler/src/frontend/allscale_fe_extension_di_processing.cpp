
#include "allscale/compiler/frontend/allscale_fe_extension.h"

#include <boost/algorithm/string.hpp>
#include "insieme/annotations/c/include.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/frontend/clang.h"
#include "insieme/frontend/converter.h"
#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/utils/iterator_utils.h"

#include "allscale/compiler/config.h"
#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/frontend/allscale_fe_extension_exprs.h"
#include "allscale/compiler/core/data_item_annotation.h"

namespace iu = insieme::utils;

namespace allscale {
namespace compiler {
namespace frontend {

	namespace core = insieme::core;

	insieme::core::ExpressionPtr applyDataItemProcessing(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
	                                                     insieme::frontend::conversion::Converter& converter) {
		auto& mgr = irExpr->getNodeManager();
		core::IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();
		const auto& allscaleExt = mgr.getLangExtension<lang::AllscaleModule>();

		if(auto call = irExpr.isa<core::CallExprPtr>()) {
			auto callee = call->getFunctionExpr();
			auto calleeType = callee->getType().as<core::FunctionTypePtr>();

			// if this is an instantiate node, we need to extract the actual callee
			if(basic.isCallOfInstantiate(callee)) {
				callee = core::analysis::getArgument(callee, 1);
			}

			// we only deal with literals
			if(auto calleeLit = callee.isa<core::LiteralPtr>()) {
				// check every intercepted method call
				if(calleeType->isMember() && insieme::annotations::c::hasIncludeAttached(calleeLit)) {
					// this lambda extracts the last statement in the body of the given FunctionDecl
					auto extractLastChild = [](const clang::FunctionDecl* funcDecl) -> clang::Stmt* {
						if(funcDecl->hasBody()) {
							if(auto compound = llvm::dyn_cast<clang::CompoundStmt>(funcDecl->getBody())) {
								if(compound->size() > 0) {
									return *(compound->body().end() - 1);
								}
							}
						}
						return nullptr;
					};

					// find the MethodDecl
					const clang::CXXMethodDecl* calleeDecl = nullptr;
					if(auto ctorCall = llvm::dyn_cast<clang::CXXConstructExpr>(expr)) calleeDecl = ctorCall->getConstructor();
					if(auto methodCall = llvm::dyn_cast<clang::CallExpr>(expr)) calleeDecl = llvm::dyn_cast<clang::CXXMethodDecl>(methodCall->getCalleeDecl());
					if(!calleeDecl) return irExpr;

					auto funcDecl = llvm::dyn_cast<clang::FunctionDecl>(calleeDecl);
					// switch to the declaration containing the body (if there is one)
					funcDecl->hasBody(funcDecl); // yes, right, this one has the side effect of updating funcDecl!!

					// get the last child of the body - or stop if that isn't a ReturnStmt
					auto lastChild = llvm::dyn_cast_or_null<clang::ReturnStmt>(extractLastChild(funcDecl));
					if(!lastChild) return irExpr;

					// extract the part of the expression we are actually interested in
					const clang::Expr* returnExpr = lastChild->getRetValue();
					if(auto clangExprWithCleanups = llvm::dyn_cast<clang::ExprWithCleanups>(returnExpr)) {
						returnExpr = clangExprWithCleanups->getSubExpr();
					}
					if(auto clangCXXBindTempExpr = llvm::dyn_cast<clang::CXXBindTemporaryExpr>(returnExpr)) {
						returnExpr = clangCXXBindTempExpr->getSubExpr();
					}

					// get the call - stripping LValueToRValue cast
					bool deref = false;
					auto semaCall = llvm::dyn_cast<clang::CallExpr>(returnExpr);
					if(!semaCall) {
						if(auto clangCast = llvm::dyn_cast<clang::CastExpr>(returnExpr)) {
							if(clangCast->getCastKind() == clang::CK_LValueToRValue) {
								deref = true;
							}
							semaCall = llvm::dyn_cast<clang::CallExpr>(clangCast->getSubExpr());
						}
					}
					if(!semaCall) return irExpr;

					// if it is a call and the callee's name matches the ones we are looking for
					auto semaCalleeDecl = llvm::dyn_cast<clang::FunctionDecl>(semaCall->getCalleeDecl());
					if(auto namedSemaCalleeDecl = llvm::dyn_cast_or_null<clang::NamedDecl>(semaCalleeDecl)) {
						auto semaCalleeName = namedSemaCalleeDecl->getNameAsString();
						if(boost::contains(semaCalleeName, "data_item_element_access")) {
							// first, we tag the generic type as a data item
							allscale::compiler::core::markAsDataItem(insieme::core::analysis::getReferencedType(calleeType->getParameterType(0)));

							// we replace the resulting IR with a custom built lambda
							core::VariableList params;

							// register the parameters (including a this-parameter) in order for the translation to succeed
							converter.getVarMan()->pushScope(false);
							auto thisType = insieme::frontend::utils::getThisType(calleeDecl, core::analysis::getObjectType(calleeType));
							auto thisVar = builder.variable(builder.refType(thisType));
							params.push_back(thisVar);
							converter.getVarMan()->setThis(thisVar);
							for(auto param : funcDecl->parameters()) {
								auto irParam = converter.getDeclConverter()->convertVarDecl(param);
								params.push_back(irParam.first);
							}

							// now convert the call
							auto convertedClangCall = core::lang::removeSurroundingRefCasts(converter.convertExpr(semaCall).isa<core::CallExprPtr>()).as<core::CallExprPtr>();
							// the converted call will be a type_instantiation. We are interested in the arguments only
							auto semaCallArgs = convertedClangCall->getArgumentList();

							assert_eq(semaCallArgs.size(), 3) << dumpReadable(convertedClangCall);
							semaCallArgs[2] = builder.getTypeLiteral(semaCallArgs[2]->getType());

							converter.getVarMan()->popScope();

							// now we build a new call to the core IR literal
							auto returnCall = builder.callExpr(allscaleExt.getDataItemElementAccess(), semaCallArgs);
							auto returnValue = deref ? builder.deref(returnCall) : returnCall;
							auto returnType = calleeType->getReturnType();
							if(!core::analysis::isRefType(returnType)) {
								returnType = core::transform::materialize(returnType);
							}
							// and create a new lambda returning the result of this call
							auto body = builder.returnStmt(returnValue, returnType);
							auto newCallee = builder.lambdaExpr(calleeType, params, body, calleeLit->getStringValue());
							auto newCalleeInstantiation = builder.instantiate(newCallee,callee);
							return builder.callExpr(irExpr->getType(), newCalleeInstantiation, call->getArgumentList());
						}
					}
				}
			}
		}
		return irExpr;
	}

}
}
}

