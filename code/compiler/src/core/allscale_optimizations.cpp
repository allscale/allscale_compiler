
#include "allscale/compiler/core/allscale_optimizations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/backend/allscale_extension.h"


namespace allscale {
namespace compiler {
namespace core {

	namespace core = insieme::core;

	namespace {

		bool usesOnlyLambdaParameters(const core::NodePtr& codeFragment, const core::LambdaExprPtr& lambdaExpr) {
			// collect variables which are used within the given codeFragment
			core::VariableList varList;
			core::visitDepthFirstOncePrunable(codeFragment, [&](const core::ExpressionPtr& varExpr) {
				// don't descend into other lambda expressions
				if(varExpr.isa<core::LambdaExprPtr>()) return core::Action::Prune;

				if(const auto& var = varExpr.isa<core::VariablePtr>()) {
					varList.push_back(var);
				}
				return core::Action::Descent;
			}, false);

			// and check whether all if the collected variables are parameters of the given lambda expression
			core::VariableList lambdaParams = lambdaExpr->getParameterList()->getElements();
			for(const auto& var : varList) {
				if(std::find(lambdaParams.cbegin(), lambdaParams.cend(), var) == lambdaParams.end()) {
					return false;
				}
			}
			return true;
		}

	}

	core::NodePtr performDataItemGetLoopHoisting(const core::NodePtr& code, const ProgressCallback&) {
		auto& mgr = code->getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& beExt = mgr.getLangExtension<backend::AllScaleBackendModule>();

		auto res = core::transform::transformBottomUpGen(code, [&](const core::LambdaExprPtr& lambdaExpr){
			const auto& body = lambdaExpr->getBody();

			// skip defaulted members
			if(core::analysis::isaDefaultMember(lambdaExpr)) return lambdaExpr;

			// collect all occurrences of art_data_item_get in this lambda
			core::ExpressionList getCallNodes;
			core::visitDepthFirstOncePrunable(body, [&](const core::ExpressionPtr& expr) {
				// don't descend into other lambda expressions
				if(expr.isa<core::LambdaExprPtr>()) return core::Action::Prune;

				// and collect addresses of calls to getDataItem
				if(beExt.isCallOfGetDataItem(expr)) {
					// if we are only using parameters of the current lambda, we are allowed to hoist this call
					if(usesOnlyLambdaParameters(expr, lambdaExpr)) {
						getCallNodes.push_back(expr);
					}
				}
				return core::Action::Descent;
			}, false);

			// if we found calls
			if(!getCallNodes.empty()) {
				// we create replacement variables
				core::DeclarationStmtList declStmts;
				core::NodeMap replacements;
				// for each call
				for(const auto& callNode : getCallNodes) {
					// as the types of dataItemGet differ between IR and C++ we need to wrap them here (with unpack and pack) to get correctly typed IR
					auto declStmt = builder.declarationStmt(builder.callExpr(beExt.getGetDataItemUnpack(), callNode));
					declStmts.push_back(declStmt);
					replacements.insert({callNode, builder.callExpr(beExt.getGetDataItemPack(), declStmt->getVariable())});
				}
				auto newBody = core::transform::replaceAll(mgr, body, replacements, core::transform::localReplacement).as<core::CompoundStmtPtr>();

				// and prepend them to old body
				core::StatementList bodyStmts = newBody->getStatements();
				bodyStmts.insert(bodyStmts.begin(), declStmts.cbegin(), declStmts.cend());

				// and create the new lambda
				auto res = builder.lambdaExpr(lambdaExpr->getType(), lambdaExpr->getParameterList(), builder.compoundStmt(bodyStmts), lambdaExpr->getReference()->getName()->getValue());
				core::transform::utils::migrateAnnotations(lambdaExpr, res);
				return res;
			}

			return lambdaExpr;
		}, core::transform::globalReplacement);

		// check whether this step was correct
		assert_correct_ir(res);

		return res;
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
