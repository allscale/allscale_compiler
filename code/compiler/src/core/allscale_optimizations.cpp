
#include "allscale/compiler/core/allscale_optimizations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/backend/allscale_extension.h"


namespace allscale {
namespace compiler {
namespace core {

	namespace core = insieme::core;

	core::NodePtr performDataItemGetLoopHoisting(const core::NodePtr& code, const ProgressCallback&) {
		auto& mgr = code->getNodeManager();
		core::IRBuilder builder(mgr);
		const auto& beExt = mgr.getLangExtension<backend::AllScaleBackendModule>();

		auto res = core::transform::transformBottomUpGen(code, [&](const core::LambdaExprPtr& lambdaExpr){
			const auto& body = lambdaExpr->getBody();

			// collect all occurrences of art_data_item_get in this lambda
			core::ExpressionAddressList getCallAddresses;
			core::visitDepthFirstOncePrunable(core::CompoundStmtAddress(body), [&](const core::ExpressionAddress& expr){
				// don't descend into other lambda expressions
				if(expr.isa<core::LambdaExprAddress>()) return core::Action::Prune;

				// and collect addresses of calls to getDataItem
				if(beExt.isCallOfGetDataItem(expr.getAddressedNode())) {
					getCallAddresses.push_back(expr);
				}
				return core::Action::Descent;
			}, false);

			// if we found calls
			if(!getCallAddresses.empty()) {
				// we create replacement variables
				core::DeclarationStmtList declStmts;
				std::map<core::NodeAddress, core::NodePtr> replacements;
				// for each call
				for(const auto& addr : getCallAddresses) {
					// as the types of dataItemGet differ between IR and C++ we need to wrap them here (with unpack and pack) to get correctly typed IR
					auto declStmt = builder.declarationStmt(builder.callExpr(beExt.getGetDataItemUnpack(), addr.getAddressedNode()));
					declStmts.push_back(declStmt);
					replacements.insert({addr, builder.callExpr(beExt.getGetDataItemPack(), declStmt->getVariable())});
				}
				auto newBody = core::transform::replaceAll(mgr, replacements).as<core::CompoundStmtPtr>();

				// and prepend them to old body
				core::StatementList bodyStmts = newBody->getStatements();
				bodyStmts.insert(bodyStmts.begin(), declStmts.cbegin(), declStmts.cend());

				// and create the new lambda
				return builder.lambdaExpr(lambdaExpr->getType(), lambdaExpr->getParameterList(), builder.compoundStmt(bodyStmts), lambdaExpr->getReference()->getName()->getValue());
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
