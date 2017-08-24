#include "allscale/compiler/core/data_item_conversion.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/node_replacer.h"

#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/core/data_item_annotation.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	NodePtr convertDataItemReferences(const NodePtr& code, const ProgressCallback&) {

		// To be transformed:
		//  - every ref<data_item,_,_,_> is transforemd to a art_data_item_ref<data_item>
		//  - when accessing a art_data_item_ref<data_item>, it is converted back to a ref<data_item,_,_,_> through a art_data_item_get
		//  - constructor calls of data items are replaced by art_data_item_create calls
		//  - todo: destruction of data items must be marked!

		assert_correct_ir(code);

		auto& mgr = code->getNodeManager();
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();

		auto res = core::transform::transformBottomUp(code,[&](const NodePtr& node)->NodePtr {

			// replace reference types
			if (auto type = node.isa<TypePtr>()) {
				if (isDataItem(type)) {
					return backend::getDataItemReferenceType(type);
				}
				return type;
			}

			// check for constructor calls
			if (auto decl = node.isa<DeclarationStmtPtr>()) {

				// get the variable
				auto var = decl->getVariable();
				auto dataItemRefType = insieme::core::analysis::getReferencedType(var->getType());

				// if the declared value is a DataItemReference, we have to fix the constructor
				if (backend::isDataItemReference(dataItemRefType)) {

					// retrieve the initialization value
					auto init = decl->getDeclaration()->getInitialization();

					// this should be a call expression
					assert_true(init.isa<CallExprPtr>());

					// => replace it by a data item creation call
					ExpressionList args = init.as<CallExprPtr>()->getArgumentList();
					args[0] = builder.getTypeLiteral(backend::getReferencedDataItemType(dataItemRefType));

					// build the substitution declaration statement
					auto res = builder.declarationStmt(
						builder.declaration(var->getType(),builder.callExpr(ext.getCreateDataItem(),args)),
						var
					);

					return res;
				}
			}

			// check for accesses
			if (auto call = node.isa<CallExprPtr>()) {
				// if the targeted function is a member function
				if (call->getFunctionExpr()->getType().as<FunctionTypePtr>()->isMemberFunction()) {
					// if the first argument is a reference to a data item reference
					auto arg0 = call->getArgument(0);
					if (backend::isDataItemReference(insieme::core::analysis::getReferencedType(arg0->getType()))) {
						// we need to unpack the data item reference
						auto newArg = builder.callExpr(ext.getGetDataItem(),core::lang::buildRefKindCast(arg0,core::lang::ReferenceType::Kind::CppReference));
						return core::transform::replaceNode(mgr,CallExprAddress(call)->getArgument(0),newArg);
					}
				}
			}

			// everything else, we are not interested
			return node;

		}, core::transform::globalReplacement);

		assert_correct_ir(res);

		// done
		return res;
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
