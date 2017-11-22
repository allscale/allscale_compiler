#include "allscale/compiler/core/data_item_conversion.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
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
		//  - every ref<data_item,_,_,_> is transformed to a art_data_item_ref<data_item>
		//  - when accessing a art_data_item_ref<data_item>, it is converted back to a ref<data_item,_,_,_> through a art_data_item_get
		//  - constructor calls of data items are replaced by art_data_item_create calls
		//  - todo: destruction of data items must be marked!

		auto& mgr = code->getNodeManager();
		IRBuilder builder(mgr);
		const auto& ext = mgr.getLangExtension<backend::AllScaleBackendModule>();

		return core::transform::transformBottomUp(code,[&](const NodePtr& node)->NodePtr {

			// replace reference types
			if (auto type = node.isa<TypePtr>()) {
				if (isDataItem(type)) {
					return backend::getDataItemReferenceType(type);
				}
				return type;
			}

			// check for accesses
			if (const auto& call = node.isa<CallExprPtr>()) {

				if(core::analysis::isConstructorCall(call)) {

					// TODO: use utilities in core::analysis when available
					auto isCopyOrMoveConstructorType = [](const FunctionTypePtr& type) {
						if(type->getParameterTypeList().size() != 2) { return false; }

						auto thisType = core::analysis::getObjectType(type);

						if(!core::lang::isReference(type->getParameterType(1))) { return false; }

						auto refType = core::lang::ReferenceType(type->getParameterType(1));

						if(refType.getElementType() != thisType) { return false; }

						return refType.getKind() == core::lang::ReferenceType::Kind::CppReference || refType.getKind() == core::lang::ReferenceType::Kind::CppRValueReference;
					};

					ExpressionPtr callee = call->getFunctionExpr();

					// must not modify copy or move constructor calls to avoid nested "create data item" calls to data item manager
					if(isCopyOrMoveConstructorType(callee->getType().as<FunctionTypePtr>())) { return node; }

					core::TypePtr objectType = core::analysis::getObjectType(callee->getType());
					if(backend::isDataItemReference(objectType)) {
						// replace constructor call with call to getCreateDataItem
						ExpressionList args = call->getArgumentList();
						args[0] = builder.getTypeLiteral(backend::getReferencedDataItemType(objectType));

						return builder.callExpr(ext.getCreateDataItem(), args);
					}
				}

				// if the targeted function is a member function
				auto funType = call->getFunctionExpr()->getType().as<FunctionTypePtr>();
				if (funType->isMemberFunction()) {

					// only interested if the object type is a data item reference
					if (!backend::isDataItemReference(insieme::core::analysis::getObjectType(funType))) return call;

					// unwrap each argument that is a data item reference
					std::map<NodeAddress,NodePtr> replacements;
					for(const auto& arg : CallExprAddress(call)->getArgumentList()) {
						if (backend::isDataItemReference(insieme::core::analysis::getReferencedType(arg->getType()))) {
							// we need to unpack the data item reference
							replacements[arg] = builder.callExpr(ext.getGetDataItem(),core::lang::buildRefKindCast(arg,core::lang::ReferenceType::Kind::CppReference));
						}
					}

					// apply replacement
					return (replacements.empty()) ? call : core::transform::replaceAll(mgr,replacements);
				}
			}

			// everything else, we are not interested
			return node;

		}, core::transform::globalReplacement);
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
