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
#include "allscale/compiler/lang/allscale_ir.h"


#include "insieme/core/dump/json_dump.h"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	NodePtr convertDataItemReferences(const NodePtr& code, const ProgressCallback&) {

		// To be transformed:
		//  - every data_item is transformed to a art_data_item_ref<data_item>
		//  - when accessing a art_data_item_ref<data_item>, it is converted back to a data_item through a art_data_item_get
		//  - constructor calls of data items are replaced by art_data_item_create calls

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

		// remove data item tag on references (the are produced above as a side-effect)
		visitDepthFirstOnce(res,[&](const TypePtr& type) {
			if (backend::isDataItemReference(type)) {
				removeDataItemMark(type);
			}
		},true,true);

		// done
		return res;
	}

	namespace {

		bool containsDataItemReference(const TypePtr& type) {
			bool res = false;
			visitDepthFirstOncePrunable(type,[&](const NodePtr& node){
				// check whether it can be pruned here
				if (res || node.isa<ExpressionPtr>()) return core::Action::Prune;

				// check whether the current type is a data item reference
				if (auto type = node.isa<TypePtr>()) {
					if (backend::isDataItemReference(type)) {
						res = true;
						return core::Action::Prune;
					}
				}

				// continue
				return core::Action::Descent;
			});
			return res;
		}

		int numNestedRefs(const TypePtr& type) {
			if (core::lang::isReference(type)) {
				return 1 + numNestedRefs(core::analysis::getReferencedType(type));
			}
			return 0;
		}

	}


	insieme::core::NodePtr convertCapturedDataItemReferences(const insieme::core::NodePtr& node, const ProgressCallback&) {
		auto& mgr = node->getNodeManager();
		IRBuilder builder(mgr);
		const auto& basic = mgr.getLangBasic();
		const auto& refExt = mgr.getLangExtension<core::lang::ReferenceExtension>();

		// a utility to identify fields of captur structs
		auto isCapturedFieldName = [](const StringValuePtr& name) {
			return name->getValue().substr(0,8) == "capture_";
		};

		// a utility to identify types of captured fields that need to be rewritten
		auto isTargetedFieldType = [](const TypePtr& type) {
			if (!core::lang::isReference(type)) return TypePtr();
			auto elementType = core::analysis::getReferencedType(type);
			return containsDataItemReference(elementType) ? elementType : TypePtr();
		};

		// convert captured data item references to values
		auto res = core::transform::transformBottomUp(node,[&](const NodePtr& node)->NodePtr {

			// replace field types
			if (auto field = node.isa<FieldPtr>()) {
				auto type = field->getType();

				// check that this is a captured value (not too save, sorry)
				if (!isCapturedFieldName(field->getName())) return field;

				// if this is a reference to something containing a data item reference
				if (auto resType = isTargetedFieldType(type)) {
					// we have to capture it by value instead
					return builder.field(field->getName(),resType);
				}
			}

			// also replace field accesses
			if (auto call = node.isa<CallExprPtr>()) {

				// first we are interested in member accesses
				bool isAccess = core::analysis::isCallOf(call,basic.getCompositeMemberAccess());
				bool isRefAccess = !isAccess && core::analysis::isCallOf(call,refExt.getRefMemberAccess());
				if (isAccess || isRefAccess) {

					// extract arguments
					auto obj = call->getArgument(0);
					auto field = call->getArgument(1);
					auto type = call->getArgument(2);

					// check field name
					if (isCapturedFieldName(field.as<LiteralPtr>()->getValue())) {

						// check field type
						if (auto newType = isTargetedFieldType(core::analysis::getRepresentedType(type))) {
							return builder.callExpr(
									(isAccess) ? basic.getCompositeMemberAccess() : refExt.getRefMemberAccess(),
									obj, field, builder.getTypeLiteral(newType)
							);
						}
					}
				}

				// where might also be too much deref calls
				if (refExt.isCallOfRefDeref(call)) {

					// if this is the proper type => skip the deref
					auto arg = call->getArgument(0);
					if (numNestedRefs(call->getType()) == numNestedRefs(arg->getType())) {
						if (refExt.isCallOfRefMemberAccess(arg) && isTargetedFieldType(arg->getType())) {
							// perform a ref-cast instead
							return core::lang::buildRefCast(arg,call->getType());
						}
					}
				}

				return call;

				// for all other calls => rebuild call to update result type
				auto fun = call->getFunctionExpr();
				auto args = call->getArgumentList();
				return builder.callExpr(fun,args);
			}


			// default, do nothing
			return node;
		}, core::transform::globalReplacement);

		// check wether this step was correct
		assert_correct_ir(res);

		// done
		return res;
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
