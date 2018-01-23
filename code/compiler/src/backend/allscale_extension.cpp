#include "allscale/compiler/backend/allscale_extension.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::core;

	namespace {

		TypePtr toType(const NodePtr& node) {
			TypePtr type = node.isa<TypePtr>();
			if (!type && node.isa<ExpressionPtr>()) {
				type = node.as<ExpressionPtr>()->getType();
			}
			return type;
		}

	}

	bool isDataItemReference(const NodePtr& node) {
		// it must be a generic type
		auto genType = toType(node).isa<GenericTypePtr>();
		if (!genType) return false;

		// check properties of the type
		return genType->getParents()->empty()
				&& genType->getTypeParameter()->size() == 1
				&& genType->getFamilyName() == "art_data_item_ref";
	}

	TypePtr getDataItemReferenceType(const TypePtr& type) {
		return GenericType::get(type->getNodeManager(), "art_data_item_ref", TypeList{ type });
	}

	TypePtr getReferencedDataItemType(const NodePtr& node) {
		// if it is a data item reference ...
		if (isDataItemReference(node)) {
			// .. extract the first type parameters
			return toType(node).as<GenericTypePtr>()->getTypeParameter(0);
		}
		// otherwise return a null pointer
		assert_fail() << "Not a data item reference type: " << *node;
		return {};
	}

	bool isDataItemRequirement(const NodePtr& node)  {
		// it must be a generic type
		auto genType = toType(node).isa<GenericTypePtr>();
		if (!genType) return false;

		// check properties of the type
		return genType->getParents()->empty()
				&& genType->getTypeParameter()->size() == 1
				&& genType->getFamilyName() == "art_data_item_requirement";
	}

	TypePtr getDataItemRequirementType(const TypePtr& type) {
		return GenericType::get(type->getNodeManager(), "art_data_item_requirement", TypeList{ type });
	}

	TypePtr getRequiredDataItemType(const NodePtr& node) {
		// if it is a data item requirement ...
		if (isDataItemRequirement(node)) {
			// .. extract the first type parameters
			return toType(node).as<GenericTypePtr>()->getTypeParameter(0);
		}
		// otherwise return a null pointer
		assert_fail() << "Not a data item requirement type: " << *node;
		return {};
	}

	insieme::core::ExpressionPtr createDataItemRequirement(const insieme::core::ExpressionPtr& dataItemRef, const insieme::core::ExpressionPtr& range, analysis::AccessMode mode) {
		NodeManager& mgr = dataItemRef->getNodeManager();
		IRBuilder builder(mgr);

		// resolve the mode expression
		const auto& ext = mgr.getLangExtension<AllScaleBackendModule>();
		ExpressionPtr modeExpr =
				(mode == analysis::AccessMode::ReadOnly)  ? ext.getAccessModeReadOnly()  :
				(mode == analysis::AccessMode::ReadWrite) ? ext.getAccessModeReadWrite() :
				ExpressionPtr();

		// build a call to the corresponding factory function
		return builder.callExpr(ext.getCreateDataItemRequirement(), dataItemRef, range, modeExpr);
	}

	bool isUnusedType(const insieme::core::NodePtr& node) {
		return node->getNodeManager().getLangExtension<AllScaleBackendModule>().isUnusedType(node);
	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
