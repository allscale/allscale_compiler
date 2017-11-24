
#include "allscale/compiler/core/data_item_annotation.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/encoder.h"


namespace allscale {
namespace compiler {
namespace core {

	struct DataItemTag : public insieme::core::value_annotation::copy_on_migration {};

namespace {

	using namespace insieme::core;

	// ---------------- Support Dump ----------------------

	VALUE_ANNOTATION_CONVERTER(DataItemTag)

		typedef core::value_node_annotation<DataItemTag>::type annotation_type;

		virtual insieme::core::ExpressionPtr toIR(insieme::core::NodeManager& manager, const insieme::core::NodeAnnotationPtr& annotation) const override {
			assert_true(dynamic_pointer_cast<annotation_type>(annotation)) << "Only data item tag annotations supported!";
			return encoder::toIR(manager, string("dataitem"));
		}

		virtual insieme::core::NodeAnnotationPtr toAnnotation(const insieme::core::ExpressionPtr& node) const override {
			assert_true(encoder::isEncodingOf<string>(node.as<ExpressionPtr>())) << "Invalid encoding encountered!";
			return std::make_shared<typename insieme::core::value_node_annotation<DataItemTag>::type>(DataItemTag());
		}

	VALUE_ANNOTATION_CONVERTER_END

}

void markAsDataItem(const insieme::core::TypePtr& node) {
	node->attachValue<DataItemTag>();
}

void removeDataItemMark(const insieme::core::TypePtr& node) {
	node->detachValue<DataItemTag>();
}

bool isDataItem(const insieme::core::NodePtr& node) {
	return node->hasAttachedValue<DataItemTag>();
}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
