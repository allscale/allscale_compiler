
#include "allscale/compiler/core/data_item_annotation.h"

#include "insieme/core/lang/basic.h"


namespace allscale {
namespace compiler {
namespace core {

namespace {

	/**
	* A Converter ensuring that data item tags are preserved within binary dumps.
	*/
	struct DataItemTagConverter : public insieme::core::dump::AnnotationConverter {

		DataItemTagConverter() : AnnotationConverter("DataItemTag") {}

		virtual insieme::core::ExpressionPtr toIR(insieme::core::NodeManager& manager, const insieme::core::NodeAnnotationPtr& annotation) const override {
			// the actual node here is not really important ... it just must not be null
			return manager.getLangBasic().getTrue();
		}

		virtual insieme::core::NodeAnnotationPtr toAnnotation(const insieme::core::ExpressionPtr& node) const override {
			return std::make_shared<typename insieme::core::value_node_annotation<DataItemTag>::type>(DataItemTag());
		}

	};

	// register the converter into the central annotation converter registry
	__insieme_unused bool reg = insieme::core::dump::AnnotationConverterRegister::getDefault().registerConverter<DataItemTagConverter, DataItemTag>();
}

void markAsDataItem(const insieme::core::TypePtr& node) {
	node->attachValue<DataItemTag>();
}

bool isDataItem(const insieme::core::NodePtr& node) {
	return node->hasAttachedValue<DataItemTag>();
}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
