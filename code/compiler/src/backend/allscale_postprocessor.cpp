#include "allscale/compiler/backend/allscale_postprocessor.h"


namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::backend;

	c_ast::NodePtr AllScalePostProcessor::process(c_ast::CNodeManager& manager, const c_ast::NodePtr& code) {
		// only postprocess type definitions for structs
		if(const auto& typeDefinition = code.isa<c_ast::TypeDefinitionPtr>()) {
			if(const auto& structType = typeDefinition->type.isa<c_ast::StructTypePtr>()) {
				// once we are limited to struct types, we search for fields of DataItemReference& type
				for(const auto& field : structType->elements) {
					if(const auto& variable = field.isa<c_ast::VariablePtr>()) {
						if(const auto& referenceType = variable->type.isa<c_ast::ReferenceTypePtr>()) {
							if(const auto& elementType = referenceType->elementType.isa<c_ast::NamedTypePtr>()) {
								if(elementType->name->name == "allscale::runtime::DataItemReference") {
									// if we found it, we change the type of the field from reference to value
//									variable->type = elementType;
								}
							}
						}
					}
				}
			}
		}
		// everything else stays untouched
		return code;
	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
