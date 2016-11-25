#pragma once

#include <memory>

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast.h"

#include "allscale/compiler/backend/allscale_runtime_entities.h"

namespace allscale {
namespace compiler {
namespace backend {

	// ------------------------------------------------------------------------
	//  Within this header file a list of special code fragments used for
	//  creating code to be executed on the AllScale runtime is defined.
	// ------------------------------------------------------------------------


	namespace detail {

		// a forward declaration for the pimpl pattern
		class WorkItemDescriptionsImpl;

	}

	/**
	 * The summary of the information maintained per work item description.
	 */
	struct WorkItemDescriptionInfo {

		/**
		 * The fragment describing this work item.
		 */
		insieme::backend::c_ast::CodeFragmentPtr definition;

		/**
		 * The type used to describe this work item.
		 */
		insieme::backend::c_ast::TypePtr description_type;

	};

	/**
	 * The code fragment that contains the definitions of all the work item
	 * descriptions utilized within the final application code.
	 */
	class WorkItemDescriptions : public insieme::backend::c_ast::CodeFragment {

		// the pointer to the actual implementation
		std::unique_ptr<detail::WorkItemDescriptionsImpl> impl;

	public:

		/**
		 * Constructor for this code fragment.
		 */
		WorkItemDescriptions(const insieme::backend::Converter&);

		/**
		 * Retrieves the type the given description is mapped to.
		 */
		const WorkItemDescriptionInfo& getDescriptionType(insieme::backend::ConversionContext&, const WorkItemDescription&);

		/**
		 * Retrieves the type the given encoded description is mapped to.
		 */
		const WorkItemDescriptionInfo& getDescriptionType(insieme::backend::ConversionContext&, const insieme::core::ExpressionPtr&);

		/**
		 * Prints this code fragment to some output stream.
		 */
		std::ostream& printTo(std::ostream&) const override;

		/**
		 * Get the descriptions active within the given converter.
		 */
		static WorkItemDescriptions& getInstance(const insieme::backend::Converter& converter);

	};

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
