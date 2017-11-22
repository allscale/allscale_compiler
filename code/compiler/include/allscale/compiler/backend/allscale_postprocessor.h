#pragma once

#include "insieme/backend/preprocessor.h"

#include "insieme/backend/postprocessor.h"
#include "insieme/backend/c_ast/c_ast.h"

namespace allscale {
namespace compiler {
namespace backend {

	/**
	 * A post-processor adapting the generated C AST to our needs.
	 *
	 * This includes:
	 * - change DataItemReference& to DataItemReference for fields of structs
	 */
	class AllScalePostProcessor : public insieme::backend::PostProcessor {
	  public:

		insieme::backend::c_ast::NodePtr process(insieme::backend::c_ast::CNodeManager& manager, const insieme::backend::c_ast::NodePtr& code) override;
	};

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
