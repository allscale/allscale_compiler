
#include "insieme/frontend/extensions/frontend_extension.h"

namespace allscale {
namespace compiler {
namespace frontend {

	class AllscaleExtension : public insieme::frontend::extensions::FrontendExtension {

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;
		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

	};

}
}
}
