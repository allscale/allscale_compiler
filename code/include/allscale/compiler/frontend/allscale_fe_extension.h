
#include "insieme/frontend/extensions/frontend_extension.h"

namespace allscale {
namespace compiler {
namespace frontend {

	class AllscaleExtension : public insieme::frontend::extensions::FrontendExtension {

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
		                                         insieme::frontend::conversion::Converter& converter) override;
	};

}
}
}
