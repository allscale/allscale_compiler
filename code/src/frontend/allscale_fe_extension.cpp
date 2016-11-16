
#include "allscale/compiler/frontend/allscale_fe_extension.h"

using namespace insieme;

namespace allscale {
namespace compiler {
namespace frontend {

	core::ExpressionPtr AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

	core::TypePtr AllscaleExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
		return nullptr;
	}

}
}
}

