
#include "allscale/compiler/frontend/allscale_fe_extension.h"

insieme::core::ExpressionPtr allscale::compiler::frontend::AllscaleExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) {
	return nullptr;
}

insieme::core::TypePtr allscale::compiler::frontend::AllscaleExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) {
	return nullptr;
}

