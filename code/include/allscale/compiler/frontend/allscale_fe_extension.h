
#pragma once

#include "insieme/frontend/extensions/frontend_extension.h"

#include <vector>
#include <map>

#include <boost/optional.hpp>

#include "allscale/compiler/frontend/allscale_fe_extension_types.h"
#include "allscale/compiler/frontend/allscale_fe_extension_exprs.h"

namespace allscale {
namespace compiler {
namespace frontend {

	class AllscaleExtension : public insieme::frontend::extensions::FrontendExtension {

		detail::TypeMapper typeMapper;

		detail::TypeMapper& getTypeMapper(insieme::frontend::conversion::Converter& converter);

	  protected:
		virtual boost::optional<std::string> isPrerequisiteMissing(insieme::frontend::ConversionSetup& setup) const override;

		// Types

		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
			                                     insieme::frontend::conversion::Converter& converter) override;

		// Expressions

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
			                                           insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr Visit(const clang::CastExpr* castExpr, insieme::core::ExpressionPtr& irExpr, insieme::core::TypePtr& irTargetType,
			                                       insieme::frontend::conversion::Converter& converter) override;

		virtual std::pair<insieme::core::VariablePtr, insieme::core::ExpressionPtr> PostVisit(const clang::VarDecl* varDecl,
			                                                                                  const insieme::core::VariablePtr& var,
			                                                                                  const insieme::core::ExpressionPtr& varInit,
			                                                                                  insieme::frontend::conversion::Converter& converter) override;

		// TU and Program

		virtual insieme::core::tu::IRTranslationUnit IRVisit(insieme::core::tu::IRTranslationUnit& tu) override;

		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog) override;
	};
}
}
}
