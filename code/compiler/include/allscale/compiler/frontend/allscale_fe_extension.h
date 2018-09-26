
#pragma once

#include "insieme/frontend/extensions/mapping_frontend_extension.h"

#include <vector>
#include <map>

#include <boost/optional.hpp>

namespace allscale {
namespace compiler {
namespace frontend {

	class AllscaleExtension : public insieme::frontend::extensions::MappingFrontendExtension {

	  protected:
		virtual boost::optional<std::string> isPrerequisiteMissing(insieme::frontend::ConversionSetup& setup) const override;

		// Types

		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

		virtual std::map<std::string, std::string> getTypeMappings() override;

		// Expressions

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
		                                               insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr Visit(const clang::CastExpr* castExpr, insieme::core::ExpressionPtr& irExpr, insieme::core::TypePtr& irTargetType,
		                                           insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr Visit(const clang::CXXCtorInitializer* ctorInit, const clang::Expr* initExpr,
		                                           insieme::core::ExpressionPtr& irInitializedMemLoc,
		                                           insieme::frontend::conversion::Converter& converter) override ;

		virtual std::vector<insieme::frontend::extensions::detail::FilterMapper> getExprMappings() override;

		// TU and Program

		virtual insieme::core::tu::IRTranslationUnit IRVisit(insieme::core::tu::IRTranslationUnit& tu) override;

		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog) override;


	  public:
		AllscaleExtension();
	};


	insieme::core::ExpressionPtr applyDataItemProcessing(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
	                                                     insieme::frontend::conversion::Converter& converter);

}
}
}
