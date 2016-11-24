
#include "insieme/frontend/extensions/frontend_extension.h"

#include <vector>
#include <map>

namespace allscale {
namespace compiler {
namespace frontend {

	class AllscaleExtension : public insieme::frontend::extensions::FrontendExtension {

	  public:
		using TranslationState = std::pair<insieme::core::TypePtr, insieme::core::TypePtr>;

		class TranslationStateManager {
			std::vector<TranslationState> translationStates;

		  public:
			void pushState(const TranslationState translationState);

			TranslationState getState();

			void popState();
		};

	  private:
		TranslationStateManager translationStateManager;

		std::map<const clang::Type*, insieme::core::TypePtr> typeMappings;

		TranslationStateManager& getTranslationStateManager() { return translationStateManager; }

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::frontend::stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
			                                           insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
			                                     insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog) override;
	};

}
}
}
