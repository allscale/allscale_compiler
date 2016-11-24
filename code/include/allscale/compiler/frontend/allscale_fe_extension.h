
#include "insieme/frontend/extensions/frontend_extension.h"

#include <vector>
#include <map>

namespace allscale {
namespace compiler {
namespace frontend {

	using TranslationState = std::pair<insieme::core::TypePtr, insieme::core::TypePtr>;
	using ClangIrTypeMap = std::map<const clang::Type*, insieme::core::TypePtr>;

	class TranslationStateManager {

		ClangIrTypeMap typeMappings;
		std::vector<TranslationState> translationStates;

		public:
		void pushState(const TranslationState translationState);
		void popState();

		TranslationState getState();

		ClangIrTypeMap& getTypeMappings();
	};

	class AllscaleExtension : public insieme::frontend::extensions::FrontendExtension {

		TranslationStateManager translationStateManager;

		TranslationStateManager& getTranslationStateManager() { return translationStateManager; }

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::frontend::stmtutils::StmtWrapper Visit(const clang::Stmt* stmt, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
			                                           insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
			                                     insieme::frontend::conversion::Converter& converter) override;

		virtual std::pair<insieme::core::VariablePtr, insieme::core::ExpressionPtr> PostVisit(const clang::VarDecl* varDecl,
			                                                                                  const insieme::core::VariablePtr& var,
			                                                                                  const insieme::core::ExpressionPtr& varInit,
			                                                                                  insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog) override;
	};
}
}
}
