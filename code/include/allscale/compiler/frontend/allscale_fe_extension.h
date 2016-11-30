
#include "insieme/frontend/extensions/frontend_extension.h"

#include <vector>
#include <map>

namespace allscale {
namespace compiler {
namespace frontend {

	using TranslationState = std::pair<insieme::core::TypePtr, insieme::core::TypePtr>;
	using ClangIrTypeMap = std::map<const clang::Type*, insieme::core::TypePtr>;

	class TranslationStateManager {

		std::vector<TranslationState> translationStates;
		ClangIrTypeMap clangTypeMappings;
		insieme::core::NodeMap irTypeMappings;

		public:
		void pushState(const TranslationState translationState);
		void popState();

		TranslationState getState();

		insieme::core::TypePtr getClangTypeMapping(const clang::QualType& clangType) const;

		const insieme::core::NodeMap& getIrTypeMappings() const;

		void addTypeMappings(const clang::QualType& clangType, const insieme::core::TypePtr& targetIrType,
		                     insieme::frontend::conversion::Converter& converter);
	};

	class AllscaleExtension : public insieme::frontend::extensions::FrontendExtension {

		TranslationStateManager translationStateManager;

		TranslationStateManager& getTranslationStateManager() { return translationStateManager; }

		virtual insieme::core::ExpressionPtr Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr Visit(const clang::CastExpr* castExpr,
		                                           insieme::core::ExpressionPtr& irExpr, insieme::core::TypePtr& irTargetType,
		                                           insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
			                                           insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
			                                     insieme::frontend::conversion::Converter& converter) override;

		virtual std::pair<insieme::core::VariablePtr, insieme::core::ExpressionPtr> PostVisit(const clang::VarDecl* varDecl,
			                                                                                  const insieme::core::VariablePtr& var,
			                                                                                  const insieme::core::ExpressionPtr& varInit,
			                                                                                  insieme::frontend::conversion::Converter& converter) override;

		virtual insieme::core::tu::IRTranslationUnit IRVisit(insieme::core::tu::IRTranslationUnit& tu) override;

		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog) override;
	};
}
}
}
