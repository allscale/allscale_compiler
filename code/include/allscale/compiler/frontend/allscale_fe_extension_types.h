
#pragma once

#include <string>
#include <map>
#include <functional>

#include "insieme/frontend/clang.h"
#include "insieme/core/ir_node.h"


namespace insieme {
namespace frontend {
namespace conversion {
	class Converter;
}
}
}

namespace allscale {
namespace compiler {
namespace frontend {

	namespace detail {

		class TypeMapper {
			std::map<std::string, insieme::core::TypePtr> typeIrMap;

			using CodeExtractor = std::function<insieme::core::TypePtr(const clang::RecordDecl* recordDecl, const insieme::core::TypePtr& irType)>;
			std::map<insieme::core::TypePtr, CodeExtractor> placeholderReplacer;

		  public:
			/// initialize this instance of the type mapper using the given converter, if not already done
			void initializeIfNeeded(insieme::frontend::conversion::Converter& converter);

			/// map the given clang type to IR. Returns nullptr if the type doesn't concern AllScale.
			insieme::core::TypePtr apply(const clang::Type* type);
		};
	}
}
}
}
