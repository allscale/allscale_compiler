#pragma once

#include <memory>
#include <boost/filesystem/path.hpp>

#include "insieme/backend/backend.h"

namespace allscale {
namespace compiler {
namespace backend {


	/**
	 * The main interface for the AllScale backend, converting the given code fragment
	 * into target code. The AllScale backend is thereby producing C++14 code, utilizing
	 * the AllScale runtime interface for realizing parallel operators.
	 *
	 * @param code the code to be converted
	 * @return the converted target code
	 */
	insieme::backend::TargetCodePtr convert(const insieme::core::NodePtr& code);

	/**
	 * Compiles the given target code into a binary located at the given target location.
	 *
	 * @param code the target code to be compiled
	 * @param targetBinary the target binary to be created (if it already exists, it will be overriden)
	 * @param optimization_level the optimization level for the -O flag of the target code compiler
	 * @param syntax_only a flag indicating whether only the syntax should be checked, but the compilation skipped (for testing the backend)
	 * @return true on success, false otherwise
	 */
	bool compileTo(const insieme::backend::TargetCodePtr& code, const boost::filesystem::path& targetBinary, unsigned optimization_level = 3, bool syntax_only = false);

	/**
	 * Converts and compiles the given IR code into a binary located at the given target location using
	 * the AllScale backend.
	 *
	 * @param code the code to be converted and compiled
	 * @param targetBinary the target binary to be created (if it already exists, it will be overriden)
	 * @param optimization_level the optimization level for the -O flag of the target code compiler
	 * @param syntax_only a flag indicating whether only the syntax should be checked, but the compilation skipped (for testing the backend)
	 * @return true on success, false otherwise
	 */
	bool compileTo(const insieme::core::NodePtr& code, const boost::filesystem::path& targetBinary, unsigned optimization_level = 3, bool syntax_only = false) {
		return compileTo(convert(code), targetBinary, optimization_level, syntax_only);
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
