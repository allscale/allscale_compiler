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
	 * A collection of configuration options for the backend compiler invocation.
	 */
	struct CompilerConfig {

		// the optimization level for the -O flag of the target code compiler
		unsigned optimization_level = 3;

		// a flag indicating whether data item access checks during runtime should be enabled
		bool checkDataItemAccesses = false;

		// extra definitions to be forwarded to the backend compiler
		std::vector<std::string> definitions;

	};

	/**
	 * Compiles the given target code into a binary located at the given target location.
	 *
	 * @param code the target code to be compiled
	 * @param targetBinary the target binary to be created (if it already exists, it will be overriden)
	 * @param config the configuration for the compilation process
	 * @return true on success, false otherwise
	 */
	bool compileTo(const insieme::backend::TargetCodePtr& code, const boost::filesystem::path& targetBinary, const CompilerConfig& config = CompilerConfig());

	/**
	 * Converts and compiles the given IR code into a binary located at the given target location using
	 * the AllScale backend.
	 *
	 * @param code the code to be converted and compiled
	 * @param targetBinary the target binary to be created (if it already exists, it will be overriden)
	 * @param config the configuration for the compilation process
	 * @return true on success, false otherwise
	 */
	inline bool compileTo(const insieme::core::NodePtr& code, const boost::filesystem::path& targetBinary, const CompilerConfig& config = CompilerConfig()) {
		return compileTo(convert(code), targetBinary, config);
	}


} // end namespace backend
} // end namespace compiler
} // end namespace allscale
