
#pragma once

#include <boost/filesystem/path.hpp>

// The macro defining the allscale version information
#ifndef ALLSCALE_VERSION
// fall-back if not defined by cmake
#define ALLSCALE_VERSION "unknown"
#endif

namespace allscale {
namespace compiler {

	using std::string;

	inline const string getAllscaleSourceRootDir() {
		return string(ALLSCALE_SOURCE_ROOT); // ALLSCALE_SOURCE_ROOT is supplied by cmake
	}

	inline const string getAllscaleTestRootDir() {
		return getAllscaleSourceRootDir() + "/../test/";
	}

	inline const string getAllscaleBuildRootDir() {
		return string(ALLSCALE_BUILD_ROOT); // ALLSCALE_BUILD_ROOT is supplied by cmake
	}

	inline const string getAllscaleAPICoreIncludeDir() {
		return getAllscaleSourceRootDir() + "../api/code/api/include/";
	}

	inline const string getAllscaleAPIUtilsIncludeDir() {
		return getAllscaleSourceRootDir() + "../api/code/utils/include/";
	}

	inline const string getAllscaleAPIInterceptionIncludePath() {
		return "allscale/api/core";
	}

	inline const string getAllscaleAPIDataItemsInterceptionIncludePath() {
		return "allscale/api/user/data";
	}

	inline const std::string getVersion() {
		//TODO implement
		return "epsilon";
	}

} // end namespace compiler
} // end namespace allscale
