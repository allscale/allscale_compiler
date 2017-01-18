
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

	const boost::filesystem::path up("../");

	inline const string getAllscaleSourceRootDir() {
		return (boost::filesystem::path(__FILE__).parent_path() / up / up / up).string();
	}

	inline const string getAllscaleTestRootDir() {
		return getAllscaleSourceRootDir() + "/../test/";
	}

	inline const string getAllscaleBuildRootDir() {
		return string(ALLSCALE_BUILD_ROOT); // ALLSCALE_BUILD_ROOT is supplied by cmake
	}

	inline const std::string getVersion() {
		//TODO implement
		return "epsilon";
	}

} // end namespace compiler
} // end namespace allscale
