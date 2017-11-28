#pragma once

#include <fstream>
#include <boost/filesystem.hpp>

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/utils/test_utils.h"

#include "allscale/compiler/frontend/allscale_frontend.h"


namespace allscale {
namespace compiler {
namespace frontend {

	namespace fs = boost::filesystem;

	insieme::core::ProgramPtr parseFile(insieme::core::NodeManager& mgr, const fs::path& file) {
		return insieme::frontend::utils::parseFile(mgr,file,configureConversionJob);
	}

	insieme::core::ProgramPtr parseCode(insieme::core::NodeManager& mgr, const std::string& code) {
		return insieme::frontend::utils::parseCode(mgr,code,configureConversionJob);
	}

	insieme::core::TypePtr parseType(insieme::core::NodeManager& mgr, const std::string& type) {
		return insieme::frontend::utils::parseType(mgr,"",type,configureConversionJob);
	}

	insieme::core::TypePtr parseType(insieme::core::NodeManager& mgr, const std::string& header, const std::string& type) {
		return insieme::frontend::utils::parseType(mgr,header,type,configureConversionJob);
	}

} // end namespace frontend
} // end namespace compiler
} // end namespace allscale

