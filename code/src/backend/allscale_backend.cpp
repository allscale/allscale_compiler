#include "allscale/compiler/backend/allscale_backend.h"

#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

namespace allscale {
namespace compiler {
namespace backend {

	// include allscale runtime configuration paths
	#include "runtime/config.inc"


	/**
	 * The actual implementation of the AllScale Backend.
	 */
	class AllScaleBackend : public insieme::backend::sequential::SequentialBackend {



	};


	insieme::backend::TargetCodePtr convert(const insieme::core::NodePtr& code) {
		// simply use the AllScale backend to convert the code
		return AllScaleBackend().convert(code);
	}

	bool compileTo(const insieme::backend::TargetCodePtr& code, const boost::filesystem::path& targetBinary) {
		namespace ic = insieme::utils::compiler;

		// check the input code
		if (!code) return false;

		// get a compiler instance
		auto compiler = ic::Compiler::getDefaultCppCompiler();

		// - customize compiler -
		// include directories
		compiler.addIncludeDir(ALLSCALE_RUNTIME_INCLUDE_DIR);
		compiler.addIncludeDir(HPX_INCLUDE_DIR);
		compiler.addIncludeDir(HPX_ROOT_DIR);		// contains some configuration files
		compiler.addFlag("-isystem " + BOOST_INCLUDE_DIR);

		// add libraries
		compiler.addExternalLibrary(ALLSCALE_RUNTIME_LIBRARY_DIR, "allscale");
		compiler.addExternalLibrary(HPX_LIBRARY_DIR, "hpx_init");
		compiler.addExternalLibrary(HPX_LIBRARY_DIR, "hpx");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_chrono");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_date_time");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_filesystem");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_program_options");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_regex");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_system");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_thread");
		compiler.addExternalLibrary(BOOST_LIBRARY_DIR, "boost_atomic");

		compiler.addLibrary("dl");
		compiler.addLibrary("rt");
		compiler.addLibrary("pthread");


		// run compiler on target code
		return ic::compileToBinary(*code,targetBinary.string(),compiler);
	}



} // end namespace backend
} // end namespace compiler
} // end namespace allscale
