#include "allscale/compiler/backend/allscale_backend.h"

#include "insieme/backend/backend.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/preprocessor.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"

#include "insieme/utils/compiler/compiler.h"

#include "allscale/compiler/backend/allscale_preprocessor.h"
#include "allscale/compiler/backend/allscale_type_handler.h"
#include "allscale/compiler/backend/allscale_operator.h"

namespace allscale {
namespace compiler {
namespace backend {

	// include allscale runtime configuration paths
	#include "runtime/config.inc"

	namespace be = insieme::backend;

	/**
	 * The actual implementation of the AllScale Backend.
	 */
	class AllScaleBackend : public be::Backend {

	public:

		AllScaleBackend() {
			addDefaultAddons();
		}

	protected:

		virtual be::Converter buildConverter(insieme::core::NodeManager& manager) const {
			// create and set up the converter
			be::Converter converter(manager, "AllScaleBackend", getConfiguration());

			// set up name manager
			converter.setNameManager(std::make_shared<be::SimpleNameManager>("allscale"));

			// set up pre-processing
			converter.setPreProcessor(be::makePreProcessorSequence(
				be::makePreProcessor<EntryPointWrapper>(),
				be::getBasicPreProcessorSequence()
			));

			// register support for additional types
			converter.getTypeManager().addTypeHandler(AllScaleTypeHandler);

			// register support for additional operators
			addRuntimeSpecificOps(manager,converter.getFunctionManager().getOperatorConverterTable());

			// done
			return converter;
		}

	};


	be::TargetCodePtr convert(const insieme::core::NodePtr& code) {
		// simply use the AllScale backend to convert the code
		return AllScaleBackend().convert(code);
	}

	bool compileTo(const be::TargetCodePtr& code, const boost::filesystem::path& targetBinary, bool debug) {
		namespace ic = insieme::utils::compiler;

		// check the input code
		if (!code) return false;

		// get a compiler instance
		auto compiler = ic::Compiler::getDefaultCppCompiler();

		// - customize compiler -

		// optimization level
		if (debug) {
			compiler.addFlag("-O0");
			compiler.addFlag("-g3");
		} else {
			compiler.addFlag("-O3");
		}

		// include directories
		compiler.addIncludeDir(ALLSCALE_RUNTIME_INCLUDE_DIR);
		compiler.addIncludeDir(HPX_INCLUDE_DIR);
		compiler.addIncludeDir(HPX_ROOT_DIR);		// contains some configuration files
		compiler.addFlag("-isystem " + BOOST_INCLUDE_DIR);

		// to be not over-critical in order of libraries
		compiler.addFlag("-Wl,--no-as-needed");

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

		compiler.addFlag("-Wl,-rpath=" + HPX_LIBRARY_DIR + ":" + ALLSCALE_RUNTIME_LIBRARY_DIR + ":" + BOOST_LIBRARY_DIR);

		// run compiler on target code
		return ic::compileToBinary(*code,targetBinary.string(),compiler);
	}



} // end namespace backend
} // end namespace compiler
} // end namespace allscale
