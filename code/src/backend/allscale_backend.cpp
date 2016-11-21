#include "allscale/compiler/backend/allscale_backend.h"

#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/utils/compiler/compiler.h"

namespace allscale {
namespace compiler {
namespace backend {


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

		// customize compiler
		// TODO: do so

		// run compiler on target code
		return ic::compileToBinary(*code,targetBinary.string(),compiler);
	}



} // end namespace backend
} // end namespace compiler
} // end namespace allscale
