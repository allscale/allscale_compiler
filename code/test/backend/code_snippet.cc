#include <gtest/gtest.h>

#include "allscale/compiler/backend/allscale_backend.h"

#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"

namespace allscale {
namespace compiler {
namespace backend {

	using namespace insieme::core;
	namespace fs = boost::filesystem;


	bool isCompiling(const insieme::backend::TargetCodePtr& code) {

		// get a temporary file path
		auto tmp = fs::unique_path(fs::temp_directory_path() / "allscale-trg-%%%%%%%%");

		bool res = compileTo(code, tmp);

		// delete the temporary
		if (res) fs::remove(tmp);

		// return result
		return res;
	}


	TEST(CodeSnippet, EmptyMain) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		// create an empty code snippet
		auto program = builder.parseProgram(
				"int<4> main() { return 0; }"
		);
		ASSERT_TRUE(program);

		// convert with allscale backend
		auto code = convert(program);
		ASSERT_TRUE(code);

		// check that the resulting source is compiling
		EXPECT_PRED1(isCompiling, code);

	}

} // end namespace backend
} // end namespace compiler
} // end namespace allscale
