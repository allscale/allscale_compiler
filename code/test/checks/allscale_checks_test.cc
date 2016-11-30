#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/checks/allscale_checks.h"

using namespace insieme::core;
using namespace allscale::compiler::checks;
using namespace allscale::compiler::lang;

TEST(AllscaleChecks, CppLambdaToClosureCheck) {
	NodeManager nm;
	IRBuilder builder(nm);

	auto init = builder.parseExpr(R"(
		def struct S {
			lambda IMP__operator_call_ = (i : int<4>) -> int<8> {
				return 5;
			}
		};
		<ref<S,f,f,cpp_rref>>(ref_cast(ref_temp(type_lit(S)), type_lit(f), type_lit(f), type_lit(cpp_rref))) {}
	)");
	ASSERT_TRUE(init);
	EXPECT_TRUE(check(init).empty()) << check(init);

	auto correctFunType = builder.parseType("(int<4>) => int<8>").as<FunctionTypePtr>();
	auto correctCall = buildCppLambdaToClosure(init, correctFunType);
	EXPECT_TRUE(check(correctCall).empty()) << check(correctCall);

	auto wrongFunType1 = builder.parseType("(int<4>) => bool").as<FunctionTypePtr>();
	auto wrongCall1 = buildCppLambdaToClosure(init, wrongFunType1);
	EXPECT_FALSE(check(wrongCall1).empty());

	auto wrongFunType2 = builder.parseType("(bool) => int<8>").as<FunctionTypePtr>();
	auto wrongCall2 = buildCppLambdaToClosure(init, wrongFunType2);
	EXPECT_FALSE(check(wrongCall2).empty());
}
