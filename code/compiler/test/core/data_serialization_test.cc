#include <gtest/gtest.h>

#include <algorithm>
#include <iostream>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/backend/c_ast/c_ast.h"

#include "allscale/compiler/lang/allscale_ir.h"
#include "allscale/compiler/core/data_serialization.h"
#include "allscale/compiler/backend/allscale_extension.h"
#include "allscale/compiler/backend/allscale_backend.h"

#include "../test_utils.inc"

namespace allscale {
namespace compiler {
namespace core {

	using namespace insieme::core;

	TEST(IsSerializable, PositveTest) {
		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		// check basic types
		EXPECT_PRED1(isSerializable, basic.getBool());

		EXPECT_PRED1(isSerializable, basic.getChar());
		EXPECT_PRED1(isSerializable, basic.getWChar16());
		EXPECT_PRED1(isSerializable, basic.getWChar32());

		EXPECT_PRED1(isSerializable, basic.getInt1());
		EXPECT_PRED1(isSerializable, basic.getInt2());
		EXPECT_PRED1(isSerializable, basic.getInt4());
		EXPECT_PRED1(isSerializable, basic.getInt8());

		EXPECT_PRED1(isSerializable, basic.getUInt1());
		EXPECT_PRED1(isSerializable, basic.getUInt2());
		EXPECT_PRED1(isSerializable, basic.getUInt4());
		EXPECT_PRED1(isSerializable, basic.getUInt8());

		EXPECT_PRED1(isSerializable, basic.getReal4());
		EXPECT_PRED1(isSerializable, basic.getReal8());
		EXPECT_PRED1(isSerializable, basic.getReal16());

		// array types
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"using int_array = int[10]; int_array"));

		// enum type
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"enum Bla { X, Y }; Bla"));

		// enum class type
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"enum class Bla { X, Y }; Bla"));

		// check standard library types
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::string"));
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::pair<int,bool>"));
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::array<int,3>"));
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::vector<int>"));

		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::vector<std::pair<std::array<int,3>,std::string>>"));

		EXPECT_PRED1(isSerializable, frontend::parseType(mgr, "std::map<int,int>"));

		// check core types
		auto& ext = mgr.getLangExtension<lang::AllscaleModule>();
		EXPECT_PRED1(isSerializable, lang::TreetureType(basic.getInt4(), false).toIRType());
		EXPECT_PRED1(isSerializable, ext.getTaskRef());

		// check runtime types
		EXPECT_PRED1(isSerializable, backend::getDataItemReferenceType(basic.getInt8()));

	}


	TEST(IsSerializable, NegativeTest) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		auto isNotSerializable = [](const TypePtr& type) {
			return !isSerializable(type);
		};

		// check basic types
		EXPECT_PRED1(isNotSerializable, basic.getUnit());
		EXPECT_PRED1(isNotSerializable, basic.getIntInf());
		EXPECT_PRED1(isNotSerializable, basic.getUIntInf());

		// no reference shall be serializable
		EXPECT_PRED1(isNotSerializable, builder.parseType("ref<int<4>>"));
		EXPECT_PRED1(isNotSerializable, builder.parseType("ref<int<4>,t,f,plain>"));
		EXPECT_PRED1(isNotSerializable, builder.parseType("ref<int<4>,t,f,cpp_ref>"));
		EXPECT_PRED1(isNotSerializable, builder.parseType("ref<int<4>,t,f,cpp_rref>"));

		// no pointer shall be serializable
		EXPECT_PRED1(isNotSerializable, builder.parseType("ptr<int<4>>"));
		EXPECT_PRED1(isNotSerializable, builder.parseType("ptr<int<4>,t,f>"));

		// arrays of pointers shall not be serializable
		EXPECT_PRED1(isNotSerializable, builder.parseType("array<ptr<int<4>>,12>"));
		EXPECT_PRED1(isNotSerializable, builder.parseType("array<int<4>,inf>"));

		// test std-library types with non-serializable elements
		EXPECT_PRED1(isNotSerializable, frontend::parseType(mgr,"std::array<int*,3>"));
		EXPECT_PRED1(isNotSerializable, frontend::parseType(mgr,"std::vector<int*>"));

		EXPECT_PRED1(isNotSerializable, frontend::parseType(mgr,"std::pair<int*,int>"));
		EXPECT_PRED1(isNotSerializable, frontend::parseType(mgr,"std::pair<int,int*>"));

		// test a case where something nested is not working
		EXPECT_PRED1(isNotSerializable, frontend::parseType(mgr,"std::pair<int,std::vector<std::array<int*,3>>>"));
	}

	TEST(AutoSerialization, PrimitiveTypes) {
		NodeManager mgr;
		auto& basic = mgr.getLangBasic();

		// the auto-serialization should work fine for p
		EXPECT_EQ(basic.getBool(), detail::tryMakeSerializable(basic.getBool()));

		EXPECT_EQ(basic.getInt1(), detail::tryMakeSerializable(basic.getInt1()));
		EXPECT_EQ(basic.getInt2(), detail::tryMakeSerializable(basic.getInt2()));
		EXPECT_EQ(basic.getInt4(), detail::tryMakeSerializable(basic.getInt4()));
		EXPECT_EQ(basic.getInt8(), detail::tryMakeSerializable(basic.getInt8()));

		EXPECT_EQ(basic.getUInt1(), detail::tryMakeSerializable(basic.getUInt1()));
		EXPECT_EQ(basic.getUInt2(), detail::tryMakeSerializable(basic.getUInt2()));
		EXPECT_EQ(basic.getUInt4(), detail::tryMakeSerializable(basic.getUInt4()));
		EXPECT_EQ(basic.getUInt8(), detail::tryMakeSerializable(basic.getUInt8()));

	}

	TEST(AutoSerialization, ReferenceAndPointers) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_FALSE(detail::tryMakeSerializable(builder.parseType("ref<int<4>>")));
		EXPECT_FALSE(detail::tryMakeSerializable(builder.parseType("ref<int<4>,t,f,plain>")));
		EXPECT_FALSE(detail::tryMakeSerializable(builder.parseType("ref<int<4>,f,f,cpp_ref>")));
		EXPECT_FALSE(detail::tryMakeSerializable(builder.parseType("ref<int<4>,f,t,cpp_rref>")));

		EXPECT_FALSE(detail::tryMakeSerializable(builder.parseType("ptr<int<4>>")));
		EXPECT_FALSE(detail::tryMakeSerializable(builder.parseType("ptr<int<4>,t,f>")));

	}

	TEST(AutoSerialization, PODs) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = builder.parseType(
				"def struct A { x : int<4>; y : real<4>; }; A"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_FALSE(isSerializable(type)) << *type;

		// but it should be possible to make it serializable
		auto mod = detail::tryMakeSerializable(type);
		ASSERT_TRUE(mod);
		EXPECT_PRED1(isSerializable, mod);
		assert_correct_ir(mod);
	}

	TEST(AutoSerialization, UserDefinedSerialization) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = frontend::parseType(
				mgr,
				"#include <allscale/utils/serializer.h>",
				"struct A { "
				"	int x; "
				"	double y; "
				"	static A load(allscale::utils::ArchiveReader& reader) { "
				"		int x = reader.read<int>();"
				"		double y = reader.read<double>();"
				"		return { x, y }; "
				"	}"
				"	void store(allscale::utils::ArchiveWriter& writer) const {"
				"		writer.write<int>(x);"
				"		writer.write<double>(y);"
				"	}"
				"}"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_TRUE(isSerializable(type)) << *type;

		// it should also not change when adding serialization code
		EXPECT_EQ(type,detail::tryMakeSerializable(type));
	}

	TEST(AutoSerialization, UserDefinedSerialization2) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = frontend::parseType(
			mgr,
			"#include <allscale/utils/serializer.h>\n#include <map>",
			"struct A { "
			"	std::map<int,int> a; "
			"}"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_FALSE(isSerializable(type)) << *type;


		// it should change when adding serialization code
		const auto& type2 = detail::tryMakeSerializable(type);
		EXPECT_NE(type, type2);

		// this one should be serializable
		EXPECT_TRUE(isSerializable(type2)) << *type2;
	}

	TEST(AutoSerialization, RecursiveTypes) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;

		// check a simple recursive type first
		type = builder.parseType(
				"def struct A {"
				"	x : int<4>;"
				"	r : ref<A>;"
				"};"
				""
				"A"
		);

		EXPECT_TRUE(type);
		EXPECT_TRUE(type.as<TagTypePtr>()->isRecursive());
		EXPECT_FALSE(isSerializable(type));
		EXPECT_FALSE(detail::tryMakeSerializable(type));

		// check also a mutually recursive type
		type = builder.parseType(
				"decl struct A;"
				"decl struct B;"
				"def struct A {"
				"	x : int<4>;"
				"	r : ref<B>;"
				"};"
				""
				"def struct B {"
				"	x : int<4>;"
				"	r : ref<A>;"
				"};"
				""
				"A"
		);

		EXPECT_TRUE(type);
		EXPECT_TRUE(type.as<TagTypePtr>()->isRecursive());
		EXPECT_FALSE(isSerializable(type));
		EXPECT_FALSE(detail::tryMakeSerializable(type));
	}

	TEST(AutoSerialization, InterceptedParent) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = frontend::parseType(mgr,
			"#include <vector>",
			"struct B : public std::vector<int> {"
			"	float b;"
			"}"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_FALSE(isSerializable(type)) << *type;

		// but it should be possible to make it serializable
		auto mod = addAutoSerializationCode(type).as<core::TypePtr>();
		EXPECT_PRED1(isSerializable, mod);
		assert_correct_ir(mod);
	}

	TEST(AutoSerialization, SingleParent) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = frontend::parseType(mgr,
			"struct A { "
			"	int a;"
			"};"
			"struct B : public A {"
			"	float b;"
			"}"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_FALSE(isSerializable(type)) << *type;

		// but it should be possible to make it serializable
		auto mod = addAutoSerializationCode(type).as<core::TypePtr>();
		EXPECT_PRED1(isSerializable, mod);
		assert_correct_ir(mod);
	}

	TEST(AutoSerialization, Parents) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = frontend::parseType(mgr,
			"struct A1 { "
			"	int a;"
			"};"
			"struct A2 { "
			"	bool a2;"
			"};"
			"struct A3 { "
			"	double a3;"
			"};"
			"struct B : public A1, A2, A3 {"
			"	float b;"
			"}"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_FALSE(isSerializable(type)) << *type;

		// but it should be possible to make it serializable
		auto mod = addAutoSerializationCode(type).as<core::TypePtr>();
		EXPECT_PRED1(isSerializable, mod);
		assert_correct_ir(mod);
	}

	TEST(AutoSerialization, NestedParent) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		TypePtr type;
		type = frontend::parseType(mgr,
			"struct A1 { "
			"	int a;"
			"};"
			"struct A2 : public A1 { "
			"	bool a2;"
			"};"
			"struct B : public A2 {"
			"	float b;"
			"}"
		);
		EXPECT_TRUE(type);
		assert_correct_ir(type);

		// this one should not be serializable
		EXPECT_FALSE(isSerializable(type)) << *type;

		// but it should be possible to make it serializable
		auto mod = addAutoSerializationCode(type).as<core::TypePtr>();
		EXPECT_PRED1(isSerializable, mod);
		assert_correct_ir(mod);
	}


	// Helper class for passing a string directly to "compileTo"

	class StringTargetCode : public insieme::backend::TargetCode {

		string str;

	public:
		StringTargetCode(const string& str) : insieme::backend::TargetCode(nullptr), str(str) {
		}

		virtual std::ostream& printTo(std::ostream& out) const override {
			return out << str;
		}
	};

	TEST(AutoSerialization, Correctness) {

		// Tests correctness of autoserialization
		// First compile code fragment using classes which need their serialization code generated
		// Then add code for testing the serialization and compiles the result
		// Result code of the execution of the program is 0 if serialization/deserialization is correct

		NodeManager mgr;
		auto prog = frontend::parseCode(mgr, R"(
			struct A1 {
				int a;
			};
			struct A2 : public A1 {
				bool a2;
			};
			struct C {
				double c;
			};
			struct B : public A2, public C {
				float b;
			};

			int main() {
				B b;
			}
		)");

		// generate output code string
		auto result = allscale::compiler::core::convert(prog);
		assert_true(result.result) << "Conversion of input code failed in the core:\n" << result.report;
		auto targetCode = backend::convert(result.result);
		auto targetCodeString = toString(*targetCode);

		// change output code to add serialization testing
		boost::algorithm::replace_first(targetCodeString, "int32_t main(", "int32_t gen_main(");
		targetCodeString += R"(

		int main() {

			IMP_B b;
			b.a = 42;
			b.a2 = true;
			b.b = 1.3f;
			b.c = 2.6;

			auto archive = allscale::utils::serialize(b);
			IMP_B bl = allscale::utils::deserialize<IMP_B>(archive);

			return !(bl.a == b.a && bl.a2 == b.a2 && bl.b == b.b && bl.c == b.c);
		}
		)";

		// we want to test non-HPX serialization
		targetCodeString = "#undef ALLSCALE_WITH_HPX" + targetCodeString;

		//std::cout << "Output code:\n" << targetCodeString << "\n===\n";

		// compile to temporary
		auto newTargetCode = std::make_shared<StringTargetCode>(targetCodeString);
		auto tmp = boost::filesystem::unique_path(boost::filesystem::temp_directory_path() / "allscale-trg-%%%%%%%%");
		bool res = backend::compileTo(newTargetCode, tmp);

		ASSERT_TRUE(res) << "Failed to compile modified source";

		// execute and check result

		auto ret = system(tmp.c_str());
		EXPECT_EQ(ret, 0) << "Deserialization result not equal to original";

		// delete the temporary
		boost::filesystem::remove(tmp);
	}

} // end namespace core
} // end namespace compiler
} // end namespace allscale
