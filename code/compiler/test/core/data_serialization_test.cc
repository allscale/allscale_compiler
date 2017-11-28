#include <gtest/gtest.h>

#include <algorithm>
#include <iostream>

#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"

#include "allscale/compiler/core/data_serialization.h"
#include "allscale/compiler/backend/allscale_extension.h"

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

		// check standard library types
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::string"));
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::pair<int,bool>"));
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::array<int,3>"));
		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::vector<int>"));

		EXPECT_PRED1(isSerializable, frontend::parseType(mgr,"std::vector<std::pair<std::array<int,3>,std::string>>"));

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
		EXPECT_EQ(basic.getBool(), tryMakeSerializable(basic.getBool()));

		EXPECT_EQ(basic.getInt1(), tryMakeSerializable(basic.getInt1()));
		EXPECT_EQ(basic.getInt2(), tryMakeSerializable(basic.getInt2()));
		EXPECT_EQ(basic.getInt4(), tryMakeSerializable(basic.getInt4()));
		EXPECT_EQ(basic.getInt8(), tryMakeSerializable(basic.getInt8()));

		EXPECT_EQ(basic.getUInt1(), tryMakeSerializable(basic.getUInt1()));
		EXPECT_EQ(basic.getUInt2(), tryMakeSerializable(basic.getUInt2()));
		EXPECT_EQ(basic.getUInt4(), tryMakeSerializable(basic.getUInt4()));
		EXPECT_EQ(basic.getUInt8(), tryMakeSerializable(basic.getUInt8()));

	}

	TEST(AutoSerialization, ReferenceAndPointers) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		EXPECT_FALSE(tryMakeSerializable(builder.parseType("ref<int<4>>")));
		EXPECT_FALSE(tryMakeSerializable(builder.parseType("ref<int<4>,t,f,plain>")));
		EXPECT_FALSE(tryMakeSerializable(builder.parseType("ref<int<4>,f,f,cpp_ref>")));
		EXPECT_FALSE(tryMakeSerializable(builder.parseType("ref<int<4>,f,t,cpp_rref>")));

		EXPECT_FALSE(tryMakeSerializable(builder.parseType("ptr<int<4>>")));
		EXPECT_FALSE(tryMakeSerializable(builder.parseType("ptr<int<4>,t,f>")));

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
		auto mod = tryMakeSerializable(type);
		EXPECT_TRUE(mod);
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
		EXPECT_EQ(type,tryMakeSerializable(type));
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
		EXPECT_FALSE(tryMakeSerializable(type));

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
		EXPECT_FALSE(tryMakeSerializable(type));
	}


} // end namespace core
} // end namespace compiler
} // end namespace allscale
