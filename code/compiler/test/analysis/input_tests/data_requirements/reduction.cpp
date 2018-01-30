#include "allscale_analysis.h"
#include "data_requirements/intercepted/dummy_array_item.h"

#define NDEBUG

#include "allscale/api/user/algorithm/preduce.h"
#include "allscale/utils/vector.h"

using namespace allscale::api::user::algorithm;
using namespace allscale::utils;

using allscale::api::user::algorithm::detail::range;

int main() {

	using Vec2 = Vector<int,2>;
	using Vec3 = Vector<int,3>;

	DummyArrayItem X;

	// -- constant boundaries --

//	// start with the basics
//	{
//		cba_expect_data_requirements("{}");
//
//		// some simple range to process - no requirements
//		range<int>(0,5).forEach([](int x){});
//	}
//
//	// test the same with vectors
//	{
//		cba_expect_data_requirements("{}");
//
//		// some simple range to process - no requirements
//		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([](const Vec2& x){});
//	}

	// test an actual read access (with a constant index)
	{
		cba_expect_data_requirements("{something}");

		// some simple range to process - no requirements
		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
			int a = X[10];
		});

		cba_debug_requirements();
	}


//	// test an actual write access (with a constant index)
//	{
//		cba_expect_data_requirements("{something}");
//
//		// some simple range to process - no requirements
//		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
//			X[10] = 10;
//		});
//
////		cba_debug_requirements();
//	}
//
//	// test an actual read access (with a dependent index)
//	{
//		cba_expect_data_requirements("{something}");
//
//		// some simple range to process - no requirements
//		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
//			int a = X[x.x];
//		});
//
////		cba_debug_requirements();
//	}
//
//	// test an actual write access (with a dependent index)
//	{
//		cba_expect_data_requirements("{something}");
//
//		// some simple range to process - no requirements
//		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
//			X[x.x] = 10;
//		});
//
////		cba_debug_requirements();
//	}


	return 0;
}
