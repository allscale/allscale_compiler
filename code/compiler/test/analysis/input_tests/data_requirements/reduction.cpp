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

	// start with the basics
	{
		cba_expect_data_requirements("{}");

		// some simple range to process - no requirements
		range<int>(0,5).forEach([](int x){});
	}

	// test the same with vectors
	{
		cba_expect_data_requirements("{}");

		// some simple range to process - no requirements
		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([](const Vec2& x){});
	}

	// test an actual read access (with a constant index)
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_cast(ref_kind_cast(v0, type_lit(cpp_ref)), type_lit(f), type_lit(f), type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref))] RO }}");

		// some simple range to process - no requirements
		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
			int a = X[10];
		});
	}

	// test an actual write access (with a constant index)
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_cast(ref_kind_cast(v0, type_lit(cpp_ref)), type_lit(f), type_lit(f), type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref))] RW }}");

		// some simple range to process - no requirements
		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
			X[10] = 10;
		});
	}

	// test an actual read access (with a dependent index)
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_cast(ref_kind_cast(v0, type_lit(cpp_ref)), type_lit(f), type_lit(f), type_lit(plain)), type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_::(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_))).IMP__operator_call_(0, 0) materialize .x), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_::(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_))).IMP__operator_call_(10-1u, 10-1u) materialize .x), type_lit(cpp_ref)))] RO }}");

		// some simple range to process - no requirements
		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
			int a = X[x.x];
		});
	}

	// test an actual write access (with a dependent index)
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_cast(ref_kind_cast(v0, type_lit(cpp_ref)), type_lit(f), type_lit(f), type_lit(plain)), type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_::(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_))).IMP__operator_call_(0, 0) materialize .x), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_::(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_algorithm_colon__colon_detail_colon__colon_point_factory_allscale_colon__colon_utils_colon__colon_Vector_lt_int_comma__space_2_gt_))).IMP__operator_call_(10-1u, 10-1u) materialize .x), type_lit(cpp_ref)))] RW }}");

		// some simple range to process - no requirements
		range<Vec2>(Vec2(0,0),Vec2(10,10)).forEach([&](const Vec2& x){
			X[x.x] = 10;
		});
	}


	return 0;
}
