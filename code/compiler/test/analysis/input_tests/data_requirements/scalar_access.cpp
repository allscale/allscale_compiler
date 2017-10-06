#include "allscale_analysis.h"

#include "allscale/api/user/data/scalar.h"

using namespace allscale::api::user::data;

int main() {

	Scalar<int> data;

	// test a read requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_detail_colon__colon_ScalarRegion::ctor(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_detail_colon__colon_ScalarRegion)), true), type_lit(cpp_ref))] RO }}");
		int x = data.get();
	}

	// TODO: known bug, frontend is not providing an implementation for the set operation!
//	// test a write requirement
//	{
//		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_detail_colon__colon_ScalarRegion::ctor(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_detail_colon__colon_ScalarRegion)), true), type_lit(cpp_ref))] RW }}");
//		data.set(5);
//
//		cba_debug_requirements();
//	}

	// test a indirect requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_detail_colon__colon_ScalarRegion::ctor(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_detail_colon__colon_ScalarRegion)), true), type_lit(cpp_ref))] RW }}");
		data.get() = 5;
	}

	return 0;
}
