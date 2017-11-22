#include "allscale_analysis.h"

#include <vector>

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::user::data;

int main() {

	Grid<int,2> grid({10,20});

	// just taking a reference should not cause a requirement
	{
		cba_expect_data_requirements("{}");
		int& x = grid[{4,5}];
	}

	// taking a reference and reading from it should produce a read only requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		int& x = grid[{4,5}];
		x + 2;
	}

	// taking a reference and writing to it should cause a write dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		int& x = grid[{4,5}];
		x = 12;
	}

	return 0;
}
