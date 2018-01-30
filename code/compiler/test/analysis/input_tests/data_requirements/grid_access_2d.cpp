#include "allscale_analysis.h"

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::user::data;

int main() {

	Grid<int,2> grid({10,20});

	// test a read requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		int x = grid[{4,5}];
	}

	// test a write requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		grid[{4,5}] = 5;
	}

	return 0;
}
