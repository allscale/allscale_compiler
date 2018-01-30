#include "allscale_analysis.h"

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::user::data;

int main() {

	Grid<int,1> grid(10);

	// test a read requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_1::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_1)), ref_kind_cast(IMP_std_colon__colon_initializer_list_long::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_long)), ref_kind_cast(ref_temp_init(num_cast(4, type_lit(int<8>))), type_lit(cpp_ref))), type_lit(cpp_ref))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		int x = grid[{4}];
	}

	// test a write requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_1::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_1)), ref_kind_cast(IMP_std_colon__colon_initializer_list_long::(ref_temp(type_lit(IMP_std_colon__colon_initializer_list_long)), ref_kind_cast(ref_temp_init(num_cast(4, type_lit(int<8>))), type_lit(cpp_ref))), type_lit(cpp_ref))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		grid[{4}] = 5;
	}

	return 0;
}
