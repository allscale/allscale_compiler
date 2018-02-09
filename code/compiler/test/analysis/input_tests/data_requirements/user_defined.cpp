#include "allscale_analysis.h"

#include <vector>

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::core::sema;
using namespace allscale::api::user::data;

int main() {

	Grid<int,2> grid({10,20});

	// start with empty requirements
	{
		cba_expect_data_requirements("{}");
	}

	// let the user define a read requirement
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize ] RO }}");
		needs_read_access(grid,GridRegion<2>::single({4,5}));
	}

	// let the user define a write requirement
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize ] RW }}");
		needs_write_access(grid,GridRegion<2>::single({4,5}));
	}

	// test multiple requirements
	{
		cba_expect_data_requirements("{"
				"Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(2, type_lit(int<8>)), num_cast(3, type_lit(int<8>))), type_lit(cpp_ref))) materialize ] RO },"
				"Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize ] RW }"
			"}");
		needs_read_access(grid,GridRegion<2>::single({2,3}));
		needs_write_access(grid,GridRegion<2>::single({4,5}));
	}

	// test that a user defined requirement overrides actual requirement
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize ] RW }}");
		needs_write_access(grid,GridRegion<2>::single({4,5}));
		grid[{2,3}] = 10;
	}

	// test that the no-more-dependencies clause is accepted
	{
		cba_expect_data_requirements("{}");
		no_more_dependencies();
		grid[{2,3}] = 10;
	}

	// test that no-more-requirements works together with user defined requirements
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize ] RW }}");
		needs_write_access(grid,GridRegion<2>::single({4,5}));
		no_more_dependencies();
		grid[{2,3}] = 10;
	}


	return 0;
}
