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
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref)))] RO }}");
		needs_read_access(grid,GridRegion<2>::single({4,5}));
	}

	// let the user define a write requirement
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref)))] RW }}");
		needs_write_access(grid,GridRegion<2>::single({4,5}));
	}

	// test multiple requirements
	{
		cba_expect_data_requirements("{"
				"Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(2, type_lit(int<8>)), num_cast(3, type_lit(int<8>))), type_lit(cpp_ref)))] RO },"
				"Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref)))] RW }"
			"}");
		needs_read_access(grid,GridRegion<2>::single({2,3}));
		needs_write_access(grid,GridRegion<2>::single({4,5}));
	}

	// test that a user defined requirement overrides actual requirement
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref)))] RW }}");
		needs_write_access(grid,GridRegion<2>::single({4,5}));
		grid[{2,3}] = 10;
	}

	// test that the no-dependencies clause is accepted
	{
		cba_expect_data_requirements("{}");
		no_dependencies();
		grid[{2,3}] = 10;
	}

	// test that no-requirements works together with user defined requirements
	{
		cba_expect_data_requirements("{Requirement { v0[instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref)))] RW }}");
		needs_write_access(grid,GridRegion<2>::single({4,5}));
		no_dependencies();
		grid[{2,3}] = 10;
	}

	// test indirect read-requirement specification
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(2, type_lit(int<8>)), num_cast(3, type_lit(int<8>))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		needs_read_access_on(grid[{2,3}]);
	}

	// test that no-requirements works together with user defined requirements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(2, type_lit(int<8>)), num_cast(3, type_lit(int<8>))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		needs_write_access_on(grid[{2,3}]);
	}

	return 0;
}
