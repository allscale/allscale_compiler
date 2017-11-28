#include "allscale_analysis.h"

#include <vector>

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::user::data;

int f(int x) {
	return x;
}

void read(const int& x) {
	if (x > 12) {};
}

void read2(int& x) {
	if (x > 12) {};
}


void write(int& x) {
	x = 2;
}

int main() {

	Grid<int,2> grid({10,20});

	// using the value in a declaration should constitute a read access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		int x = grid[{4,5}];
	}

	// passing the value to a function by value should cause a read dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		f(grid[{4,5}]);
	}

	// passing the reference to a function reading the value should cause a read dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		read(grid[{4,5}]);
	}

	// also if passed by non-const reference but only read, it should remain a read access only
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		read2(grid[{4,5}]);
	}

	// passing the value to a function with write access should cause a write dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		write(grid[{4,5}]);
	}

	return 0;
}
