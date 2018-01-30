#include "allscale_analysis.h"

#include <vector>

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::user::data;

using Elements = std::vector<int>;

void f(Elements x) {
}

void read(Elements& x) {
	if (x.size() > 12) {};
}

void write(Elements& x) {
	x.push_back(12);
}

int main() {

	Grid<Elements,2> grid({10,20});

	// using the value in a declaration should constitute a read access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		Elements x = grid[{4,5}];
	}

	// using the value in a declaration with a move constructor should constitute a read write access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		Elements x = static_cast<Elements&&>(grid[{4,5}]);
	}

	// passing the value to a function by value should cause a read dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		f(grid[{4,5}]);
	}

	// passing the reference to a function reading the value should cause a read dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		read(grid[{4,5}]);
	}

	// passing the value to a function with write access should cause a write dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), num_cast(4, type_lit(int<8>)), num_cast(5, type_lit(int<8>))), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		write(grid[{4,5}]);

		cba_debug_requirements();
	}

	return 0;
}
