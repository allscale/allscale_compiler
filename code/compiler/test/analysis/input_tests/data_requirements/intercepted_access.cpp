#include "allscale_analysis.h"

#include <vector>

#include "allscale/api/user/data/grid.h"

using namespace allscale::api::user::data;

int main() {

	using Elements = std::vector<int>;

	Grid<Elements,2> grid({10,20});

	// test a direct read requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		Elements x = grid[{4,5}];
	}

	// test a direct write requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		grid[{4,5}] = Elements();
	}

	// test an indirect read requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		grid[{4,5}].size();
	}

	// test an indirect write requirement
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(instantiate(target_type, IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single)(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_2)), 4, 5), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		grid[{4,5}].push_back(12);
	}


	return 0;
}
