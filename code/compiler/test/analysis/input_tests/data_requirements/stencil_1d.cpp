#include "allscale_analysis.h"
#include "data_requirements/intercepted/dummy_array_item.h"

int main() {

	DummyArrayItem A;
	DummyArrayItem B;

	// test a stencil with fixed boundaries
	{
		cba_expect_data_requirements("{"
				  "Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RO },"
				  "Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0+1), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10+1), type_lit(cpp_ref)))] RO },"
				  "Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0-1), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10-1), type_lit(cpp_ref)))] RO },"
				  "Requirement { ref_kind_cast(v1, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RW }"
				"}");

		// compute a 1D stencil
		for(int i = 0; i<10; i++) {
			B[i] = A[i] + 4*(A[i-1] + A[i+1] - 2*A[i]);
		}

	}


	return 0;
}
