#include "allscale_analysis.h"
#include "data_requirements/intercepted/dummy_array_item.h"

int some_source();

int main() {

	DummyArrayItem X;

	int a = some_source();
	int b = some_source();

	// -- constant boundaries --

	// test some loop over elements (read only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RO }"
			"}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=0; i<10; i++) {
			sum += X[i];
		}
	}

	// test some loop over elements (write only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RW }"
			"}");

		// fabricate an artificial access
		for(int i=0; i<10; i++) {
			X[i] = 1;
		}
	}

	// test some loop over elements (read / write)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RO },"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RW }"
			"}");

		// fabricate an artificial access
		for(int i=0; i<10; i++) {
			X[i]++;
		}
	}

	// -- variable boundaries --

	// test some loop over elements (read only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v1), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)))] RO }"
			"}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=a; i<b; i++) {
			sum += X[i];
		}
	}

	// test some loop over elements (write only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v1), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)))] RW }"
			"}");

		// fabricate an artificial access
		for(int i=a; i<b; i++) {
			X[i] = 1;
		}
	}

	// test some loop over elements (read / write)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v1), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)))] RO },"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v1), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)))] RW }"
			"}");

		// fabricate an artificial access
		for(int i=a; i<b; i++) {
			X[i]++;
		}
	}

	return 0;
}
