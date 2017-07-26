#include "intercepted/dummy_array_item.h"

#include "allscale_analysis.h"

int main() {

	DummyArrayItem X;
	int data;

	// test some request, but no access
	{
		cba_expect_data_requirements("{}");

		// fabricate an artificial access
		auto& ref = X.get(3);
	}

	// test some read access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(5), type_lit(cpp_ref))] RO }}");

		// fabricate an artificial access
		auto& ref = X.get(5);

		// this is the actual read
		int x = ref;
	}

	// test some write access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(7), type_lit(cpp_ref))] RW }}");

		// fabricate an artificial access
		auto& ref = X.get(7);

		// this is the actual write
		ref = 12;
	}

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RO }}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=0; i<10; i++) {
			sum += X.get(i);
		}
	}

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RW }}");

		// fabricate an artificial access
		for(int i=0; i<10; i++) {
			X.get(i) = 1;
		}
	}

	return 0;
}
