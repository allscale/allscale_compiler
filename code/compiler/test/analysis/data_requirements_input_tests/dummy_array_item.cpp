#include "intercepted/dummy_array_item.h"

#include "allscale_analysis.h"

int main() {

	DummyArrayItem X;
	int x = 12;
	int a = 3;
	int b = 10;

	// -- scalar based access --

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


	// -- variable scalar based access --

	// test some request, but no access
	{
		cba_expect_data_requirements("{}");

		// fabricate an artificial access
		auto& ref = X.get(x);
	}

	// test some read access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(*v1), type_lit(cpp_ref))] RO }}");

		// fabricate an artificial access
		auto& ref = X.get(x);

		// this is the actual read
		int x = ref;
	}

	// test some write access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(*v1), type_lit(cpp_ref))] RW }}");

		// fabricate an artificial access
		auto& ref = X.get(x);

		// this is the actual write
		ref = 12;
	}


	// -- loop based access --

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

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RO }}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=0; i<10; i++) {
			sum += X[i];
		}
	}

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(10), type_lit(cpp_ref)))] RW }}");

		// fabricate an artificial access
		for(int i=0; i<10; i++) {
			X[i] = 1;
		}
	}


	// -- variable loop based access --

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v3), type_lit(cpp_ref)))] RO }}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=a; i<b; i++) {
			sum += X.get(i);
		}
	}

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v3), type_lit(cpp_ref)))] RW }}");

		// fabricate an artificial access
		for(int i=a; i<b; i++) {
			X.get(i) = 1;
		}
	}

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v3), type_lit(cpp_ref)))] RO }}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=a; i<b; i++) {
			sum += X[i];
		}
	}

	// test some loop over elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v2), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v3), type_lit(cpp_ref)))] RW }}");

		// fabricate an artificial access
		for(int i=a; i<b; i++) {
			X[i] = 1;
		}
	}

	return 0;
}
