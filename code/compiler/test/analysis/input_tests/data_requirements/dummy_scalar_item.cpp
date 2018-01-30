#include "allscale_analysis.h"
#include "data_requirements/intercepted/dummy_scalar_item.h"

int main() {

	DummyScalarItem X;
	int data;

	// test some request, but no access
	{
		cba_expect_data_requirements("{}");

		// fabricate an artificial access
		auto& ref = X.get();
	}

	// test some read access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref))] RO }}");

		// fabricate an artificial access
		auto& ref = X.get();

		// this is the actual read
		int x = ref;
	}


	// test some write access
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref))] RW }}");

		// fabricate an artificial access
		auto& ref = X.get();

		// this is the actual write
		ref = 12;
	}


	return 0;
}
