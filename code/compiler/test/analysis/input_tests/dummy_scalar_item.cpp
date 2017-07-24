#include "allscale_analysis.h"

#include "intercepted/dummy_scalar_item.h"

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
		cba_expect_data_requirements("{Requirement { ref_cast(v3, type_lit('c), type_lit('v), type_lit(cpp_ref))[0] RO }}");

		// fabricate an artificial access
		auto& ref = X.get();

		// this is the actual read
		int x = ref;
	}


	// test some write access
	{
		cba_expect_data_requirements("{Requirement { ref_cast(v3, type_lit('c), type_lit('v), type_lit(cpp_ref))[0] RW }}");

		// fabricate an artificial access
		auto& ref = X.get();

		// this is the actual write
		ref = 12;
	}


	return 0;
}
