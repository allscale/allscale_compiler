#include "allscale_analysis.h"
#include "data_requirements/intercepted/dummy_scalar_item.h"

bool some_condition();

int main() {

	DummyScalarItem X;

	// start by testing a simple access
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[0] RO }"
			"}");

		// fabricate an artificial access
		int x = X.get();
	}

	// placing it into a while loop should not have any effect
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[0] RO }"
			"}");

		// fabricate an artificial access
		while(some_condition()) {
			int x = X.get();
		}
	}

	// also the data requirement of the condition should be considered
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[0] RO }"
			"}");

		// fabricate an artificial access
		while(X.get() < 10) {
			// nothing ...
		}
	}

	// also the body should be considered
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[0] RO },"
				"Requirement { ref_kind_cast(v0, type_lit(cpp_ref))[0] RW }"
			"}");

		// fabricate an artificial access
		while(X.get() < 10) {
			X.get()++;
		}
	}

	return 0;
}
