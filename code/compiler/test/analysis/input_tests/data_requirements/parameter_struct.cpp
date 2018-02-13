#include "allscale_analysis.h"
#include "data_requirements/intercepted/dummy_array_item.h"

struct Parameters {
	DummyArrayItem X;
	int a, b;
};

int main() {


	Parameters p;

	// start with a scalar
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0.X, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref))] RW }"
			"}");

		// fabricate an artificial access
		p.X[0] = 5;
	}

	// also reading should work
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0.X, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref))] RO }"
			"}");

		// fabricate an artificial access
		int tmp = p.X[0];
	}

	// let's try using a more complex index
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0.X, type_lit(cpp_ref))[ref_kind_cast(ref_temp_init(*v0.a), type_lit(cpp_ref))] RO }"
			"}");

		// fabricate an artificial access
		int tmp = p.X[p.a];
	}

	// test some loop over elements (read only)
	{
		cba_expect_data_requirements("{}");

		// fabricate an artificial access
		int sum = 0;
		for(int i=p.a; i<p.b; i++) {
//			sum += p.X[i];
		}
	}

	// test some loop over elements (read only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0.X, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v0.a), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v0.b-1u), type_lit(cpp_ref)))] RO }"
			"}");

		// fabricate an artificial access
		int sum = 0;
		int from = p.a;		// TODO: remove this once while-to-for is fixed for this use case
		int to = p.b;
		for(int i=from; i<to; i++) {
			sum += p.X[i];
		}
	}

	// test some loop over elements (read only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0.X, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v0.a), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v0.b-1u), type_lit(cpp_ref)))] RW }"
			"}");

		// fabricate an artificial access
		int sum = 0;
		int from = p.a;		// TODO: remove this once while-to-for is fixed for this use case
		int to = p.b;
		for(int i=from; i<to; i++) {
			p.X[i] = 1;
		}
	}

	// test some loop over elements (read only)
	{
		cba_expect_data_requirements("{"
				"Requirement { ref_kind_cast(v0.X, type_lit(cpp_ref))[span(ref_kind_cast(ref_temp_init(*v0.a), type_lit(cpp_ref)),ref_kind_cast(ref_temp_init(*v0.b-1u), type_lit(cpp_ref)))] RW }"
			"}");

		// fabricate an artificial access
		int sum = 0;
		int from = p.a;		// TODO: remove this once while-to-for is fixed for this use case
		int to = p.b;
		for(int i=from; i<to; i++) {
			p.X[i]++;
		}
	}

	return 0;
}
