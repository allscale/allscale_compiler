#include "allscale/api/user/data/scalar.h"

#include "allscale_analysis.h"

using namespace allscale::api::user::data;

int main() {

	Scalar<int> s;

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");

	}

	// test a single read requirement
	{
		cba_expect_data_requirements("{}");
		int x = s.get();
	}

	// test a single write requirement
	{
		cba_expect_data_requirements("{}");
		s.set(12);
	}

	// test a read/write requirement
	{
		cba_expect_data_requirements("{}");
		s.set(s.get() + 2);
	}

	return 0;
}
