#include "allscale_analysis.h"

int main() {

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");
		// just see whether nothing has no requirements
	}

	return 0;
}
