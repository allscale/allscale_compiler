#include "allscale_analysis.h"

#include "allscale/api/core/data.h"

int main() {

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");
		// just see whether nothing has no requirements
		// TODO: add a data reference access
	}

	return 0;
}
