#include "allscale_analysis.h"

#include <iostream>

int main() {

	// test some empty requirement
	{
		cba_expect_data_requirements("{}");

		// see that IO can be handled
		std::cout << "Hello\n";
	}

	return 0;
}
