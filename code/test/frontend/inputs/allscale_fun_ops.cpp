
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// fun construction

	#pragma test expect_ir(R"({
	})")
	{
		fun([](unsigned x) { return x<=2; },
		    [](unsigned x) { return x; },
		    [](unsigned x, auto f) { return f(x-1); });
	}

	return 0;
}
