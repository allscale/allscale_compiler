

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({ var ref<int<4>,f,f,plain> v0 = 42; })")
	{
		int i = 42;
	}

	return 0;
}
