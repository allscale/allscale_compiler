
#include "intercepted/data_item_sema.h"

int main() {

	#pragma test expect_ir(R"({
	})")
	{
		SimplestDI di;
		di.get();
	}

	{
		SimplestDI di;
		di[0] = 0;
	}

	{
		SimplestDI di;
		di.someOtherMethod();
	}

}
