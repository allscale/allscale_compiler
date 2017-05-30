#include "allscale/cba/dummy.h"

extern "C" {
	void hat_foo(void);
}

namespace allscale {
namespace cba {

	void foo(void) {
		hat_foo();
	}

}
}
