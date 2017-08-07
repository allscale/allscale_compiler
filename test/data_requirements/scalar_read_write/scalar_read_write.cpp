
#include "allscale/api/user/operator/async.h"
#include "allscale/api/user/data/scalar.h"

using namespace allscale::api::user;
using namespace allscale::api::user::data;

int main() {

    Scalar<int> a;

    // this task should a read-only data dependency
	async([&]{ 
        a.get() = 12;
    }).wait();

	return 0;
}
