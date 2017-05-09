
#include "allscale/api/user/operator/async.h"

using namespace allscale::api::user;

int main() {

    int x = 12;

	auto job = async([x]{ return x; });
	std::cout << job.get() << std::endl;

	return 0;
}
