
#include "allscale/api/user/algorithm/async.h"

using namespace allscale::api::user::algorithm;

int main() {

    int x = 12;

	auto job = async([&x]{ return x; });
	std::cout << job.get() << std::endl;

	return 0;
}
