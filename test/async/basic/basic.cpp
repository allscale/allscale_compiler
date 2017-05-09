
#include "allscale/api/user/operator/async.h"

using namespace allscale::api::user;

int main() {

	auto job = async([]{ return 12; });
	std::cout << job.get() << std::endl;

	return 0;
}
