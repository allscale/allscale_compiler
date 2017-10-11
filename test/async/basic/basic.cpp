
#include "allscale/api/user/algorithm/async.h"

using namespace allscale::api::user::algorithm;

int main() {

	auto job = async([]{ return 12; });
	std::cout << job.get() << std::endl;

	return 0;
}
