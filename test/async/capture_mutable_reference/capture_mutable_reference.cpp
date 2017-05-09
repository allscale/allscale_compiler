
#include "allscale/api/user/operator/async.h"

using namespace allscale::api::user;

int main() {

    int x = 10;

	auto job = async([&x]{ 
        x = x + 1; 
        return x; 
    });
	std::cout << job.get() << std::endl;
	std::cout << x << std::endl;

	return 0;
}
