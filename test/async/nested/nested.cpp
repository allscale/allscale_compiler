
#include "allscale/api/user/algorithm/async.h"

using namespace allscale::api::user::algorithm;

int main() {

    int x = 10;

    // let's try to nest two async calls

	auto outer = async([&x]{ 

        auto inner = async([&x]{
            
            x = x + 1; 
            return x; 

        });

        return inner.get();
    });

	std::cout << outer.get() << std::endl;
	std::cout << x << std::endl;

	return 0;
}
