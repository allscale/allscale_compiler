#include <allscale/api/user/algorithm/pfor.h>

using namespace allscale::api::user::algorithm;

int main() {

    const int N = 100;
    std::vector<int> a(N,0);

    // increment each element by one
    pfor(a,[](int& x){
        x++;
    });

    // test new state
    for(const auto& cur : a) {
        if (cur != 1) {
            std::cout << "Invalid value: " << cur << "\n";
            return 1;
        }
    }

	return 0;
}
