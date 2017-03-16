#include <allscale/api/user/operator/pfor.h>

using namespace allscale::api::user;

int main() {

    const int N = 100;
    std::vector<int> a(N);
    std::vector<int> b(N);

    // initialize first vector
    pfor(a,[](int& x){
        x = 1;
    });

    // initialize second vector
    pfor(b,[](int& x){
        x = 1;
    });

    // sum up a and b
    std::vector<int> c(N);
    pfor(0,N,[&](int i){
        c[i] = a[i] + b[i];
    });

    // print the result
    for(const auto& cur : c) {
        std::cout << cur << "\n";
    }

	return 0;
}
