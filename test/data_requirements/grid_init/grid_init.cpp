#include <allscale/api/user/operator/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user;
using namespace allscale::api::user::data;

int main() {

    const int N = 1000;

    Grid<int> data(N); 

    // initialize first vector
    pfor(0,N,[&](int i){
        data[i] = 1;
    });

	return 0;
}
