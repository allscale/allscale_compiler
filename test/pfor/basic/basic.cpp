
#include "allscale/api/user/operator/pfor.h"

using namespace allscale::api::user;

int main() {

	const int N = 10;
	std::vector<int> a(N,0);

	pfor(a,[](int x){
		x;
	});
	std::cout << "done" << std::endl;

	return 0;
}
