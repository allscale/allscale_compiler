#include <allscale/api/user/operator/pfor.h>

using namespace allscale::api::user;

int main() {

	const int N = 10000000;
	std::vector<int> data(N);

	auto As = pfor(0, N, [&](int i) {
		data[i] = 0;
	});

	auto Bs = pfor(0, N, [&](int i) {
		if(data[i] != 0) {
			std::cout << i << std::endl;
		}
		data[i] = 1;
	}, one_on_one(As));

	auto Cs = pfor(0, N, [&](int i) {
		if(data[i] != 1) {
			std::cout << i << std::endl;
		}
		data[i] = 2;
	}, one_on_one(Bs));

	Cs.wait();

	for(int i = 0; i < N; i++) {
		if(data[i] != 2) {
			std::cout << i << std::endl;
		}
	}

	return 0;
}
