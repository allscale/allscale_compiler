#include <allscale/api/user/operator/pfor.h>

using namespace allscale::api::user;

int main() {

	const int N = 10000000;
	std::vector<int> data(N + 20);

	auto As = pfor(0, N, [&](int i) {
		data[i] = 0;
	});

	auto Bs = pfor(0, N - 1, [&](int i) {
		if(data[i] != 0) {
			std::cout << i << std::endl;
		}
		data[i] = 1;
	}, one_on_one(As));

	auto Cs = pfor(0, N - 2, [&](int i) {
		if(data[i] != 1) {
			std::cout << i << std::endl;
		}
		data[i] = 2;
	}, one_on_one(Bs));

	auto Ds = pfor(0, N + 20, [&](int i) {
		if(i < N - 2) {
			if(data[i] != 2) {
				std::cout << i << std::endl;
			}
		}
		else if(i < N - 1) {
			if(data[i] != 1) {
				std::cout << i << std::endl;
			}
		}
		else if(i < N) {
			if(data[i] != 0) {
				std::cout << i << std::endl;
			}
		}
		data[i] = 3;
	}, one_on_one(Cs));

	Ds.wait();

	for (int i = 0; i < N + 20; i++) {
		if (data[i] != 3) {
			std::cout << i << std::endl;
		}
	}

	return 0;
}
