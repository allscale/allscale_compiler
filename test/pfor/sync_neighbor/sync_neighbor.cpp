#include <allscale/api/user/algorithm/pfor.h>

using namespace allscale::api::user::algorithm;

int main() {

	const int N = 10000000;

	std::vector<int> dataA(N);
	std::vector<int> dataB(N);

	auto As = pfor(0, N, [&](int i) {
		dataA[i] = 1;
	});

	auto Bs = pfor(0, N, [&](int i) {
		if ((i != 0 && dataA[i - 1] != 1) || dataA[i] != 1 || (i != N - 1 && dataA[i + 1] != 1)) {
			std::cout << i << std::endl;
		}

		dataB[i] = 2;
	}, small_neighborhood_sync(As));

	auto Cs = pfor(0, N, [&](int i) {
		if ((i != 0 && dataB[i - 1] != 2) || dataB[i] != 2 || (i != N - 1 && dataB[i + 1] != 2)) {
			std::cout << i << std::endl;
		}

		dataA[i] = 3;
	}, small_neighborhood_sync(Bs));

	Cs.wait();

	for (int i = 0; i < N; i++) {
		if(dataA[i] != 3 || dataB[i] != 2) {
			std::cout << i << std::endl;
		}
	}

	return 0;
}
