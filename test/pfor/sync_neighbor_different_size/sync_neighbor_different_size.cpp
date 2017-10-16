#include <allscale/api/user/algorithm/pfor.h>

using namespace allscale::api::user::algorithm;

int main() {

	const int N = 10000000;

	std::vector<int> dataA(N + 20);
	std::vector<int> dataB(N + 20);

	auto As = pfor(0, N, [&](int i) {
		dataA[i] = 1;
	});

	auto Bs = pfor(0, N - 1, [&](int i) {
		if ((i != 0 && dataA[i - 1] != 1) || dataA[i] != 1 || dataA[i + 1] != 1) {
			std::cout << i << std::endl;
		}

		dataB[i] = 2;
	}, neighborhood_sync(As));

	auto Cs = pfor(0, N - 2, [&](int i) {
		if ((i != 0 && dataB[i - 1] != 2) || dataB[i] != 2 || dataB[i + 1] != 2) {
			std::cout << i << std::endl;
		}
		dataA[i] = 3;
	}, neighborhood_sync(Bs));

	auto Ds = pfor(0, N + 20, [&](int i) {

		if (i != 0 && i <= N-2 ) {
			if(dataA[i - 1] != 3) {
				std::cout << i << std::endl;
			}
		} else if ( i != 0 && i < N ) {
			if(dataA[i - 1] != 1) {
				std::cout << i << std::endl;
			}
		}

		if (i < N-2) {
			if(dataA[i] != 3) {
				std::cout << i << std::endl;
			}
		} else if (i < N) {
			if(dataA[i] != 1) {
				std::cout << i << std::endl;
			}
		}

		if (i != N-1 && i < N-3) {
			if(dataA[i + 1] != 3) {
				std::cout << i << std::endl;
			}
		} else if (i != N-1 && i < N) {
			if(dataA[i + 1] != 1) {
				std::cout << i << std::endl;
			}
		}

		dataB[i] = 4;

	}, neighborhood_sync(Cs));

	Ds.wait();

	for (int i = 0; i < N - 2; i++) {
		if(dataA[i] != 3) {
			std::cout << i << std::endl;
		}
	}
	for (int i = N - 2; i < N - 1; i++) {
		if(dataA[i] != 1) {
			std::cout << i << std::endl;
		}
	}
	for (int i = 0; i < N + 20; i++) {
		if(dataB[i] != 4) {
			std::cout << i << std::endl;
		}
	}

	return 0;
}
