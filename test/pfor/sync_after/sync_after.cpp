#include <allscale/api/user/operator/pfor.h>

using namespace allscale::api::user;

int main() {

	const int N = 10000000;
	const int T = 100;

	int* A = new int[N];
	int* B = new int[N];

	auto ref = pfor(0, N, [A,B](int i) {
		A[i] = 0;
		B[i] = -1;
	});

	int counter = 0;

	for (int t = 0; t < T; ++t) {
		ref = pfor(1, N - 1, [A,B,t,N](int i) {

			if(i != 1 && A[i - 1] != t) {
				std::cout << i << std::endl;
			}
			if(A[i] != t) {
				std::cout << i << std::endl;
			}
			if(i != N - 2 && A[i + 1] != t) {
				std::cout << i << std::endl;
			}

			if(B[i] != t - 1) {
				std::cout << i << std::endl;
			}

			B[i] = t + 1;

		}, neighborhood_sync(ref));

		if (t % 2 == 0) {
			ref = after(ref, N / 2, [B, t, &counter, N] {
				if(B[N / 2] != t + 1) {
					std::cout << t << std::endl;
				}
				counter++;
			});
		}

		std::swap(A, B);
	}

	pfor(1, N - 1, [A](int i) {
		if(A[i] != T) {
			std::cout << T << std::endl;
		}
	}, neighborhood_sync(ref));

	if(T / 2 != counter) {
		std::cout << T << std::endl;
	}

	delete[] A;
	delete[] B;

	return 0;
}
