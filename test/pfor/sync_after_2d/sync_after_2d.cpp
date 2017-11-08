#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/utils/vector.h>

#include <memory>

using namespace allscale::api::user::algorithm;

int main() {
	const int N = 1000;
	const int T = 100;

	using Point = allscale::utils::Vector<int,2>;

	Point size = { N, N };
	Point center = { N / 2, N / 2 };

	auto bufferA = std::make_unique<std::array<std::array<int, N>, N>>();
	auto bufferB = std::make_unique<std::array<std::array<int, N>, N>>();

	auto* A = &*bufferA;
	auto* B = &*bufferB;

	auto ref = pfor(size, [A,B](const Point& p) {
		(*A)[p.x][p.y] = 0;
		(*B)[p.x][p.y] = -1;
	});

	int counter = 0;

	for (int t = 0; t < T; ++t) {
		ref = pfor(Point { 1, 1 }, Point { N - 1, N - 1 }, [A,B,t,N](const Point& p) {
			if (p.x != 1 && p.y != 1 && (*A)[p.x-1][p.y-1] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.y != 1 && (*A)[p.x ][p.y-1] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.x != N-2 && p.y != 1 && (*A)[p.x+1][p.y-1] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.x != N-2 && p.y != 1 && (*A)[p.x+1][p.y-1] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.x != N-2 && (*A)[p.x+1][p.y] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.x != N-2 && p.y != N-2 && (*A)[p.x+1][p.y+1] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.y != 1 && (*A)[p.x][p.y-1] != t) {
				std::cout << p.x << std::endl;
			}
			if (p.y != N-2 && (*A)[p.x][p.y+1] != t) {
				std::cout << p.x << std::endl;
			}
			if((*A)[p.x][p.y] != t) {
				std::cout << p.x << std::endl;
			}
			if((*B)[p.x][p.y] != t - 1) {
				std::cout << p.x << std::endl;
			}

			(*B)[p.x][p.y]=t+1;

		}, full_neighborhood_sync(ref));

		if (t % 2 == 0) {
			ref = after(ref, center, [&,B,t] {
				if((*B)[center.x][center.y] != t + 1) {
					std::cout << t << std::endl;
				}
				counter++;
			});
		}

		std::swap(A, B);
	}

	pfor(Point { 1, 1 }, Point { N - 1, N - 1 }, [A](const Point& p) {
		if((*A)[p.x][p.y] != T) {
			std::cout << p.x << std::endl;
		}
	}, small_neighborhood_sync(ref));

	if(T / 2 != counter) {
		std::cout << counter << std::endl;
	}

	return 0;
}
