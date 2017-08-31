#include <allscale/api/user/operator/pfor.h>
#include <allscale/utils/vector.h>

using namespace allscale::api::user;

int main() {

	const int N = 500;
	const int T = 10;

	using Point = allscale::utils::Vector<int,2>;

	Point size = { N, N };

    auto bufferA = new std::array<std::array<int, N>, N>();
    auto bufferB = new std::array<std::array<int, N>, N>();

	auto* A = bufferA;
	auto* B = bufferB;

	auto ref = pfor(size, [A,B](const Point& p) {
		(*A)[p.x][p.y] = 0;
		(*B)[p.x][p.y] = -1;
	});

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
		}, neighborhood_sync(ref));

		std::swap(A, B);
	}

	// check the final state
	pfor(Point { 1, 1 }, Point { N - 1, N - 1 }, [T,A](const Point& p) {
		if((*A)[p.x][p.y] != T) {
			std::cout << p.x << std::endl;
		}
	}, neighborhood_sync(ref));

    delete bufferA;
    delete bufferB;

	return 0;
}
