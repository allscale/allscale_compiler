#include <allscale/api/user/operator/pfor.h>
#include <allscale/utils/vector.h>

using namespace allscale::api::user;

int main() {

	const int N = 5000000;
	const int T = 100;

	using Point = allscale::utils::Vector<int,2>;

	Point size = { N, N };

	std::array<std::array<int, N>, N> bufferA;
	std::array<std::array<int, N>, N> bufferB;

	auto* A = &bufferA;
	auto* B = &bufferB;

	auto ref = pfor(size, [A,B](const Point& p) {
		(*A)[p.x][p.y] = 0;
		(*B)[p.x][p.y] = -1;
	});

	for (int t = 0; t < T; ++t) {
		ref = pfor(Point { 1, 1 }, Point { N - 1, N - 1 }, [A,B,t](const Point& p) {
			if((*A)[p.x][p.y] != t) {
				std::cout << p.x << std::endl;
			}
			if((*B)[p.x][p.y] != t - 1) {
				std::cout << p.x << std::endl;
			}
			(*B)[p.x][p.y]=t+1;
		}, one_on_one(ref));
		std::swap(A, B);
	}
	pfor(Point { 1, 1 }, Point { N - 1, N - 1 }, [T,A](const Point& p) {
		if((*A)[p.x][p.y] != T) {
			std::cout << p.x << std::endl;
		}
	}, one_on_one(ref));

	return 0;
}
