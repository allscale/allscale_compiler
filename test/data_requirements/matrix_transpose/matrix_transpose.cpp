
#include <chrono>

#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/static_grid.h>

const int N = 1000;

using namespace allscale::api::user::data;
using namespace allscale::api::user::algorithm;

using Element = double;
using Matrix = StaticGrid<Element,N,N>;
using Point = typename Matrix::coordinate_type;

using myClock = std::chrono::high_resolution_clock;

int main() {

	// create two matrixes
	Matrix A;
	Matrix B;

	auto start = myClock::now();

	// perform transformation
	pfor(A.size(),[&](const Point& p) {
		B[{p.y,p.x}] = A[p];
		A[p]++;
	});

	auto end = myClock::now();

	auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - start).count();
	std::cerr << "Completed in " << ms << "ms\n";
	std::cerr << "Throughput: " << ((sizeof(Element)*N*N)/(double)ms/(1024*1024)) << "MiB/s\n";

	return EXIT_SUCCESS;
}
