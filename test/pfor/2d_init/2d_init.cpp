
#include "allscale/utils/vector.h"
#include "allscale/api/user/algorithm/pfor.h"

using namespace allscale::api::user::algorithm;
using namespace allscale::utils;

int main() {

    const int N = 10;
    using Grid = std::array<std::array<int,N>,N>;
    using Point = Vector<int,2>;

    Grid A;

	pfor(Point{N,N},[&](const Point& p){
		A[p.x][p.y] = p.x * p.y;
	});

	std::cout << "done" << std::endl;

    // check whether it is properly initialized
    for(int i=0; i<N; i++) {
        for(int j=0; j<N; j++) {
            if (A[i][j] != i*j) {
                std::cout << "Error: (" << i << "," << j << ")\n";
                return 1;
            }
        }
    }

    // everything is OK
	return 0;
}
