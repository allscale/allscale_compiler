#include <iostream>
#include <vector>

#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>
#include <allscale/utils/vector.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;
using namespace allscale::utils;

using Vec3 = Vector<int,3>;

struct Particle {
    int id;
    Vec3 pos;
};

struct Cell {
    int time;
    std::vector<Particle> particles;
};

void expect_eq(int a, int b) {
    if (a != b) {
        std::cout << "Expected: " << a << "\n";
        std::cout << "      Is: " << b << "\n";
//        exit(1);
    }
}

using Space = Grid<Cell,2>;

int main() {

    const int N = 1000;
    const int T = 100;

    Space dataA({N,N}); 
    Space dataB({N,N}); 

    using Point = Space::coordinate_type;

    // initialize first vector
    pfor(Point{0,0},Point{N,N},[&](Point p){
        dataA[p].time = 0;
        if (p.x == 0 && p.y == 0) {
            Vec3 pos(1,2,3);
            dataA[p].particles.push_back({ 42, pos });
        }
    });

    // run a time loop
    for(int t=0; t<T; t++) {

        // handle buffer swapping
        Space& A = (t % 2) ? dataB : dataA;
        Space& B = (t % 2) ? dataA : dataB;

        // update state
        pfor(Point{1,1},Point{N-1,N-1},[t,&A,&B](Point p) {

            // check the last state
            expect_eq(t,A[p].time);
            if (p.x > 1)   expect_eq(t,A[Point{p.x-1,p.y}].time);
            if (p.x < N-2) expect_eq(t,A[Point{p.x+1,p.y}].time);
            if (p.y > 1)   expect_eq(t,A[Point{p.x,p.y-1}].time);
            if (p.y < N-2) expect_eq(t,A[Point{p.x,p.y+1}].time);

            if (p.x == p.y && t == p.x) {
                expect_eq(1,A[p].particles.size());
                expect_eq(42,A[p].particles.front().id);
                expect_eq(1,A[p].particles.front().pos.x);
                expect_eq(2,A[p].particles.front().pos.y);
                expect_eq(3,A[p].particles.front().pos.z);
            } else {
                expect_eq(0,A[p].particles.size());
            }

            // move the particel
            auto& src = A[Point{p.x-1,p.y-1}].particles;
            B[p].particles = src;
            src.clear();

            // update time
            B[p].time = t + 1;

        });

    }

	return 0;
}
