
#include "allscale/utils/vector.h"
#include <vector>

template<typename T, std::size_t Dims>
using Vector = allscale::utils::Vector<T, Dims>;

int main() {

	Vector<int, 2> v1;
	Vector<int, 2> v2(v1);
	Vector<int, 2> v3;
	v3 = v1;

	std::vector<int> w1;
	std::vector<int> w2;
	w2 = w1;

	v1.x + v2.y;
	v1.asArray();
	v1 == v2;

	Vector<unsigned long, 4> vl;
}
