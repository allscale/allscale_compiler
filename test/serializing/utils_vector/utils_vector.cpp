
#include <iostream>
#include "allscale/utils/vector.h"

using namespace allscale::utils;

int main() {

    std::cout << "int:" << is_serializable<int>::value << "\n";
    std::cout << "vector<int,2>:" << is_serializable<Vector<int,2>>::value << "\n";

    return EXIT_SUCCESS;
}
