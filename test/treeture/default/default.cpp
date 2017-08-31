
#include <iostream>
#include <allscale/api/core/prec.h>

int main() {

    allscale::api::core::treeture<int> t;

    if (t.isValid()) {
        std::cout << "Valid";
    } else {
        std::cout << "Invalid";
    }

    if (t.isDone()) {
        std::cout << "OK";
    } else {
        std::cout << "ERR";
    }

	return 0;
}
