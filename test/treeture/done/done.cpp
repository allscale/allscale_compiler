
#include <iostream>
#include <allscale/api/core/prec.h>

int main() {

    allscale::api::core::treeture<int> done = allscale::api::core::done(12);

    if (done.isValid()) {
        std::cout << "Valid";
    } else {
        std::cout << "Invalid";
    }

    if (done.isDone()) {
        std::cout << "OK";
    } else {
        std::cout << "ERR";
    }

	return 0;
}
