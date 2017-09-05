
#include <iostream>
#include <allscale/api/core/prec.h>

using namespace allscale::api::core;

void check(const treeture<int>& t) {

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

}

int main() {

    // create a completed treeture
    treeture<int> a = done(12);

    // support a move assignment
    auto t = std::move(a);
    check(t);

    // support multiple moves to the same location
    for(int i=0; i<10; i++) {
        t = done(i);
        check(t);
    }

	return 0;
}
