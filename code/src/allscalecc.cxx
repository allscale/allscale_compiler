#include <cstdlib>
#include <iostream>

#include "allscale/name.h"

int main() {
	std::cout << "Hello World, this is " << allscale::name() << std::endl;
	return EXIT_SUCCESS;
}
