// This program exists to test whether we get the same parameters passed from HPX
#include <iostream>

int main(int argc, char** argv) {
	std::cout << argc << std::endl;
	std::cout << argv[1] << std::endl;
	return 0;
}
