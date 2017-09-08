#include <cstdlib>
#include <iostream>
#include <random>

int main() {
	std::random_device rd;
	std::mt19937 gen(rd());
	std::uniform_real_distribution<double> dis(0, 1);

	dis(gen);

	std::cout << "random number generated" << std::endl;

    return 0;
}
