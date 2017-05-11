
#include "allscale/api/user/operator/async.h"

#include <atomic>


using namespace allscale::api::user;

int main() {

	std::cout << "Start" << std::endl;

	std::atomic<int> counter(0);

	std::cout << counter.load() << std::endl;

	auto task = async([&]{
		counter = 1;
	});

	// the given task should be valid
//	std::cout << task.isValid() << std::endl;

	// wait for the task to complete
	task.wait();

	// check whether side-effects took place
	std::cout << counter.load() << std::endl;
//	std::cout << task.isDone() << std::endl;

	std::cout << "End" << std::endl;

	return 0;
}

