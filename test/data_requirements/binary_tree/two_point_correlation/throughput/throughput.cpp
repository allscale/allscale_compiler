#include "../two_point_correlation.h"

int main() {

	const int Dims = 7;				// number of dimensions of points
	const std::size_t N = 29;		// number of points = 2^N
	const std::size_t M = 1000;		// number of queries

	// lower and upper boundary of nodes
	const coordinate_t low = 0;
	const coordinate_t high = 100;
	const distance_t radius = 20;

	using kdtree = KDTree<Dims,N>;

	// time measurement utils
	auto now = []{ return std::chrono::high_resolution_clock::now(); };
	auto ms = [](const auto& a, const auto& b) -> long {
		if (std::getenv("NO_TIME_IN_OUTPUT")) {
			return -1;
		}
		return std::chrono::duration_cast<std::chrono::milliseconds>(b-a).count();
	};
	auto begin = now();
	auto end = begin;

	// print a test case summary
	std::cout << "Two-Point-Correlation with 2^" << N << " points in [" << low << "," << high << ")^" << Dims << " space, radius=" << radius << "\n";

	// 1) set up tree
	std::cout << "Creating ... " << std::flush;
	begin = now();
	kdtree tree;
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";

	// 2) fill with data
	std::cout << "Filling  ... " << std::flush;
	begin = now();
	fill(tree, low, high);
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";

#ifdef NDEBUG

	// check the tree
	std::cout << "Checking ... " << std::flush;
	begin = now();
	bool correct = check(tree);
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";

	if (!correct) {
		std::cout << "INVALID KD TREE GENERATED - ABORTING!\n";
		return EXIT_FAILURE;
	}

	// 3) compute two-point correlation for two known points
	Point<Dims> p = Point<Dims>(0);

	std::cout << "\n";
	std::cout << "Computing correlation for p=" << p << " seq ... " << std::flush;
	begin = now();
	auto resSeq = twoPointCorrelationSequential(tree, p, radius);
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";
	std::cout << "Number of points within region: " << resSeq << "\n\n";

	std::cout << "\n";
	std::cout << "Computing correlation for p=" << p << " par ... " << std::flush;
	begin = now();
	auto resPar = twoPointCorrelation(tree, p, radius);
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";
	std::cout << "Number of points within region: " << resPar << "\n\n";

	if (resSeq != resPar) {
		std::cout << "VALIDATION FAILED - ABORTING!\n";
		return EXIT_FAILURE;
	}


	// test another point
	p = Point<Dims>(40);

	std::cout << "\n";
	std::cout << "Computing correlation for p=" << p << " seq ... " << std::flush;
	begin = now();
	resSeq = twoPointCorrelationSequential(tree, p, radius);
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";
	std::cout << "Number of points within region: " << resSeq << "\n\n";

	std::cout << "Computing correlation for p=" << p << " par ... " << std::flush;
	begin = now();
	resPar = twoPointCorrelation(tree, p, radius);
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";
	std::cout << "Number of points within region: " << resPar << "\n\n";

	if (resSeq != resPar) {
		std::cout << "VALIDATION FAILED - ABORTING!\n";
		return EXIT_FAILURE;
	}

#endif

	// 4) run benchmarking
	std::cout << "\n\n ----- Benchmarking -----\n\n";

	std::cout << "Creating benchmark input set (M=" << M << ") ... " << std::flush;
	begin = now();
	std::vector<Point<Dims>> points;
	for(std::size_t i=0; i<M; i++) {
		points.push_back(getRandomPoint<Dims>(low,high));
	}
	end = now();
	std::cout << std::setw(6) << ms(begin,end) << "ms\n";


    std::cout << "Benchmarking ... " << std::flush;
    begin = now();
    pfor(std::size_t(0),points.size(),[&,points,radius](std::size_t i) {
        sema::no_more_dependencies();
        twoPointCorrelation(tree, points[i], radius).get();
    });
    end = now();
    auto time = ms(begin,end);
    std::cout << std::setw(22) << time << "ms - ";
    std::cout << (time/M) << "ms per query, " << ((M*1000)/time) << " queries/s\n";

	std::cout << "\n ------------------------\n\n";

	// done
	return EXIT_SUCCESS;
}
