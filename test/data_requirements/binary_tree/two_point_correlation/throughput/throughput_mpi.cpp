#include "../two_point_correlation.h"

#include <mpi.h>

template<std::size_t depth>
using KDRegion = typename KDTree<2,depth>::region_type;

namespace {

	template<std::size_t Dims, std::size_t depth>
	void fillSequential(KDTree<Dims,depth>& tree, const KDNode<depth>& node, const Point<Dims>& min, const Point<Dims>& max, const KDRegion<depth>& owned) {

		// skip not-owned region
		if (!node.addressesRootTree() && !owned.containsSubTree(node.getSubtreeIndex())) return;

		// split in half on this level
		auto d = node.getLevel() % Dims;

		// get current point
		auto& p = tree[node];

		// TODO: randomize coordinates
		p = min;

		// make this point split area in half
		p[d] = min[d] + (max[d] - min[d]) / 2;

		// if this is a leaf, we are done
		if (node.isLeaf()) return;

		// compute new boundaries
		Point<Dims> maxA = max;
		Point<Dims> minB = min;

		maxA[d] = p[d];
		minB[d] = p[d];

		// fill recursively
		fillSequential(tree,node.getLeftChild(),min,maxA,owned);
		fillSequential(tree,node.getRightChild(),minB,max,owned);
	}


	template<std::size_t Dims, std::size_t depth>
	bool checkSequential(const KDTree<Dims,depth>& tree, const KDNode<depth>& node, const BoundingBox<Dims>& box, const KDRegion<depth>& owned) {

		// skip not-owned region
		if (!node.addressesRootTree() && !owned.containsSubTree(node.getSubtreeIndex())) return true;

		// check this node
		auto& p = tree[node];
		if (!box.contains(p)) return false;

		// if node is a leaf node => stop here
		if (node.isLeaf()) return true;

		// otherwise descent
		BoundingBox<Dims> left  = box;
		BoundingBox<Dims> right = box;

		// get the split dimension
		auto d = node.getLevel() % Dims;

		// update bounding boxes
		left.high[d] = p[d];
		right.low[d] = p[d];

		// descent
		return checkSequential(tree, node.getLeftChild(), left, owned) && checkSequential(tree, node.getRightChild(), right, owned);
	}



	template<std::size_t Dims, std::size_t depth>
	void twoPointCorrelationLocal(const KDTree<Dims,depth>& tree, const Point<Dims>& t, distance_t radiusSquared, const KDNode<depth>& node, const BoundingBox<Dims>& box, const KDRegion<depth>& owned, std::pair<std::uint32_t, std::uint32_t>& res) {

		// if not in local subtree => skip
		if (!node.addressesRootTree() && !owned.containsSubTree(node.getSubtreeIndex())) return;

		// prune the search space
		if (box.minimumDistanceSquared(t) > radiusSquared) return;

		// check this node
		auto& p = tree[node];
		if (distanceSquared(t,p) <= radiusSquared) {
			// if this is within the root fragment ...
			if (node.getSubtreeIndex() == KDRegion<depth>::num_leaf_trees) {
				res.first++; // .. increment root fragment counter
			} else {
				res.second++; // .. increment subtree counter
			}
		}

		// if node is a leaf node => stop here
		if (node.isLeaf()) return;

		// otherwise descent
		BoundingBox<Dims> left  = box;
		BoundingBox<Dims> right = box;

		// get the split dimension
		auto d = node.getLevel() % Dims;

		// update bounding boxes
		left.high[d] = p[d];
		right.low[d] = p[d];

		// descent
		twoPointCorrelationLocal(tree,t,radiusSquared,node.getLeftChild(),left,owned,res);
		twoPointCorrelationLocal(tree,t,radiusSquared,node.getRightChild(),right,owned,res);
	}

	// counts the number of points within the neighborhood of a given point
	template<std::size_t Dims, std::size_t depth>
	std::size_t twoPointCorrelation(const KDTree<Dims,depth>& tree, const Point<Dims>& t, distance_t radius, const KDRegion<depth>& owned) {

		// count locally
		std::pair<std::uint32_t,std::uint32_t> localRes(0,0);
		twoPointCorrelationLocal(tree,t,radius*radius,KDNode<depth>(),BoundingBox<Dims>(),owned,localRes);

		// sum up total result
		std::uint32_t subTreeRes;
		MPI_Reduce(&localRes.second,&subTreeRes,1,MPI_UNSIGNED,MPI_SUM,0,MPI_COMM_WORLD);

		// aggregate result
		return localRes.first + subTreeRes;
	}

	// counts the number of points within the neighborhood of a given point
	template<std::size_t Dims, std::size_t depth>
	std::vector<std::uint32_t> twoPointCorrelation(const KDTree<Dims,depth>& tree, const std::vector<Point<Dims>>& pts, distance_t radius, const KDRegion<depth>& owned) {

		// create result vector length
		std::vector<std::uint32_t> roots(pts.size());
		std::vector<std::uint32_t> trees(pts.size());

		// count locally
		for(std::size_t i = 0 ; i<pts.size(); i++) {
			std::pair<std::uint32_t,std::uint32_t> localRes(0,0);
			twoPointCorrelationLocal(tree,pts[i],radius*radius,KDNode<depth>(),BoundingBox<Dims>(),owned,localRes);
			roots[i] = localRes.first;
			trees[i] = localRes.second;
		}

		// aggregate subtree results
		std::vector<std::uint32_t> sums(pts.size());
		MPI_Reduce(&trees[0],&sums[0],pts.size(),MPI_UNSIGNED,MPI_SUM,0,MPI_COMM_WORLD);

		// aggregate result
		for(std::size_t i = 0; i<pts.size(); i++) {
			sums[i] += roots[i];
		}

		// done
		return sums;
	}
}



int my_main(int argc, char** argv) {

	const int Dims = 7;				// number of dimensions of points
	const std::size_t N = 29;		// number of points = 2^N
	std::size_t M = 1000;		    // number of queries

    // parse parameters
    if (argc > 1) {
        M = std::atoi(argv[1]);
    }

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

	// get rank and size
	int rank, size;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	bool master = (rank == 0);

	// print a test case summary
	if (master) std::cout << "Two-Point-Correlation with 2^" << N << " points in [" << low << "," << high << ")^" << Dims << " space, radius=" << radius << ", performing M=" << M << " queries\n";

	// 1) set up tree
	if (master) std::cout << "Creating ... " << std::flush;
	begin = now();

	// pick the local share
	using kdfragment = kdtree::fragment_type;
	using kdrange = kdfragment::region_type;

	// every node gets a copy of the root
	kdrange ownShare = kdrange::root();

	// assign a fair share of the sub-trees
	for(int i=0; i<kdrange::num_leaf_trees; i++) {
		if ((i % size) == rank) ownShare = kdrange::merge(kdrange::subtree(i),ownShare);
	}

	// print debug info
	// std::cout << "Share on " << rank << ": " << ownShare << "\n";

	// create the local fragment
	kdfragment fragment(kdfragment::shared_data_type(),ownShare);

	// create the local tree
	kdtree tree = fragment.mask();

	// create a root node reference
	KDNode<N> root;

	end = now();
	if (master) std::cout << std::setw(6) << ms(begin,end) << "ms\n";

	// 2) fill with data
	if (master) std::cout << "Filling  ... " << std::flush;
	begin = now();
	fillSequential(tree, root, Point<Dims>(low), Point<Dims>(high), ownShare);
	end = now();
	if (master) std::cout << std::setw(6) << ms(begin,end) << "ms\n";

#ifndef NDEBUG

	// check the tree
	if (master) std::cout << "Checking ... " << std::flush;
	begin = now();

	bool correct = false;
	{
		BoundingBox<Dims> universe;
		universe.low  = Point<Dims>(-std::numeric_limits<coordinate_t>::max());
		universe.high = Point<Dims>(+std::numeric_limits<coordinate_t>::max());

		// run checks on local share
		bool localResult = checkSequential(tree, root, universe, ownShare);

		// combine results
		MPI_Allreduce(&localResult,&correct,1,MPI_CXX_BOOL,MPI_LAND,MPI_COMM_WORLD);
	}
	end = now();
	if (master) std::cout << std::setw(6) << ms(begin,end) << "ms\n";

	if (!correct) {
		if (master) std::cout << "INVALID KD TREE GENERATED - ABORTING!\n";
		return EXIT_FAILURE;
	}


	// 3) compute two-point correlation for two known points
	Point<Dims> p = Point<Dims>(0);

	if (master) std::cout << "\n";
	if (master) std::cout << "Computing correlation for p=" << p << " seq ... " << std::flush;
	begin = now();
	std::size_t res = twoPointCorrelation(tree, p, radius, ownShare);
	end = now();
	if (master) std::cout << std::setw(6) << ms(begin,end) << "ms\n";
	if (master) std::cout << "Number of points within region: " << res << "\n\n";

	// test another point
	p = Point<Dims>(40);

	if (master) std::cout << "\n";
	if (master) std::cout << "Computing correlation for p=" << p << " seq ... " << std::flush;
	begin = now();
	res = twoPointCorrelation(tree, p, radius, ownShare);
	end = now();
	if (master) std::cout << std::setw(6) << ms(begin,end) << "ms\n";
	if (master) std::cout << "Number of points within region: " << res << "\n\n";

#endif

	// 4) run benchmarking
	if (master) std::cout << "\n\n ----- Benchmarking -----\n\n";

	if (master) std::cout << "Creating benchmark input set (M=" << M << ") ... " << std::flush;
	begin = now();
	srand(1);
	std::vector<Point<Dims>> points;
	for(std::size_t i=0; i<M; i++) {
		points.push_back(getRandomPoint<Dims>(low,high));
	}
	end = now();
	if (master) std::cout << std::setw(6) << ms(begin,end) << "ms\n";

	if (master) std::cout << "Benchmarking ... " << std::flush;
	begin = now();

	// broadcast points
	MPI_Bcast(&points[0],sizeof(Point<Dims>)*M,MPI_CHAR,0,MPI_COMM_WORLD);

	// compute two-point correlations
	auto counts = twoPointCorrelation(tree,points,radius,ownShare);
	MPI_Barrier(MPI_COMM_WORLD);
	end = now();
	auto time = ms(begin,end);
	if (master) std::cout << std::setw(22) << time << "ms - ";
	if (master) std::cout << (time/M) << "ms per query, " << ((time > 0) ? ((M*1000)/time) : -1) << " queries/s\n";

	std::size_t sum = 0;
	for(const auto& cur : counts) {
		sum += cur;
	}

	if (master) std::cout << "Total number of points: " << sum << "\n";

	if (master) std::cout << "\n ------------------------\n\n";

	// done
	return EXIT_SUCCESS;
}


int main(int argc, char** argv) {

    // start up MPI
    MPI_Init(&argc,&argv);

    // run actual computation
    int res = my_main(argc,argv);

    // shut down MPI
    MPI_Finalize();

    // done
    return res;
}
