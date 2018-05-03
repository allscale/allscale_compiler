
#include <array>
#include <chrono>
#include <cstdlib>
#include <iostream>
#include <iomanip>
#include <limits>
#include <vector>

#include <allscale/utils/vector.h>

#include <allscale/api/core/prec.h>
#include <allscale/api/user/arithmetic.h>
#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/binary_tree.h>

using namespace allscale::api::core;
using namespace allscale::api::user;
using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;

// ------- KD-Tree ------

using coordinate_t = float;
using distance_t = coordinate_t;

template<std::size_t Dims>
using Point = allscale::utils::Vector<coordinate_t,Dims>;

template<std::size_t Dims>
distance_t lengthSquared(const Point<Dims>& p) {
	return sumOfSquares(p);
}

template<std::size_t Dims>
distance_t distanceSquared(const Point<Dims>& a, const Point<Dims>& b) {
	return sumOfSquares(a-b);
}

template<std::size_t Dims>
Point<Dims> getRandomPoint(const Point<Dims>& low, const Point<Dims>& high) {
	Point<Dims> res;
	for(unsigned i=0; i<Dims; i++) {
		res[i] = low[i] + ((rand()/coordinate_t(RAND_MAX)) * (high[i]-low[i]));
	}
	return res;
}

template<std::size_t Dims>
struct BoundingBox {
	Point<Dims> low;
	Point<Dims> high;

	/**
	 * Creates a bounding box enclosing the universe.
	 */
	BoundingBox() {
		auto max = std::numeric_limits<coordinate_t>::max();
		for(unsigned i=0; i<Dims; i++) {
			low[i] = -max;
			high[i] = max;
		}
	}

	/**
	 * Obtains the minimum distance of the given point to a point
	 * within this bounding box.
	 */
	distance_t minimumDistanceSquared(const Point<Dims>& p) const {
		Point<Dims> dist;
		for(unsigned i=0; i<Dims; i++) {
			dist[i] = std::max(low[i] - p[i], std::max(p[i] - high[i], coordinate_t(0)));
		}
		return lengthSquared<Dims>(dist);
	}

	/**
	 * Tests whether the given point is contained in this box.
	 */
	bool contains(const Point<Dims>& p) const {
		return low.dominatedBy(p) && p.strictlyDominatedBy(high);
	}

	friend std::ostream& operator<<(std::ostream& out, const BoundingBox& box) {
		return out << "[" << box.low << " - " << box.high << ")";
	}

};


template<std::size_t Dims, std::size_t depth>
using KDTree = StaticBalancedBinaryTree<Point<Dims>,depth>;

template<std::size_t depth>
using KDNode = StaticBalancedBinaryTreeElementAddress<depth>;

namespace {

	template<std::size_t Dims, std::size_t depth>
	void fillSequential(KDTree<Dims,depth>& tree, const KDNode<depth>& node, const Point<Dims>& min, const Point<Dims>& max) {

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
		fillSequential(tree,node.getLeftChild(),min,maxA);
		fillSequential(tree,node.getRightChild(),minB,max);
	}

}


template<std::size_t Dims, std::size_t depth>
void fill(KDTree<Dims,depth>& tree, const Point<Dims>& min, const Point<Dims>& max) {

	using region = StaticBalancedBinaryTreeRegion<depth>;

	struct Args {
		KDNode<depth> node;
		Point<Dims> min;
		Point<Dims> max;
	};

	prec(
		[](const Args& args) {
			// stop on leaf level
			return args.node.isLeaf();
		},
		[&tree](const Args& args) {
			sema::needs_write_access(tree,region::closure(args.node.getSubtreeIndex()));
			fillSequential(tree,args.node,args.min,args.max);
		},
		pick(
			[&tree](const Args& args, const auto& rec) {

				// split in half on this level
				auto d = args.node.getLevel() % Dims;

				// get current point
				auto& p = tree[args.node];

				// TODO: randomize coordinates
				p = args.min;

				// make this point split area in half
				p[d] = args.min[d] + (args.max[d] - args.min[d]) / 2;

				// this node must not be a leaf (otherwise it would be a base case)
				assert_false(args.node.isLeaf());

				// compute new boundaries
				Point<Dims> maxA = args.max;
				Point<Dims> minB = args.min;

				maxA[d] = p[d];
				minB[d] = p[d];

				// fill recursively
				return parallel(
					rec({args.node.getLeftChild(),args.min,maxA}),
					rec({args.node.getRightChild(),minB,args.max})
				);
			},
			[&tree](const Args& args, const auto&) {
				sema::needs_write_access(tree,region::closure(args.node.getSubtreeIndex()));
				fillSequential(tree,args.node,args.min,args.max);
			}
		)
	)({KDNode<depth>(),min,max}).get();

}

template<std::size_t Dims, std::size_t depth>
void fill(KDTree<Dims,depth>& tree, coordinate_t min, coordinate_t max) {

	// update boundaries to points
	Point<Dims> a(min);
	Point<Dims> b(max);

	// fill in nodes
	fill(tree,a,b);
}

namespace {

	template<std::size_t Dims, std::size_t depth>
	bool checkSequential(const KDTree<Dims,depth>& tree, const KDNode<depth>& node, const BoundingBox<Dims>& box) {

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
		return checkSequential(tree, node.getLeftChild(), left) && checkSequential(tree, node.getRightChild(), right);
	}

}

// checks that the KD tree condition is satisfied
template<std::size_t Dims, std::size_t depth>
bool check(const KDTree<Dims,depth>& tree) {

	struct Args {
		KDNode<depth> node;
		BoundingBox<Dims> box;
	};

	// the box enclosing everything
	BoundingBox<Dims> universe;
	universe.low  = Point<Dims>(-std::numeric_limits<coordinate_t>::max());
	universe.high = Point<Dims>(+std::numeric_limits<coordinate_t>::max());

	return prec(
		[](const Args& args) {
			// stop on leaf level
			return args.node.isLeaf();
		},
		[&tree](const Args& args) {
			return checkSequential(tree,args.node,args.box);
		},
		pick(
			[&tree](const Args& args, const auto& rec) {

				auto& node = args.node;
				auto& box = args.box;

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

				// descent recursively, in parallel
				return combine(
					rec({node.getLeftChild(),left}),
					rec({node.getRightChild(),right}),
					[](bool a, bool b) { return a && b; }
				).get();
			},
			[&tree](const Args& args, const auto&) {
				return checkSequential(tree,args.node,args.box);
			}
		)
	)({KDNode<depth>(),universe}).get();

}



namespace {

	template<std::size_t Dims, std::size_t depth>
	std::size_t twoPointCorrelationSequential(const KDTree<Dims,depth>& tree, const Point<Dims>& t, distance_t radiusSquared, const KDNode<depth>& node, const BoundingBox<Dims>& box) {

		// prune the search space
		if (box.minimumDistanceSquared(t) > radiusSquared) return 0;

		// check this node
		auto& p = tree[node];
		std::size_t res = 0;
		if (distanceSquared(t,p) <= radiusSquared) res++;

		// if node is a leaf node => stop here
		if (node.isLeaf()) return res;

		// otherwise descent
		BoundingBox<Dims> left  = box;
		BoundingBox<Dims> right = box;

		// get the split dimension
		auto d = node.getLevel() % Dims;

		// update bounding boxes
		left.high[d] = p[d];
		right.low[d] = p[d];

		// descent
		return res +
				twoPointCorrelationSequential(tree,t,radiusSquared,node.getLeftChild(),left) +
				twoPointCorrelationSequential(tree,t,radiusSquared,node.getRightChild(),right);
	}

	// a direct entry point for computing the two-point correlation sequentially -- for debugging
	template<std::size_t Dims, std::size_t depth>
	std::size_t twoPointCorrelationSequential(const KDTree<Dims,depth>& tree, const Point<Dims>& t, distance_t radius) {
		return twoPointCorrelationSequential(tree,t,radius*radius,KDNode<depth>(),BoundingBox<Dims>());
	}

}

// counts the number of points within the neighborhood of a given point
template<std::size_t Dims, std::size_t depth>
treeture<std::size_t> twoPointCorrelation(const KDTree<Dims,depth>& tree, const Point<Dims>& t, distance_t radius) {

	// the kind of region used to address requirements
	using region = StaticBalancedBinaryTreeRegion<depth>;

	// square the radius (all computation operates on squares)
	distance_t radiusSquared = radius * radius;

	struct Args {
		KDNode<depth> node;
		BoundingBox<Dims> box;
	};

	// the box enclosing everything
	BoundingBox<Dims> universe;

	return prec(
		[](const Args& args) {
			// stop on leaf level
			return args.node.isLeaf() || args.node.getLevel() > int(2*depth/3);
		},
		[=,&tree](const Args& args) {
			sema::needs_read_access(tree,region::closure(args.node.getSubtreeIndex()));
			return twoPointCorrelationSequential(tree,t,radiusSquared,args.node,args.box);
		},
		pick(
			[=,&tree](const Args& args, const auto& rec) -> std::size_t {

				auto& node = args.node;
				auto& box = args.box;

				// prune the search space
				if (box.minimumDistanceSquared(t) > radiusSquared) return 0;

				// check this node
				auto& p = tree[node];
				std::size_t res = 0;
				if (distanceSquared(t,p) < radiusSquared) res++;

				// if node is a leaf node => stop here
				if (node.isLeaf()) return res;

				// otherwise descent
				BoundingBox<Dims> left  = box;
				BoundingBox<Dims> right = box;

				// get the split dimension
				auto d = node.getLevel() % Dims;

				// update bounding boxes
				left.high[d] = p[d];
				right.low[d] = p[d];

				// descent
				auto fA = run(rec({node.getLeftChild(),left}));
				auto fB = run(rec({node.getRightChild(),right}));
				return res + fA.get() + fB.get();
			},
			[=,&tree](const Args& args, const auto&) {
				sema::needs_read_access(tree,region::closure(args.node.getSubtreeIndex()));
				return twoPointCorrelationSequential(tree,t,radiusSquared,args.node,args.box);
			}
		)
	)({KDNode<depth>(),universe});

}
