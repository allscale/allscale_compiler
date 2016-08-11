#pragma once

#include <ostream>
#include <vector>

#include <cilk/cilk.h>

#include "parec/ops.h"

#include "allscale/stencil/stencil.h"
#include "utils/container_utils.h"

namespace allscale {
namespace stencil {
namespace solver {

	using std::vector;

	namespace detail {

		template<size_t dims>
		using Slopes = std::array<int,dims>;

		template<typename A, size_t dims>
		std::array<A,dims> operator+(const std::array<A,dims>& a, const std::array<A,dims>& b) {
			std::array<A,dims> res;
			for(int i=0; i<dims; i++) res[i] = a[i] + b[i];
			return res;
		}

		template<typename A, typename B, size_t dims>
		std::array<A,dims> operator*(const std::array<A,dims>& in, B f) {
			std::array<A,dims> res;
			for(int i=0; i<dims; i++) res[i] = in[i] * f;
			return res;
		}

		template<typename A, typename B, size_t dims>
		std::array<A,dims> operator/(const std::array<A,dims>& in, B f) {
			std::array<A,dims> res;
			for(int i=0; i<dims; i++) res[i] = in[i] / f;
			return res;
		}


		template<size_t dims>
		struct Base {

			using range = std::pair<int,int>;

			std::array<range,dims> boundaries;

			static Base full(const utils::Size<dims>& size) {
				Base res;
				for(size_t i=0; i<dims; i++) {
					res.boundaries[i] = { 0, size[i] };
				}
				return res;
			}

			range& operator[](size_t i) {
				return boundaries[i];
			}

			const range& operator[](size_t i) const {
				return boundaries[i];
			}

			Base operator+(const utils::Coordinate<dims>& other) const {
				Base res;
				for(size_t i=0; i<dims; i++) {
					res.boundaries[i] = { boundaries[i].first + other[i], boundaries[i].second + other[i] };
				}
				return res;
			}

			int width(int dim) const {
				return boundaries[dim].second - boundaries[dim].first;
			}

			friend std::ostream& operator<<(std::ostream& out, const Base& base) {
				if (dims == 0) return out << "[]";
				out << "[";
				for(size_t i=0; i<dims-1; i++) {
					out << base.boundaries[i].first << "-" << base.boundaries[i].second << ",";
				}
				out  << base.boundaries[dims-1].first << "-" << base.boundaries[dims-1].second;
				return out << "]";
			}
		};


		template<size_t dims>
		Slopes<dims> ones() {
			Slopes<dims> res;
			for(size_t i=0; i<dims; i++) res[i] = 1;
			return res;
		}

		template<size_t dim>
		struct plain_scanner {

			plain_scanner<dim-1> nested;

			template<size_t full_dim, typename Lambda>
			void operator()(const Base<full_dim>& base, const Lambda& lambda, utils::Coordinate<full_dim>& pos, int t) const {
				for(pos[dim]=base[dim].first; pos[dim]<base[dim].second; pos[dim]++) {
					 nested(base, lambda, pos, t);
				}
			}

		};

		template<>
		struct plain_scanner<0> {

			template<size_t full_dim, typename Lambda>
			void operator()(const Base<full_dim>& base, const Lambda& lambda, utils::Coordinate<full_dim>& pos, int t) const {
				for(pos[0]=base[0].first; pos[0]<base[0].second; pos[0]++) {
					lambda(pos, t);
				}
			}

		};

		template<size_t dims>
		struct Zoid {
			Base<dims> base;			// the projection of the zoid to the space dimensions
			Slopes<dims> slopes;		// the direction of the slopes
			size_t t0;
			size_t t1;

			Zoid() : base(), slopes(ones<dims>()), t0(0), t1(0) {}

			Zoid(const Base<dims>& base, const Slopes<dims>& slopes, size_t t0, size_t t1)
				: base(base), slopes(slopes), t0(t0), t1(t1) {}

			vector<vector<Zoid>> splitTime() const {
				auto split = getHeight() / 2;

				Base<dims> baseA = base;
				Base<dims> baseB = base;

				for(size_t i=0; i<dims; i++) {
					auto slope = slopes[i];
					if (slope < 0) {
						baseA[i].first = baseA[i].first + slope * split;
						baseA[i].second = baseA[i].second - slope * split;
					} else {
						baseB[i].first = baseB[i].first + slope * split;
						baseB[i].second = baseB[i].second - slope * split;
					}
				}

				return vector<vector<Zoid>>({
					vector<Zoid>({ Zoid(baseA, slopes, t0, t0+split) }),
					vector<Zoid>({ Zoid(baseB, slopes, t0+split, t1) })
				});
			}

			vector<vector<Zoid>> splitSpace() const {

				// find longest dimension
				int max_dim;
				int max_width = 0;
				for(size_t i=0; i<dims; i++) {
					int width = getWidth(i);
					if (width>max_width) {
						max_width = width;
						max_dim = i;
					}
				}

				// the max dimension is the split dimensin
				auto split_dim = max_dim;

				// check whether longest dimension can be split
				assert(splitable(split_dim));

				// create 3 fragments
				Zoid a = *this;
				Zoid b = *this;
				Zoid c = *this;

				// get the split point
				auto center = (base.boundaries[split_dim].first + base.boundaries[split_dim].second) / 2;
				auto left = center;
				auto right = center;

				if (slopes[split_dim] < 0) {
					auto hight = getHeight();
					left -= hight;
					right += hight;
				}

				a.base.boundaries[split_dim].second = left;
				b.base.boundaries[split_dim] = { left, right };
				c.base.boundaries[split_dim].first = right;

				// invert direction of center piece
				b.slopes[split_dim] *= -1;

				// add fragments in right order
				if (slopes[split_dim] < 0) {
					return vector<vector<Zoid>>({
						vector<Zoid>({ b }),
						vector<Zoid>({ a,c })
					});
				}

				// return positive order
				return vector<vector<Zoid>>({
					vector<Zoid>({ a, c }),
					vector<Zoid>({ b })
				});
			}

			vector<vector<Zoid>> split() const {
				assert(!isTerminal() && "Do not split terminal Zoid!");

				// try a space split
				if (spaceSplitable()) {
					return splitSpace();
				}

				// fall back to time split
				return splitTime();
			}

			bool isTerminal() const {
				return getFootprint() < 100;			// todo: find better limit
			}


			template<typename Lambda>
			void for_each(const Lambda& lambda) const {

				// create the plain scanner
				plain_scanner<dims-1> scanner;

				utils::Coordinate<dims> x;
				auto plainBase = base;

				// over the time
				for(size_t t = t0; t < t1; ++t) {

					// process this plain
					scanner(plainBase, lambda, x, t);

					// update the plain for the next level
					for(size_t i=0; i<dims; ++i) {
						plainBase[i].first  += slopes[i];
						plainBase[i].second -= slopes[i];
					}
				}

			}

			int getWidth(int dim) const {
				int res = base.width(dim);
				if (slopes[dim] < 0) res += 2*getHeight();
				return res;
			}

			int getHeight() const {
				return t1-t0;
			}

			bool spaceSplitable() const {
				for(size_t i=0; i<dims; i++) {
					if (splitable(i)) return true;
				}
				return false;
			}

			bool splitable(int dim) const {
				return (slopes[dim] > 0) ?
					base.width(dim) > 4*getHeight()
				:
					base.width(dim) > 2*getHeight()
				;
			}

			int getFootprint() const {
				int size = 1;
				for(size_t i=0; i<dims; i++) {
					size *= base.width(i);				// todo: this is wrong!
				}
				return size;
			}

			friend std::ostream& operator<<(std::ostream& out, const Zoid& zoid) {
				return out << "Zoid(" << zoid.base << "," << zoid.slopes << "," << zoid.t0 << "-" << zoid.t1 << ")";
			}

		};

		template<size_t dims>
		vector<vector<Zoid<dims>>> splitBox(const utils::Size<dims>& size, int t0, int t1) {

			vector<vector<Zoid<dims>>> res(dims+1);

			// generate binary patterns from 0 to 2^dims - 1
			for(size_t i=0; i < (1<<dims); i++) {

				// get base of zoid
				Base<dims> base = Base<dims>::full(size);
				for(size_t j=0; j<dims; j++) {
					if (i & (1<<j)) {
						auto& cur = base.boundaries[j];
						cur.first += size[j] / 2;
						cur.second += size[j] / 2;
					}
				}

				// get slopes of zoid
				Slopes<dims> slopes;
				for(size_t j=0; j<dims; j++) {
					slopes[j] = (i & (1<<j)) ? -1 : 1;
					if (slopes[j] < 0) {
						base[j].first += size[j]/2;
						base[j].second -= size[j]/2;
					}
				}

				int num_ones = 0;
				for(size_t j=0; j<dims; j++) {
					if (i & (1<<j)) num_ones++;
				}

				res[num_ones].push_back(Zoid<dims>(base, slopes, t0, t1));
			}

			// done
			return res;
		}


		template<size_t dims>
		vector<vector<Zoid<dims>>> splitBox(const utils::Size<dims>& size, unsigned height) {

			// determine layer height
			int hStep = size[0];
			for(size_t i=0; i<dims; i++) {
				if (size[i] < hStep) hStep = size[i];
			}

			// pyramids are half as wide as high
			hStep = hStep/2;

			// build up layer by layer
			vector<vector<Zoid<dims>>> res;
			for(unsigned i=0; i<height; i+=hStep) {
				auto top = i+hStep;
				if (top > height) top = height;
				for(const auto& cur : splitBox(size,i,top)) {
					res.push_back(cur);
				}
			}

			// done
			return res;

		}

	}


	template<typename Kernel, typename Observers>
	struct simple_recursive_solver {

		template<typename Data, typename Zoid, typename Operation>
		void process(Data& a, Data& b, const Zoid& zoid, const Operation& kernel) const {

			using namespace detail;

			parec::prec(
					// - base case test -
					[&](const Zoid& zoid) {
						return zoid.isTerminal();
					},
					// - base case -
					[&](const Zoid& zoid) {
						zoid.for_each([&](const auto& x, int t) {
							if (t%2) {
								kernel(a,b,x,t);
							} else {
								kernel(b,a,x,t);
							}
						});
					},
					// - step case -
					[&](const Zoid& zoid, const auto& rec) {
						// for each group ..
						for(const auto& group : zoid.split()) {
							// run tasks in parallel
							vector<decltype(rec(zoid))> tasks;
							for(const auto& cur : group) {
								tasks.push_back(rec(cur));
							}
							for(auto& cur : tasks) {
								cur.get();
							}
						}
					}
			)(zoid).get();

//			if (zoid.isTerminal()) {
//				// compute zoid
//				zoid.for_each([&](const utils::Coordinate<dims>& x, int t) {
//					if (t%2) {
//						kernel(a,b,x,t);
//					} else {
//						kernel(b,a,x,t);
//					}
//				});
//			} else {
//
//				for(const auto& group : zoid.split()) {
//					// this one can be parallelized
//// -- sequential --
//					for(const auto& cur : group) {
//						process(a,b,cur,kernel);
//					}
//
//// -- cilk --
////					cilk_for(auto it = group.begin(); it < group.end(); ++it) {
////						auto& cur = *it;
////						process(a,b,cur,kernel);
////					}
//
//				}
//			}

		}

		template<
			typename Cell,
			size_t Dimensions,
			typename Initializer,
			template<typename,size_t> class BoundaryCondition,
			typename ... ConstantsAndStreams
		>
		void operator()(
				int timeSteps,
				const VariableGrid<Cell,Dimensions,Initializer,BoundaryCondition>& varGrid,
				const ConstantsAndStreams& ... parameters
			) const {


			using grid_type = utils::Grid<Cell,Dimensions,BoundaryCondition<Cell,Dimensions>>;

			Initializer init = Initializer();
			Kernel step = Kernel();

			auto size = varGrid.size;

			// create the grid (A/B)
			grid_type a(size);
			grid_type b(size);

			// create and initialize the parameters
			detail::parameter_container<ConstantsAndStreams...> params(parameters...);

			// create observers
			detail::observer_handler<Observers> observers;

			// run grid initializer
			utils::pfor(size,[&](const auto& pos) {
				a[pos] = init(pos);
				observers.process(a[pos], pos, 0, params);
			});

			auto kernel = [&](grid_type& res, const grid_type& old, const utils::Coordinate<Dimensions>& pos, int t) {

				utils::Coordinate<Dimensions> x;
				for(size_t i=0; i<Dimensions; i++) {
					x[i] = (size[i] + pos[i]) % size[i];
				}

				// update cell
				params.call(step, res[x], old, x, t);

				// run observers
				observers.process(res[x], x, t+1, params);			// the t+1 version has been computed here
			};

			// process problem
			for(const auto& group : detail::splitBox(size, timeSteps)) {
				// this can be run in parallel

// -- sequential --
//				for(const auto& cur : group) {
//					process(a,b,cur,kernel);
//				}

// -- cilk --
//				cilk_for(auto it = group.begin(); it < group.end(); ++it) {
//					auto& cur = *it;
//					process(a,b,cur,kernel);
//				}

// -- parec --
				parec::pfor(group, [&](const detail::Zoid<Dimensions>& cur) {
					process(a,b,cur,kernel);
				});
			}
		}
	};


} // end namespace solver
} // end namespace stencil
} // end namespace allscale
