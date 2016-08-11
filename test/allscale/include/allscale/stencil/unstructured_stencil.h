#pragma once

#include "allscale/utils/graph.h"
#include "parec/ops.h"

namespace allscale {
namespace stencil {

	// -- unstructured stencil --

	struct iterative_unstructured_solver;

	template<
		typename kernel,
		typename solver = iterative_unstructured_solver,
		typename node_spec
	>
	void unstructured_stencil(const utils::Graph<node_spec>& graph, int T) {
		solver().template operator()<kernel,node_spec>(graph,T);
	}


	struct iterative_unstructured_solver {

		template<
			typename kernel,
			typename node_spec
		>
		void operator()(const utils::Graph<node_spec>& graph, int T) const {

			// create static data container => to be distributed to all nodes
			const auto dataS = graph.createStaticData();

			// get reference to topology
			const auto& topology = graph.getTopologyData();

			// create A and B copy	=> to be dynamically distributed
			auto dataA = graph.createDynamicData();
			auto dataB = graph.createDynamicData();

			// get partitioning scheme of graph
			auto nodes = graph.partition();

			kernel krnl;
			for(int time=0; time<T; time++) {

				auto& A = (time % 2) ? dataA : dataB;
				const auto& B = (time % 2) ? dataB : dataA;

//				// for all nodes ...
//				for(const auto& cur : nodes) {
//					// ... apply kernel
//					krnl(A[cur], B, topology, cur, time);
//				}

				// for all nodes parallel ...
				parec::pfor(nodes, [&](const auto& cur) {
					// ... apply kernel
					krnl(A[cur], B, topology, cur, time);
				});


			}

		}

	};



	template<
		typename Set
	>
	class Frustum {

		enum Orientation {
			Opening, Closing
		};

		Set base;					// base
		Set crop;					// the base of the croping frustum
		int start, end;				// start / end time
		Orientation orientation;	// upwards / downwards frustum

	public:

		Frustum(const Set& base, const Set& crop, int start, int end, Orientation orientation = Closing)
			: base(base), crop(crop), start(start), end(end), orientation(orientation) {}

		bool isBaseCase() const {
//			return true;
			// TODO: this is an arbitrary value and needs to be fixed
			return (start == end) || base.radius() < 4 || base.size() < 2;		// can happen with isolated nodes
		}

		int radius() const {
			int res = 0;
			auto space = base;
			auto limit = crop;
			while(!(space - limit).empty()) {
				res++;
				space = (orientation == Closing) ? space.interior() : space.closure();
				limit = (orientation == Closing) ? limit.closure() : limit.interior();
			}
			return res;
		}

		std::vector<std::vector<Frustum>> time_split() const {

			// compute cut time and base
			auto dt = (end - start) / 2;

			// get properties at cut
			auto cutTime = start + dt;

			auto cutBase = (orientation == Closing)
					? base.interior(dt)
					: base.closure(dt);

			auto cutCrop = (orientation == Closing)
					? crop.closure(dt)
					: crop.interior(dt);

			// return fragments to be computed
			return {
				{Frustum(base, crop, start, cutTime, orientation)},
				{Frustum(cutBase, cutCrop, cutTime, end, orientation)}
			};

		}

		std::vector<std::vector<Frustum>> split() const {

			// check radius of base
			int dt = end - start;

			// handle flat partitions
			if (dt == 1) {
				auto parts = base.split();
				auto& A = parts[0];
				auto& B = parts[1];
				return {
					{Frustum(A,crop,start,end,Closing), Frustum(B,crop,start,end,Closing)}
				};
			}

			// split base / top depending on orientation
			auto N = (orientation == Closing || (dt == 1))
					? base
					: base.closure(dt) - crop.interior(dt);

			if (N.empty()) {
				return time_split();
			}

			// split N
			auto parts = N.split();
			if (parts.size() != 2) {
				std::cout << "Error in splitting " << *this << "\n";
				std::cout << "Number of parts: " << parts.size() << "\n" << parts << "\n";
			}
			assert(parts.size() == 2);

			// check radius of fragments
			for(const auto& cur : parts) {
				// in this case one of the fragments is to small => use time cut
				if (cur.radius() <= dt) return time_split();
			}

			// apply space cut

			if (orientation == Closing) {

				/* it has shape: /___________\ */
				/* split:        /_A_\_C_/_B_\ */

				auto A = parts[0] & crop;
				auto B = parts[1] & crop;
				auto C = base.interior() - (A.interior() + B.interior());

				return {
					{Frustum(A,A,start,end,Closing),Frustum(B,B,start,end,Closing)},
					{Frustum(C,base.interior(),start+1,end,Opening)}
				};

			} else {

				/* it has shape: \___________/ */
				/* split:        \_A_/_C_\_B_/ */

				// update A,B,C
				auto A = parts[0].interior(dt) & base;
				auto B = parts[1].interior(dt) & base;
				auto C = (base - ( A + B )) & crop;

				assert(!A.empty());
				assert(!B.empty());

				return {
					{Frustum(C,crop,start,end,Closing)},
					{Frustum(A,crop,start,end,Opening),Frustum(B,crop,start,end,Opening)}
				};

			}



//			// apply space cut
//			auto parts = base.split();
//			assert(parts.size() == 2);
//
//			auto& A = parts[0];
//			auto& B = parts[1];
//			auto C = base.interior() - (A.interior() + B.interior());
//
//			if (orientation == Closing) {
//
//				/* it has shape: /___________\ */
//				/* split:        /_A_\_C_/_B_\ */
//
//				return {
//					{Frustum(A,A,start,end,Closing),Frustum(B,B,start,end,Closing)},
//					{Frustum(C,base,start+1,end,Opening)}
//				};
//
//			} else {
//
//				/* it has shape: \___________/ */
//				/* split:        \_A_/_C_\_B_/ */
//
//				// update A,B,C
//				A = A.closure(dt);
//				B = B.closure(dt);
//				C = C.interior(dt);
//
//				return {
//					{Frustum(C,C,start,end-1,Closing)},
//					{Frustum(A,base,start,end,Opening),Frustum(B,base,start,end,Opening)}
//				};
//
//			}

			return std::vector<std::vector<Frustum>>();
		}

		template<typename Fun>
		void for_each(const Fun& f) const {
			auto space = base;
			auto limit = crop;

			// for the time range
			for(int time = start; time<end; time++) {

				/*
				if (space != (space & limit)) {
					std::cout << "Limit: " << limit << "\n";
					std::cout << "Space: " << space << "\n";
					std::cout << "  Res: " << (space & limit) << "\n";
					std::cout << "\n";
				}
				*/

				// process plain (intersection of space and limit)
				for(const auto& cur : space & limit) {
					f(cur, time);
				}
				// update space and limit
				space = (orientation == Closing) ? space.interior() : space.closure();
				limit = (orientation == Closing) ? limit.closure() : limit.interior();
			}
		}

		std::set<std::pair<typename Set::node_ref_type,int>> getPoints() const {
			std::set<std::pair<typename Set::node_ref_type,int>> res;
			for_each([&](const auto& pos, int time) {
				res.insert({pos, time});
			});
			return res;
		}

		friend std::ostream& operator<<(std::ostream& out, const Frustum& f) {
			return out << "Frustum(" << f.base << "," << f.crop << ",[" << f.start << "-" << f.end << ")," << ((f.orientation == Closing)?"/\\":"\\/") << ")";
		}

	};


	struct recursive_unstructured_solver {

		// the function processing frustums recursively
		template<
			typename Kernel,
			typename Set,
			typename Data,
			typename Topology
		>
		void process (const Frustum<Set>& f, Data& dataA, Data& dataB, const Topology& topology) const {
//			std::cout << "Processing " << f << "\n";

			Kernel krnl;

			// handle base case
			if (f.isBaseCase()) {

				// process frustum volume
				f.for_each([&](const auto& ref, int time) {

//					std::cout << "      Updating " << ref << " @ " << time << "\n";

					auto& A = (time % 2) ? dataA : dataB;
					const auto& B = (time % 2) ? dataB : dataA;

					krnl(A[ref], B, topology, ref, time);
				});

				// done
				return;
			}

//			std::cout << "Splitting " << f << " into " << f.split() << "\n";

			// --- <assertions> ---

			std::vector<Frustum<Set>> pieces;
			for(const auto& list : f.split()) {
				for(const auto& cur : list) {
					pieces.push_back(cur);
				}
			}

			using PointSet = decltype(f.getPoints());

			// make sure the frustums are a real partition
			PointSet all = f.getPoints();
			for(const auto& cur : pieces) {
				auto points = cur.getPoints();

				PointSet intersect;
				PointSet extra;
				for(const auto& cur : points) {
					if (all.find(cur) != all.end()) {
						intersect.insert(cur);
						all.erase(cur);
					} else {
						extra.insert(cur);
					}

				}

				if (intersect != points) {
					std::cout << "Frustum " << cur << " not a sub-set of\n"
							     "        " << f << "\n"
							     "  Partition: " << pieces << "\n";
					std::cout << "   cur = " << points << "\n";
					std::cout << "     f = " << f.getPoints() << "\n";
					std::cout << " extra = " << extra << "\n";

					assert(false);
				}

			}
			if (!all.empty()) {
				std::cout << "Frustums " << pieces << " are not a complete partition of " << f << "\n";
				std::cout << "Uncovered elements: " << all << "\n";

				for(const auto& cur : pieces) {
					std::cout << cur << "\n\t" << cur.getPoints() << "\n";
				}
				std::cout << "Full: " << f.getPoints() << "\n";

				assert(false);
			}

			// --- <\assertions> ---

			// split the frustum
			for(const auto& list : f.split()) {
				// TODO: run in parallel
				for(const auto& cur : list) {
					process<Kernel>(cur, dataA, dataB, topology);
				}
			}

		}

		template<
			typename kernel,
			typename node_spec
		>
		void operator()(const utils::Graph<node_spec>& graph, int T) const {

			// create static data container => to be distributed to all nodes
			const auto dataS = graph.createStaticData();

			// get reference to topology
			const auto& topology = graph.getTopologyData();

			// create A and B copy	=> to be dynamically distributed
			auto dataA = graph.createDynamicData();
			auto dataB = graph.createDynamicData();

			// get partitioning scheme of graph
			auto nodes = graph.partition();

			// -- process recursive --

			using Frstum = Frustum<decltype(nodes)>;
			process<kernel>(Frstum(nodes, nodes, 0, T), dataA, dataB, topology);
			return;

//			// split in parts
//			auto parts = nodes.split();
//			assert(parts.size() == 2);
//
//			// get the height limit of the frustums
//			int dt = parts[0].radius();
//			for(const auto& cur : parts) {
//				// TODO: think about infite radus support - component without boundary
//				dt = std::min(dt, cur.radius());
////				dt = std::max(dt, cur.radius());		// TODO: test this - it should work too
//			}
//
//			// process time slices
//			using Frstum = Frustum<decltype(nodes)>;
//			for(int time = 0; time < T ; time += dt) {
//
//				int timeUp = std::min(time + dt, T);
//
//				auto C = parts[0].boundary();
//				for(const auto& cur : parts) {
//					process<kernel>(Frstum(cur, time, timeUp, true), dataA, dataB, topology);
//					C |= cur.boundary();
//				}
//
//				process<kernel>(Frstum(C, time+1, timeUp, false), dataA, dataB, topology);
//			}


		}

	};

} // end namespace stencil
} // end namespace allscale
