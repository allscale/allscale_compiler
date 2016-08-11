#pragma once

#include <cassert>
#include <array>
#include <ostream>
#include <algorithm>
#include <initializer_list>

#include "allscale/utils/static_grid.h"

#include "stencil/stencil.h"
#include "stencil/solver/recursive.h"

namespace allscale {

	// -- modeling primitives --

	template<size_t ... size>
	struct layer {
		static const int dims = sizeof...(size);

		template<typename Cell>
		using grid = utils::grid<Cell,size...>;
	};


	// -- addressing sub-cells --

	template<typename ... layers>
	class AdaptiveGridCellAddress;

	template<size_t ... size, typename ... Rest>
	class AdaptiveGridCellAddress<layer<size...>,Rest...> {
	public:

		using grid_address = utils::grid_address<size...>;
		using grid_offset = typename grid_address::offset;

		bool stop = true;
		grid_address pos;
		AdaptiveGridCellAddress<Rest...> nested;

		int getDepth() const {
			if (stop) return 0;
			return nested.getDepth() + 1;
		}

		void descent_internal() {
			if (stop) {
				this->pos = grid_address();
				this->stop = false;
				return;
			}
			nested.descent_internal();
		}

		void descent(grid_offset pos = grid_offset()) {
			descent_internal();
			move(pos);
		}

		void ascent() {
			if (nested.stop) {
				stop = true;
				return;
			}
			nested.ascent();
		}

		void move(grid_offset& offset) {


			if (nested.stop) {
				pos.move(offset);
			} else {
				nested.move(offset);
				pos.move(offset);
			}
		}

		bool operator==(const AdaptiveGridCellAddress& other) const {
			return (stop && other.stop) || (!stop && !other.stop && pos == other.pos && nested == other.nested);
		}

		AdaptiveGridCellAddress& operator+=(grid_offset offset) {
			move(offset);
			return *this;
		}

		AdaptiveGridCellAddress& operator-=(const grid_offset& offset) {
			grid_offset tmp = -offset;
			move(tmp);
			return *this;
		}
	};

	template<>
	class AdaptiveGridCellAddress<> {
	public:
		static const bool stop = true;

		int getDepth() const {
			return 0;
		}

		void descent_internal() {}

		template<typename A>
		void descent(const A&) {
			descent();
		}
		void descent() {
			assert(false && "Unable to descent further!");
		}
		void ascent() {
			assert(false && "Should not be reachable!");
		}

		template<typename O>
		void move(O&) {
			assert(false && "Unable to update address!");
		}

		bool operator==(const AdaptiveGridCellAddress& other) const {
			return true;
		}

		template<typename A>
		AdaptiveGridCellAddress& operator+=(const A&) {
			assert(false && "Unable to update address!");
			return *this;
		}

		template<typename A>
		AdaptiveGridCellAddress& operator-=(const A&) {
			assert(false && "Unable to update address!");
			return *this;
		}
	};

	template<typename ... layers>
	std::ostream& operator<<(std::ostream& out, const AdaptiveGridCellAddress<layers...>& addr) {
		if (addr.stop) return out;
		return out << "[" << addr.pos << "]" << addr.nested;
	}

	template<size_t ... size>
	std::ostream& operator<<(std::ostream& out, const AdaptiveGridCellAddress<layer<size...>>& addr) {
		if (addr.stop) return out;
		return out << "[" << addr.pos << "]";
	}

	inline std::ostream& operator<<(std::ostream& out, const AdaptiveGridCellAddress<>& addr) {
		return out << "";
	}



	// -- a adaptive grid cell --

	template<
		typename Cell,
		typename Refiner,
		typename Coarsener,
		typename ... layers
	>
	class AdaptiveGridCell;

	template<
		typename Cell,
		typename Refiner,
		typename Coarsener,
		size_t ... size,
		typename ... Rest
	>
	class AdaptiveGridCell<Cell,Refiner,Coarsener,layer<size...>,Rest...> {

	public:

		using address = AdaptiveGridCellAddress<layer<size...>,Rest...>;

	private:

		using nested_cell_type = AdaptiveGridCell<Cell,Refiner,Coarsener,Rest...>;
		using grid_type = typename layer<size...>::template grid<nested_cell_type>;

		grid_type nested;

		Cell data;

		bool active;

	public:

		AdaptiveGridCell(const Cell& cell = Cell()) : data(cell), active(true) {}

		void operator=(const Cell& value) {
			data = value;
		}

		operator Cell&() {
			if (!active) coarsen_data();
			return data;
		}

		operator const Cell&() const {
			if (!active) coarsen_data();
			return data;
		}

		Cell& operator[](const address& addr) {
			if (addr.stop) {
				// obtain value for this cell
				if (!active) coarsen_data();
				return data;
			}

			// if this level is the active level
			if (active) refine_data();

			// access nested
			return nested[addr.pos][addr.nested];
		}

		const Cell& operator[](const address& addr) const {
			return const_cast<AdaptiveGridCell*>(this)->operator[](addr);
		}

		void refine_data() const {
			// refine data
			Refiner r;
			r.template operator()(const_cast<grid_type&>(nested), data);
		}

		void coarsen_data() const {
			// coarsen data
			Coarsener c;
			c.template operator()(const_cast<Cell&>(data), nested);
		}

		void refine(const address& addr = address()) {

			// test whether addressed level has been reached
			if (addr.stop) {

				// if active => refine
				if (active) {
					// push in data
					refine_data();
					// disable this level
					active = false;
				}

				// done
				return;
			}

			// refine nested structure
			nested[addr.pos].refine(addr.nested);
		}

		void coarsen(const address& addr = address()) {

			// test whether addressed level has been reached
			if (addr.stop) {

				// skip if already active
				if (active) return;

				// activate this level
				coarsen_data();
				active = true;

				// done
				return;
			}

			// coarsen some nested structure
			nested[addr.pos].coarsen(addr.nested);
		}

		void assignRefinement(const AdaptiveGridCell& other) {
			// TODO: just move active flags
			*this = other;
		}

		template<typename Lambda>
		void for_each_sub_address(address& addr, const Lambda& lambda) const {
			// run on this
			lambda();

			// run for nested nodes
			addr.descent();
			nested.for_each(addr.pos, [&](const nested_cell_type& cell) {
				cell.for_each_sub_address(addr.nested, lambda);
			});
			addr.ascent();
		}

		template<typename Lambda>
		void for_each_active_sub_address(address& addr, const Lambda& lambda) const {
			// run on this
			if (active) {
				lambda();
				return;
			}

			// run for nested nodes
			addr.descent();
			nested.for_each(addr.pos, [&](const nested_cell_type& cell) {
				cell.for_each_active_sub_address(addr.nested, lambda);
			});
			addr.ascent();
		}

		std::vector<address> getActiveSubAddresses() const {
			std::vector<address> res;
			address tmp;
			for_each_active_sub_address(tmp, [&](){
				res.push_back(tmp);
			});
			return res;
		}

		friend std::ostream& operator<<(std::ostream& out, const AdaptiveGridCell& cell) {
			address addr;

			auto activeAddresses = cell.getActiveSubAddresses();

			auto isActive = [&](const address& addr) {
				return std::find(activeAddresses.begin(), activeAddresses.end(), addr) != activeAddresses.end();
			};

//			cell.for_each_sub_address(addr, [&](){
			cell.for_each_active_sub_address(addr, [&](){
				out << addr << " => " << cell[addr] << ((isActive(addr))?" active":"") << "\n";
			});
			return out;
		}
	};

	template<
		typename Cell,
		typename Refiner,
		typename Coarsener
	>
	class AdaptiveGridCell<Cell,Refiner,Coarsener> {

	public:

		using address = AdaptiveGridCellAddress<>;

	private:

		Cell cell;

	public:

		static const bool active = true;

		AdaptiveGridCell() {}

		AdaptiveGridCell(const Cell& cell) : cell(cell) {}

		void operator=(const Cell& value) {
			cell = value;
		}

		operator Cell&() {
			return cell;
		}

		operator const Cell&() const {
			return cell;
		}

		Cell& operator[](const address&) {
			return cell;
		}

		const Cell& operator[](const address&) const {
			return cell;
		}

		void refine_data() const {
			// nothing to do
		}

		void coarsen_data() const {
			// nothing to do
		}

		void refine(const address& addr = address()) const {
			// nothing to do
		}

		void coarsen(const address& addr = address()) {
			// nothing to do
		}

		void assignRefinement(const AdaptiveGridCell& other) {
			// nothing to do
		}

		template<typename Lambda>
		void for_each_sub_address(address& addr, const Lambda& lambda) const {
			// run on this
			lambda();
		}

		template<typename Lambda>
		void for_each_active_sub_address(address& addr, const Lambda& lambda) const {
			// run on this
			lambda();
		}

		std::vector<address> getActiveSubAddresses() const {
			std::vector<address> res;
			address tmp;
			res.push_back(tmp);
			return res;
		}

		friend std::ostream& operator<<(std::ostream& out, const AdaptiveGridCell& cell) {
			return out;
		}
	};

	namespace detail {

		template<typename ... Ts>
		struct first;

		template<typename F, typename ... Rest>
		struct first<F,Rest...> {
			using type = F;
		};

	}


	template<typename ... lyers>
	struct layers {
		template<typename Cell, typename Refiner, typename Coarsener>
		using cell_type = AdaptiveGridCell<Cell,Refiner,Coarsener,lyers...>;
		using addr_type = AdaptiveGridCellAddress<lyers...>;
		static const int dimensions = detail::first<lyers...>::type::dims;
	};

	template<typename Layers, size_t dims>
	struct AdaptiveGridAddress {

		using cell_sub_address = typename Layers::addr_type;
		using offset_type = typename cell_sub_address::grid_offset;

		utils::Coordinate<dims> pos;
		cell_sub_address sub;

		AdaptiveGridAddress() : pos(0), sub() {}

		AdaptiveGridAddress(const utils::Coordinate<dims>& pos)
			: pos(pos), sub() {}

		int getDepth() const {
			return sub.getDepth();
		}

		AdaptiveGridAddress operator+(const offset_type& offset) {
			return AdaptiveGridAddress(*this) += offset;
		}

		AdaptiveGridAddress& operator+=(offset_type offset) {
			if (!sub.stop) sub.move(offset);
			move(offset);
			return *this;
		}

		AdaptiveGridAddress& descent(const offset_type& offset = offset_type()) {
			sub.descent(offset);
			return *this;
		}

		AdaptiveGridAddress& ascent() {
			sub.ascent();
			return *this;
		}

	private:

		template<size_t i>
		struct position {};

		template<size_t x, typename offset>
		void move_internal(const offset& off, position<x>) {
			pos[dims-x] += off.pos;
			move_internal(off.nested,position<x-1>());
		}

		template<typename offset>
		void move_internal(const offset& off, position<0>) {
			// nothing
		}

		template<typename offset>
		void move(const offset& off) {
			move_internal(off, position<dims>());
		}

	};


	template<typename Layers, size_t dims>
	std::ostream& operator<<(std::ostream& out, const AdaptiveGridAddress<Layers,dims>& addr) {
		return out << addr.pos << addr.sub;
	}

	template<
		typename Cell, typename Refiner, typename Coarsener, typename Layers, size_t dims,
		template<typename,size_t> class BoundaryCondition
	>
	struct AdaptiveGrid {

		using cell_type = typename Layers::template cell_type<Cell,Refiner,Coarsener>;
		using grid_type = utils::Grid<cell_type,dims,BoundaryCondition<cell_type,dims>>;
		using addr_type = AdaptiveGridAddress<Layers,dims>;

		const grid_type& grid;

		AdaptiveGrid(const grid_type& grid) : grid(grid) {}

		Cell operator[](const addr_type& addr) const {
			return grid[addr.pos][addr.sub];
		}

	};


	template<
		typename Cell,
		template<typename,size_t> class BoundaryCondition,
		typename Layers,
		typename Refiner,
		typename Coarsener,
		size_t dims = Layers::dimensions
	>
	struct adaptive_grid_spec {
		using cell_type = Cell;
		using ag_cell_type = typename Layers::template cell_type<cell_type,Refiner,Coarsener>;
		using grid_type = utils::Grid<ag_cell_type,dims,BoundaryCondition<ag_cell_type,dims>>;
		using ag_grid_type = AdaptiveGrid<Cell,Refiner,Coarsener,Layers,dims,BoundaryCondition>;
		static const size_t dimensions = dims;

		template<typename T, size_t d>
		using boundary_condition = BoundaryCondition<T,d>;

		template<typename Init>
		using variable_grid_spec = stencil::VariableGrid<ag_cell_type,dims,Init,BoundaryCondition>;

	};


	template<typename ag_spec>
	struct AdaptiveGridOperator {
		using cell_type = typename ag_spec::cell_type;
		using grid_type = typename ag_spec::ag_grid_type;
		using ag_cell_type = typename grid_type::cell_type;
		using pos_type = typename grid_type::addr_type;
	};

	template<typename ag_spec>
	struct AdaptiveGridKernel
			: public AdaptiveGridOperator<ag_spec> {

		using super = AdaptiveGridOperator<ag_spec>;
		using typename super::cell_type;
		using typename super::grid_type;
		using typename super::pos_type;

	};

	template<typename ag_spec>
	struct AdaptiveGridInit : public AdaptiveGridOperator<ag_spec> {

		using super = AdaptiveGridOperator<ag_spec>;
		using typename super::cell_type;
		using typename super::grid_type;
		using typename super::pos_type;

	};


	enum class AdaptiveGridAction {
		Refine, None, Coarsen
	};

	template<typename grid_spec>
	struct AdaptiveGridAdjuster : public AdaptiveGridOperator<grid_spec> {

		using super = AdaptiveGridOperator<grid_spec>;
		using typename super::cell_type;
		using typename super::grid_type;
		using typename super::pos_type;

	};

	template<typename grid_spec>
	struct AdaptiveGridObserver : public AdaptiveGridOperator<grid_spec> {

		using super = AdaptiveGridOperator<grid_spec>;
		using typename super::cell_type;
		using typename super::grid_type;
		using typename super::pos_type;

	};

	template<typename grid_spec>
	struct AdaptiveGridDataAssimilator : public AdaptiveGridOperator<grid_spec> {

		using super = AdaptiveGridOperator<grid_spec>;
		using typename super::cell_type;
		using typename super::grid_type;
		using typename super::pos_type;

	};

	namespace detail {

		template<
			typename Kernel,
			typename AdaptiveGrid,
			typename AdaptiveGridAddress,
			typename DataAssimilator,
			typename ResolutionAdjuster
		>
		struct kernel_wrapper {
			template<typename Cell, typename Grid, typename Coordinate, typename ... Params>
			void operator()(Cell& res, const Grid& grid, const Coordinate& pos, int time, const Params& ... parameters) const {
				Kernel krnel;

				// create adaptive grid view on grid
				AdaptiveGrid agrid(grid);

				// move refinement information to next time step
				res.assignRefinement(grid[pos]);

				// update sub-elements
				AdaptiveGridAddress addr = pos;

				// this would be the fast way -- but it causes GCC to seg-fault
//				grid[addr.pos].template for_each_active_sub_address(addr.sub, [&]()->void {
//					krnel(res[addr.sub], mgrid, addr, time);
//				});

				auto subAddresses = grid[pos].getActiveSubAddresses();

				// the slower version -- materializing all sub-addresses
				for(const auto& cur : subAddresses) {
					addr.sub = cur;
					krnel(res[cur], agrid, addr, time, parameters...);
				}

				// adjust the grid
				ResolutionAdjuster adjustr;
				for(const auto& cur : subAddresses) {
					addr.sub = cur;
					switch(adjustr(res[cur],agrid,addr,time, parameters...)) {
						case AdaptiveGridAction::Refine: {
							res.refine(cur);
							break;
						}
						case AdaptiveGridAction::None:	{
							break;
						}
						case AdaptiveGridAction::Coarsen: {
							auto tmp = cur;
							tmp.ascent();
							res.coarsen(tmp);
							break;
						}
					}
				}

				// finally: assimilate data
				DataAssimilator assimilator;
				assimilator(res, agrid, pos, time, parameters...);

			}
		};
	}


	template<
		typename grid_spec,
		typename kernel,
		typename initializer,
		typename assimilator,
		typename adjuster,
		typename observers = stencil::observers<>,
		typename ... Parameters
	>
	void solve_adaptive_grid(const utils::Size<grid_spec::dimensions>& size, int timeSteps, const Parameters& ... parameters) {

		static const size_t dims = grid_spec::dimensions;

		using ag_cell_type = typename grid_spec::ag_cell_type;
		using ag_grid_type = typename grid_spec::ag_grid_type;
		using ag_grid_addr = typename ag_grid_type::addr_type;

		struct init_wrapper {
			ag_cell_type operator()(const utils::Coordinate<dims>& pos) const {
				return initializer()(pos);
			}
		};

		// create variable grid
		typename grid_spec::template variable_grid_spec<init_wrapper> varGrid(size);

		stencil::solve<
			detail::kernel_wrapper<kernel,ag_grid_type,ag_grid_addr,assimilator,adjuster>,
			observers,
			stencil::solver::simple_recursive_solver
		>(timeSteps, varGrid, parameters...);

	}


} // end namespace allscale
