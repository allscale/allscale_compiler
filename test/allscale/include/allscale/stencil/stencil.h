#pragma once

#include <functional>

#include "allscale/utils/grid.h"

namespace allscale {
namespace stencil {

	template<typename Cell>
	struct default_init {
		Cell operator()(int) const {
			return Cell();
		}
	};


	// -- stencil parameters --

	template<
		typename Cell,
		size_t Dimensions,
		typename Initializer,
		template<typename,size_t> class BoundaryCondition
	>
	struct VariableGrid {
		utils::Size<Dimensions> size;
		VariableGrid() : size(0) {};
		VariableGrid(const utils::Size<Dimensions>& size) : size(size) {};
	};

	template<
		typename Cell,
		size_t Dimensions,
		typename Initializer,
		template<typename,size_t> class BoundaryCondition
	>
	struct ConstantGrid {
		utils::Size<Dimensions> size;
		ConstantGrid(const utils::Size<Dimensions>& size) : size(size) {};
	};

	template<
		typename Cell,
		size_t Dimensions,
		typename Loader
	>
	struct DataStream {
		utils::Size<Dimensions> size;			// size of each sample
		DataStream(const utils::Size<Dimensions>& size) : size(size) {}
	};


	// -- observer --

	template<typename Filter, typename Operation>
	struct observer {
		using filter_type = Filter;
		using operator_type = Operation;
	};

	template<typename ... Operators>
	struct observers {};

	template<size_t time>
	struct time_step_filter {
		template<typename Size>
		bool operator()(const Size&, int t) const {
			return t == time;
		}
	};

	template<size_t time>
	struct periodic_time_step_filter {
		template<typename Size>
		bool operator()(const Size&, int t) const {
			return t % time == 0;
		}
	};


	// -- solver driver --

	namespace solver {
		// forward-declaration of default solver
		template<typename Kernel, typename Observers>
		struct iterative_solver;
	}

	template<
		typename Kernel,
		typename Observers = observers<>,
		template<typename,typename> class solver = solver::iterative_solver,
		typename Cell,
		size_t Dimensions,
		typename Initializer,
		template<typename,size_t> class BoundaryCondition,
		typename ... ConstantsAndStreams
	>
	void solve(
			int timeSteps,
			const VariableGrid<Cell,Dimensions,Initializer,BoundaryCondition>& varGrid,
			const ConstantsAndStreams& ... parameters
		) {

		// make sure this is a sensible request
		if (timeSteps == 0) return;

		// quick exit
		if (utils::area(varGrid.size) <= 0) {
			std::cerr << "WARNING: applying stencil on empty hyper-volume!\n";
			return;
		}

		// run the solver
		solver<Kernel,Observers>()(timeSteps, varGrid, parameters...);

		// that's it
	}


} // end namespace stencil
} // end namespace allscale

// the default solver - needs to be included after the interface!
#include "allscale/stencil/solver/iterative.h"
