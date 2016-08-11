#pragma once

#include "allscale/stencil/stencil.h"
#include "allscale/utils/grid.h"
#include "allscale/utils/stream.h"

namespace allscale {
namespace stencil {
namespace solver {

	namespace detail {

		// ---- parameters -----------------------------------------------------

		template<typename ... ParameterSpecs>
		struct parameter_container;

		template<
			typename ValueType,
			typename ... Rest
		>
		struct parameter_container<ValueType,Rest...> {

			ValueType value;

			parameter_container<Rest...> nested;

			parameter_container(const ValueType& cur, const Rest& ... rest)
				: value(cur), nested(rest...) {}

			template<typename F, typename ... Args>
			void call(F& fun, Args& ... args) const {
				nested.template call(fun, args..., value);
			}

		};

		template<
			typename Cell,
			size_t dims,
			typename Init,
			template<typename,size_t> class Boundary,
			typename ... Rest
		>
		struct parameter_container<ConstantGrid<Cell,dims,Init,Boundary>,Rest...> {

			utils::Grid<Cell,dims,Boundary<Cell,dims>> a;

			parameter_container<Rest...> nested;

			parameter_container(const ConstantGrid<Cell,dims,Init,Boundary>& cur, const Rest& ... rest)
				: a(cur.size), nested(rest...) {

				// initialize the constant grid
				Init init = Init();
				utils::pfor(cur.size,[&](const auto& pos) {
					a[pos] = init(pos);
				});

			}

			template<typename F, typename ... Args>
			void call(F& fun, Args& ... args) const {
				nested.template call(fun, args..., a);
			}

		};

		template<
			typename Cell,
			size_t dims,
			typename Loader,
			typename ... Rest
		>
		struct parameter_container<DataStream<Cell,dims,Loader>,Rest...> {

			utils::Stream<Cell,dims,Loader> s;

			parameter_container<Rest...> nested;

			parameter_container(const DataStream<Cell,dims,Loader>& cur, const Rest& ... rest)
				: nested(rest...) {}

			template<typename F, typename ... Args>
			void call(F& fun, Args& ... args) const {
				nested.template call(fun, args..., s);
			}
		};


		template<>
		struct parameter_container<> {
			parameter_container() {}

			template<typename F, typename ... Args>
			void call(F& fun, Args& ... args) const {
				fun(args...);
			}
		};


		// ---- observers ---------------------------------------------------------

		template<typename Observer>
		struct observer_handler;

		template<typename Filter, typename Operator, typename ... Rest>
		struct observer_handler<observers<observer<Filter,Operator>, Rest...>> {

			Filter filter;

			Operator op;

			observer_handler<observers<Rest...>> nested;

			template<typename Cell, size_t Dim, typename ParamHandler>
			void process(const Cell& cell, const utils::Size<Dim>& pos, int t, const ParamHandler& params) const {
				if (filter(pos,t)) params.call(op, cell, pos, t);
				nested.process(cell,pos,t,params);
			}

		};

		template<>
		struct observer_handler<observers<>> {
			template<typename Cell, size_t Dim, typename ParamHandler>
			void process(const Cell& cell, const utils::Size<Dim>& pos, int t, const ParamHandler&) const {}
		};

	}


	template<typename Kernel, typename Observers>
	struct iterative_solver {
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

			// run the stencil
			for(int t=0; t<timeSteps; t++) {

				auto& la = (t%2) ? a : b;
				auto& lb = (t%2) ? b : a;

				// process layer in parallel
				utils::pfor(size, [&](const auto& pos){
					params.call(step, la[pos], lb, pos, t);
					observers.process(la[pos], pos, t+1, params);			// the t+1 version has been computed in this step
				});
			}

			// done
		}
	};

} // end namespace solver
} // end namespace stencil
} // end namespace allscale
