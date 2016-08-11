#pragma once

namespace allscale {
namespace utils {

	template<
		typename T,
		size_t Dims,
		typename Loader
	>
	struct Stream {

		template<typename ... ExtraParams>
		T get(int time, const Coordinate<Dims>& pos, const ExtraParams& ... params) const {
			return Loader()(time,pos,params...);
		}

	};

} // end namespace utils
} // end namespace allscale
