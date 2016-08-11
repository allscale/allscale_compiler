#pragma once

#include <type_traits>
#include <future>

#include "parec/core.h"
#include "parec/futures.h"

namespace parec {

	/**
	 * A simple version of an async operator based on the prec operator.
	 */
	template< class Function>
	utils::runtime::Future<typename std::result_of<Function()>::type>
	async(const Function& f ) {
		struct empty {};
		typedef typename std::result_of<Function()>::type res_type;

		// maps the operation to a recursion
		return prec(
				[](empty)->bool { return true; },
				[=](empty)->res_type { return f(); },
				[](empty, const auto&)->res_type { return res_type(); }
		)(empty());
	}

} // end namespace parec
