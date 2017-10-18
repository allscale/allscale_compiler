#include "allscale_analysis.h"

#include "allscale/api/user/algorithm/pfor.h"
#include "allscale/utils/vector.h"

using namespace allscale::api::user::algorithm;

int main() {

	using point = std::array<int,2>;

	{
		// the creation of a range should not cause data requirements
		cba_expect_data_requirements("{}");
		detail::range<point> r(point{1,2},point{2,3});
	}

	{
		// computation of a volume should not cause data requirements
		cba_expect_data_requirements("{}");
		detail::volume<point>()(point{1,2},point{2,3});
	}

	{
		using vector = allscale::utils::Vector<int,2>;

		// compute the volume between vectors
		cba_expect_data_requirements("{}");
		detail::volume<vector>()(vector{1,2},vector{2,3});
	}

	{
		// test the creation of a range from points
		cba_expect_data_requirements("{}");
		auto r = detail::range<point>(point{1,2},point{1,2});
	}

	{
		// the make_fragments function
		cba_expect_data_requirements("{}");
		auto r = detail::range<point>(point{1,2},point{2,3});
		detail::make_fragments(r,r);
	}

	{
		// test the range split operation
		cba_expect_data_requirements("{}");
		detail::range_spliter<point>::split(0,detail::range<point>(point{1,2},point{2,3}));
	}

	return 0;
}
