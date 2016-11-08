#include <gtest/gtest.h>

#include "allscale/name.h"

namespace allscale {

	TEST(Allscale, Basic) {
		ASSERT_EQ("AllScale", name());
	}

} // end namespace allscale
