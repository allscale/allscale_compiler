#pragma once

#include "data_requirements/intercepted/dummy_item_interface.h"

struct DummyScalarItem {
	using range_type = int;

	int x;

	int& get() {
		return data_item_element_access(*this,0,x);
	}

	const int& get() const {
		return data_item_element_access(*this,0,x);
	}
};
