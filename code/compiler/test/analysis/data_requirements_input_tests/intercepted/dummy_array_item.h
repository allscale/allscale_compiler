#pragma once

#include "intercepted/dummy_item_interface.h"

struct DummyArrayItem {
	using range_type = int;

	int x;

	int& get(int i) {
		return data_item_element_access(*this,i,x);
	}

	const int& get(int i) const {
		return data_item_element_access(*this,i,x);
	}

	int& operator[](int i) {
		return data_item_element_access(*this,i,x);
	}

	const int& operator[](int i) const {
		return data_item_element_access(*this,i,x);
	}
};
