
#pragma once

void shouldNeverBeInIR(int x) { }

template<typename DataItem, typename T>
T& data_item_element_access(DataItem& item, const typename DataItem::range_type& range, T& ref) {
	return ref;
}

template<typename DataItem, typename T>
const T& data_item_element_access(const DataItem& item, const typename DataItem::range_type& range, const T& ref) {
	return ref;
}

// The simplest imaginable data item

class SimplestDI {

	using range_type = int;
	double data[100];

public:
	SimplestDI() {}

	double get() const {
		return data_item_element_access(*this, 0, data[0]);
	}

	const int& operator[](int i) const {
		return data_item_element_access(*this, i, data[i]);
	}

	double& operator[](int i) {
		return data_item_element_access(*this, i, data[i]);
	}

	void someOtherMethod() const {
		shouldNeverBeInIR(1);
	}
};

// A data item with a range type which is not a basic type

struct NonBaseRange {
	int left, right;
	NonBaseRange(int a, int b)
		: left(a)
		, right(b)
	{ }
};

class NonBaseRangeDI {

	using range_type = NonBaseRange;
	double data[100];

public:
	NonBaseRangeDI() {}

	const double& operator[](int i) const {
		return data_item_element_access(*this, { i, i + 1 }, data[i]);
	}

	double& operator[](int i) {
		return data_item_element_access(*this, { i - 1, i }, data[i]);
	}

	void someOtherMethod() const {
		shouldNeverBeInIR(2);
	}
};

// A templated data item with a templated base type

template<typename T>
class TemplatedRange {
	T indices[3];

	TemplatedRange(T a, T b, T c) {
		indices[0] = a;
		indices[1] = b;
		indices[2] = c;
	}
};

template<typename T, typename INDEX_T>
class TemplatedDI {

	using range_type = TemplatedRange<INDEX_T>;
	T data[100];

public:
	TemplatedDI() {}

	const double& operator[](INDEX_T i, INDEX_T j) const {
		return data_item_element_access(*this, { i, j, i + j }, data[i]);
	}

	double& operator[](INDEX_T i, INDEX_T j) {
		return data_item_element_access(*this, { i, j, i + j }, data[i]);
	}

	void someOtherMethod() const {
		shouldNeverBeInIR(3);
	}
};
