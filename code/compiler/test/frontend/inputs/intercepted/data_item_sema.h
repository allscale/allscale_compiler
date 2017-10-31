
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

struct NonTrivialDestructor {
	int *p;

	NonTrivialDestructor() {
		p = new int();
	}

	~NonTrivialDestructor() {
		delete p;
	}

	double v() {
		return 0.0;
	}

};


// The simplest imaginable data item

class SimplestDI {

	double data[100];

public:
	using range_type = int;

	SimplestDI() {}

	double get() const {
		return data_item_element_access(*this, 0, data[0]);
	}

	double getInvolvingCleanups() const {
		return data_item_element_access(*this, 0, NonTrivialDestructor().v());
	}

	const double& operator[](int i) const {
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

	double data[100];

public:
	using range_type = NonBaseRange;

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

public:
	TemplatedRange(T a, T b, T c) {
		indices[0] = a;
		indices[1] = b;
		indices[2] = c;
	}
};

template<typename T, typename INDEX_T>
class TemplatedDI {

	T data[100];

public:
	using range_type = TemplatedRange<INDEX_T>;

	TemplatedDI() {}

	const T& operator[](INDEX_T i) const {
		return data_item_element_access(*this, { i, i * 2, i + 3 }, data[i]);
	}

	T& operator[](INDEX_T i) {
		return data_item_element_access(*this, { i, i * 2, i + 3 }, data[i]);
	}

	void someOtherMethod() const {
		shouldNeverBeInIR(3);
	}
};
