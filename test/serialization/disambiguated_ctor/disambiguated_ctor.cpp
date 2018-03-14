#include <iostream>

#include <allscale/api/user/algorithm/pfor.h>
#include <allscale/api/user/data/grid.h>

using namespace allscale::api::user::algorithm;
using namespace allscale::api::user::data;
using namespace allscale::utils;

struct NoCtors {
	int a;
	int b;
};

struct DefaultCtor {
	int a;
	int b;

	DefaultCtor() {}
};

struct DefaultArgsCtor {
	int a;
	int b;

	DefaultArgsCtor(int a = 0, int b = 0) {}
};

struct MultipleCtors {
	int a;
	int b;

	MultipleCtors() {}
	MultipleCtors(int a, int b) {}
};



int main() {

	NoCtors d0;
	DefaultCtor d1;
	DefaultArgsCtor d2;
	MultipleCtors d3;

	return 0;
}
