#include "allscale_analysis.h"

#include "allscale/utils/vector.h"

using namespace allscale::utils;


void check1D() {

	// check default initialization
	Vector<int,1> a;

//	cba_expect_is_alias(&a.x,&a[0]);

	cba_expect_undefined_int(a[0]);

	// check initialization
	Vector<int,1> b { 7 };

	cba_expect_eq_int(7,b[0]);


//	cba_debug();

}

void check2D() {


	// check default initialization
	Vector<int,2> a;

	// known bug:

//	cba_expect_is_alias(&a.x,&a[0]);
//	cba_expect_is_alias(&a.y,&a[1]);

//	cba_expect_may_alias(&a.x,&a[0]);
//	cba_expect_may_alias(&a.y,&a[1]);

	cba_expect_undefined_int(a[0]);
	cba_expect_undefined_int(a[1]);

	cba_expect_undefined_int(a.x);
	cba_expect_undefined_int(a.y);

	// check initialization
	Vector<int,2> b { 1, 2 };

//	cba_expect_eq_int(1,b[0]);
//	cba_expect_eq_int(2,b[1]);

	cba_expect_eq_int(1,b.x);
	cba_expect_eq_int(2,b.y);

	// known bug:

//	// check mutating assignments
//	Vector<int,2> c;
//
//	c.x = 1;
//	c.y = 2;
//
//	cba_expect_eq_int(1,c.x);
//	cba_expect_eq_int(1,c[0]);
//
//	cba_expect_eq_int(2,c.y);
//	cba_expect_eq_int(2,c[1]);
//
//	c.x = 3;
//
//	cba_expect_eq_int(3,c.x);
//	cba_expect_eq_int(3,c[0]);
//
//	cba_expect_eq_int(2,c.y);
//	cba_expect_eq_int(2,c[1]);
//
//	c[1] = 4;
//
//	cba_expect_eq_int(3,c.x);
//	cba_expect_eq_int(3,c[0]);
//
//	cba_expect_eq_int(4,c.y);
//	cba_expect_eq_int(4,c[1]);


//	cba_debug();

}

void check3D() {


	// check default initialization
	Vector<int,3> a;

//	cba_expect_is_alias(&a.x,&a[0]);

	cba_expect_undefined_int(a[0]);
	cba_expect_undefined_int(a[1]);
	cba_expect_undefined_int(a[2]);

	cba_expect_undefined_int(a.x);
	cba_expect_undefined_int(a.y);
	cba_expect_undefined_int(a.z);

	// check initialization
	Vector<int,3> b { 1, 2, 3 };

//	cba_expect_eq_int(1,b[0]);
//	cba_expect_eq_int(2,b[1]);
//	cba_expect_eq_int(3,b[2]);

	cba_expect_eq_int(1,b.x);
	cba_expect_eq_int(2,b.y);
	cba_expect_eq_int(3,b.z);

//	cba_debug();

}


void check4D() {


	// check default initialization
	Vector<int,4> a;

//	cba_expect_is_alias(&a.x,&a[0]);

	cba_expect_undefined_int(a[0]);
	cba_expect_undefined_int(a[1]);
	cba_expect_undefined_int(a[2]);
	cba_expect_undefined_int(a[3]);

	// check initialization
	Vector<int,4> b { 1, 2, 3, 4 };

	cba_expect_eq_int(1,b[0]);
	cba_expect_eq_int(2,b[1]);
	cba_expect_eq_int(3,b[2]);
	cba_expect_eq_int(4,b[3]);

//	cba_debug();

}


int main() {

	check1D();
	check2D();
	check3D();
	check4D();

	// TODO:
	//  - check field assignment
	//  - check alias tests
	//  - check vector assignment
	//  - check pass by value of full vector
}
