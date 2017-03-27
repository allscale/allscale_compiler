// this test checks valid compiler flag parsing, must be compiled 
// with -O3 and expects the coresponding macros to be set or not set
#ifdef NDEBUG
	#error "Expected NDEBUG not be set!"
#endif
#ifndef __OPTIMIZE__
	#error "Expected __OPTIMIZE__ to be set!"
#endif

int main(int argc, char** argv) {
	// nothing to do here
	return 0;
}
