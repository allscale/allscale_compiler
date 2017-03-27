// this test checks valid compiler flag parsing, must be compiled either with -O0 
// or no -O flag at all and expects the coresponding macros to be set or not set
#ifdef NDEBUG
	#error "Expected NDEBUG not be set!"
#endif
#ifdef __OPTIMIZE__
	#error "Expected __OPTIMIZE__ not to be set!"
#endif

int main(int argc, char** argv) {
	// nothing to do here
	return 0;
}
