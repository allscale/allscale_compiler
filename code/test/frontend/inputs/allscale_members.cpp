

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

struct S {
	impl::reference::treeture<int> t;

	void foo() {
		t.get();
	}
};

int main() {
	; // this is required because of the clang compound source location bug

	// this test will make sure that members which are AllScale types are handled correctly
	// it will also fail if we deref or dematerialize builtins
	#pragma test expect_ir(R"(
		def struct IMP_S {
			t : treeture<int<4>,t>;
			function IMP_foo = () -> unit {
				treeture_get(*(this).t);
			}
		};
		{
			var ref<IMP_S,f,f,plain> v141 = IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
			v141.IMP_foo();
		}
	)")
	{
		S s;
		s.foo();
	}

	return 0;
}
