

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

struct S {
	impl::reference::treeture<int> t;

	impl::reference::task_reference handle;

	void foo() {
		t.get();
	}

	impl::reference::task_reference getHandle() const {
		return handle;
	}
};

int main() {
	; // this is required because of the clang compound source location bug

	// this test will make sure that members which are AllScale types are handled correctly
	// it will also fail if we deref or dematerialize builtins
	// additionally, this test will ensure correct handling of copy semantics of AllScale types
	#pragma test expect_ir(R"(
		def struct IMP_S {
			t : treeture<int<4>,t>;
			handle : task_ref;
			function IMP_foo = () -> unit {
				treeture_get(*(this).t);
			}
			const function IMP_getHandle = () -> task_ref {
				return *(this).handle;
			}
		};
		{
			var ref<IMP_S,f,f,plain> v141 = IMP_S::(ref_decl(type_lit(ref<IMP_S,f,f,plain>)));
			v141.IMP_foo();
			v141.IMP_getHandle();
		}
	)")
	{
		S s;
		s.foo();
		s.getHandle();
	}

	return 0;
}
