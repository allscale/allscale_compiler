

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


const task_reference& getConstTaskReferenceReference() {
	task_reference* handle;
	return *handle;
}

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

	// this test will ensure, that we dematerialize correctly
	// this used to call problems when we have an lvalue and this is copied/moved. In this case the dematerialization didn't work correctly
	#pragma test expect_ir(R"(
		def IMP_getConstTaskReferenceReference = function () -> ref<task_ref,t,f,cpp_ref> {
			var ref<ptr<task_ref>,f,f,plain> v0 = ref_decl(type_lit(ref<ptr<task_ref>,f,f,plain>));
			return ref_kind_cast(ptr_to_ref(*v0), type_lit(cpp_ref));
		};
		{
			var ref<task_ref,f,f,plain> v0 = *IMP_getConstTaskReferenceReference();
		}
	)")
	{
		auto var = getConstTaskReferenceReference();
	}

	return 0;
}
