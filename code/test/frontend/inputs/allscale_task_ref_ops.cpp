
#include "allscale/api/core/treeture.h"


using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// CREATION/CONVERSION ////
	#pragma test expect_ir(R"({
		var ref<task_ref,f,f,plain> v0 = lit("task_ref::ctor" : task_ref::())(ref_decl(type_lit(ref<task_ref,f,f,plain>)));
		var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(treeture_done(1));
		var ref<task_ref,f,f,plain> v2 = treeture_to_task_ref(*v1);
		var ref<task_ref,f,f,plain> v3 = treeture_to_task_ref(*v1);
	})")
	{
		impl::reference::task_reference tr0;
		impl::reference::treeture<int> t = done(1);
		impl::reference::task_reference tr1 = t;
		impl::reference::task_reference tr2 = t.getTaskReference();
	}


	/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// METHODS ////
	#pragma test expect_ir(R"({
		var ref<task_ref,f,f,plain> v0 = lit("task_ref::ctor" : task_ref::())(ref_decl(type_lit(ref<task_ref,f,f,plain>)));
		task_ref_left(*v0);
		task_ref_right(*v0);
		task_ref_wait(*v0);
	})")
	{
		impl::reference::task_reference tr;
		tr.getLeft();
		tr.getRight();
		tr.wait();
	}

	return 0;
}
