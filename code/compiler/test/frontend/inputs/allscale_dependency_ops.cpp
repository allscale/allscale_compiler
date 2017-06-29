
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

using namespace allscale::api::core;

struct TaskRefConvertible {
	task_reference handle;
	operator task_reference() {
		return handle;
	}
};

int main() {
	; // this is required because of the clang compound source location bug

	// dependency construction

	#pragma test expect_ir(R"({
		var ref<dependencies,f,f,plain> v0 = dependency_after();
		var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(treeture_done(1));
		var ref<dependencies,f,f,plain> v2 = dependency_after(treeture_to_task_ref(*v1));
		var ref<treeture<int<4>,t>,f,f,plain> v3 = treeture_run(treeture_done(1));
		var ref<dependencies,f,f,plain> v4 = dependency_after(treeture_to_task_ref(*v1), treeture_to_task_ref(*v3));
	})")
	{
		auto dep1 = after();
		treeture<int> a = done(1);
		auto dep2 = allscale::api::core::after(a.getTaskReference());
		treeture<int> b = done(1);
		auto dep3 = allscale::api::core::after(a.getTaskReference(), b.getTaskReference());
	}

	// methods on dependencies

	#pragma test expect_ir(R"({
		var ref<dependencies,f,f,plain> v0 = dependency_after();
		var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(treeture_done(1));
		dependency_add(*v0, treeture_to_task_ref(*v1));
		dependency_add(*v0, treeture_to_task_ref(*v1));
	})")
	{
		impl::reference::dependencies<impl::reference::dynamic_sized> dep1 = allscale::api::core::after();
		treeture<int> a = done(1);
		dep1.add(a);
		dep1.add(a.getTaskReference());
	}

	// explicit conversion of arguments to core::after

	#pragma test expect_ir(R"(
		def struct IMP_TaskRefConvertible {
			handle : task_ref;
			function IMP__conversion_operator_task_reference = () -> task_ref {
				return *(this).handle;
			}
		};
		{
			var ref<IMP_TaskRefConvertible,f,f,plain> v0 = IMP_TaskRefConvertible::(ref_decl(type_lit(ref<IMP_TaskRefConvertible,f,f,plain>)));
			dependency_after(v0.IMP__conversion_operator_task_reference());
		}
	)")
	{
		TaskRefConvertible trc;
		allscale::api::core::after(trc);
	}

	return 0;
}
