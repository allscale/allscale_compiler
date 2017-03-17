
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// dependency construction

	#pragma test expect_ir(R"({
		var ref<dependencies,f,f,plain> v0 = dependency_after();
		var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(treeture_done(1));
		var ref<dependencies,f,f,plain> v2 = dependency_after(*v1);
		var ref<treeture<int<4>,t>,f,f,plain> v3 = treeture_run(treeture_done(1));
		var ref<dependencies,f,f,plain> v4 = dependency_after(*v1, *v3);
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
		dependency_add(*v0, *v1);
	})")
	{
		impl::reference::dependencies<impl::reference::dynamic_sized> dep1 = allscale::api::core::after();
		treeture<int> a = done(1);
		dep1.add(a.getTaskReference());
	}

	return 0;
}
