
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
		dependencies dep1 = after();
		treeture<int> a = done(1);
		dependencies dep2 = allscale::api::core::after(a.getTaskReference());
		treeture<int> b = done(1);
		dependencies dep3 = allscale::api::core::after(a.getTaskReference(), b.getTaskReference());
	}

	// methods on dependencies

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>,t>,f,f,plain> v0 = treeture_run(treeture_done(1));
		var ref<dependencies,f,f,plain> v1 = dependency_after(*v0);
		var ref<treeture<int<4>,t>,f,f,plain> v2 = treeture_run(treeture_done(2));
		dependency_add(*v1, *v2);
	})")
	{
		treeture<int> a = done(1);
		dependencies dep1 = allscale::api::core::after(a.getTaskReference());
		treeture<int> b = done(2);
		dep1.add(b.getTaskReference());
	}

	return 0;
}
