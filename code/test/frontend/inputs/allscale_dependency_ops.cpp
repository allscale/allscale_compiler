
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	// dependency construction

	#pragma test expect_ir(R"({
		var ref<list<treeture<'_dT,'_dR>>,f,f,plain> v0 = dependency_after();
		var ref<treeture<int<4>,t>,f,f,plain> v1 = treeture_run(treeture_done(1));
		var ref<list<treeture<'_dT,'_dR>>,f,f,plain> v2 = dependency_after(v1);
	})")
	{
		dependencies dep1 = after();
		treeture<int> a = done(1);
		dependencies dep = allscale::api::core::after(a.getTaskReference());
	}

	// methods on dependencies

	return 0;
}
