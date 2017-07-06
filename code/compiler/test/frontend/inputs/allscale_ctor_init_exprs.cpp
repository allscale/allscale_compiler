
#include "allscale/api/core/treeture.h"


using namespace allscale::api::core;

class RefConstructExpr {
	impl::reference::task_reference y;
public:
	RefConstructExpr(const impl::reference::task_reference& y) : y(y) {}
};

int main() {
	; // this is required because of the clang compound source location bug

	// we test the creation of initExprs for initializing fields. The default behavior for AllScale types would not generate these.
	#pragma test expect_ir(R"(
		def struct IMP_RefConstructExpr {
			y : task_ref;
			ctor function (v1 : ref<task_ref,t,f,cpp_ref>) {
				<ref<task_ref,f,f,plain>>((this).y) {*v1};
			}
		};
		{
			var ref<task_ref,f,f,plain> v0 = task_ref_done();
			var ref<IMP_RefConstructExpr,f,f,plain> v1 = IMP_RefConstructExpr::(ref_decl(type_lit(ref<IMP_RefConstructExpr,f,f,plain>)), ref_kind_cast(v0, type_lit(cpp_ref)));
		}
	)")
	{
		impl::reference::task_reference tr0;
		RefConstructExpr rce(tr0);
	}

	return 0;
}
