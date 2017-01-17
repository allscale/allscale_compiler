
#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({
		var ref<completed_task<int<4>>,f,f,plain> a = task_done(1);
	})")
	{
		auto a = done(1);
	}

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>,t>,f,f,plain> a =  task_to_treeture(task_done(1));
	})")
	{
		impl::reference::treeture<int> a = done(1);
		a.wait();
		a.get();
	}

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>,f>,f,f,plain> a =  task_to_unreleased_treeture(task_done(1));
	})")
	{
		impl::reference::unreleased_treeture<int> a = done(1);
		a.wait();
		a.get();
	}

	return 0;
}
