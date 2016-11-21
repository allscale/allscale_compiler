

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({ var ref<treeture<int<4>,f>,f,f,plain> v0 = treeture_done(1); })")
	{
		auto a = done(1);
	}

//	#pragma test expect_ir(R"({ var ref<int<4>> i = 42; })")
//	{
//		auto fib = prec(fun(
//				[](int x)->bool { return x < 2; },
//				[](int x)->int { return x; },
//				[](int x, const auto& f) {
//					return done(1);
//				}
//			)
//		);
//
//		fib(10).get();
//	}

	return 0;
}
