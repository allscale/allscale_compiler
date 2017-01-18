
#include "allscale/api/core/prec.h"
#include "allscale/api/core/treeture.h"

using namespace allscale::api::core;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"({
		var ref<completed_task<int<4>>,f,f,plain> a = task_done(1);
	})")
	{
		auto a = done(1);
	}

	// methods on treetures

	#pragma test expect_ir(R"({
		var ref<treeture<int<4>,t>,f,f,plain> a =  task_to_treeture(task_done(1));
		treeture_wait(*a);
		treeture_get(*a);
		treeture_left(*a);
		treeture_right(*a);
	})")
	{
		impl::reference::treeture<int> a = done(1);
		a.wait();
		a.get();
		a.getLeft();
		a.getRight();
	}

	// methods on unreleased treetures

	#pragma test expect_ir(R"({
		var ref<treeture<real<4>,f>,f,f,plain> a =  task_to_unreleased_treeture(task_done(1.0E+0f));
		treeture_wait(*a);
		treeture_left(*a);
		treeture_right(*a);
	})")
	{
		impl::reference::unreleased_treeture<float> a = done(1.0f);
		a.wait();
		a.getLeft();
		a.getRight();
	}

	// treeture combination operations

	#pragma test expect_ir(R"(
		decl struct __any_string__combine;
		decl IMP__conversion_operator_int_space__lparen__star__rparen__lparen_int_comma__space_int_rparen_:const __any_string__combine::() -> ptr<(int<4>, int<4>) -> int<4>,t,f>;
		def struct __any_string__combine {
			const function IMP__operator_call_ = (v673 : ref<int<4>,f,f,plain>, v674 : ref<int<4>,f,f,plain>) -> int<4> {
				return *v673+*v674;
			}
		};
		{
			var ref<completed_task<int<4>>,f,f,plain> a = task_done(1);
			var ref<completed_task<int<4>>,f,f,plain> b = task_done(2);
			treeture_combine(
				task_to_unreleased_treeture(*a),
				task_to_unreleased_treeture(*b),
				cpp_lambda_to_lambda(
					<ref<__any_string__combine,f,f,plain>>(ref_temp(type_lit(__any_string__combine))) {},
					type_lit((int<4>, int<4>) -> int<4>)
				),
				true
			);

		}
	)")
	{
		auto a = done(1);
		auto b = done(2);
		combine(std::move(a), std::move(b), [](int m, int n) { return m + n; });
	}

	return 0;
}
