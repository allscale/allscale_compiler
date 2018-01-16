
#include "intercepted/data_item_sema.h"

int main() {

	#pragma test expect_ir(R"(
		def IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>> :: const function IMP__operator_subscript_ = (v1 : ref<int<4>,f,f,plain>) -> ref<real<8>,t,f,cpp_ref> {
			return data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), ref_kind_cast(instantiate_ctor(lit("target_type" : IMP_TemplatedRange<ref<int<4>,f,f,qualified>>::(int<4>, int<4>, int<4>)), lit("IMP_TemplatedRange::ctor" : IMP_TemplatedRange<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::('T_0_0, 'T_0_0, 'T_0_0)))(ref_temp(type_lit(IMP_TemplatedRange<ref<int<4>,f,f,qualified>>)), *v1, *v1*2, *v1+3), type_lit(cpp_ref)), type_lit(ref<real<8>,t,f,cpp_ref>));
		};
		{
			var ref<IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>,t,f,plain> v0 = instantiate_ctor(lit("target_type" : IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>::()), lit("IMP_TemplatedDI::ctor" : IMP_TemplatedDI<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::()))(ref_cast(ref_decl(type_lit(ref<IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>,t,f,plain>)), type_lit(f), type_lit(f), type_lit(plain)));
			instantiate_member(lambda_name IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>::IMP__operator_subscript_, lit("IMP_TemplatedDI::IMP__operator_subscript_" : const IMP_TemplatedDI<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::('T_0_1) -> ref<'T_0_0,t,f,cpp_ref>))(v0, 0) materialize ;
		}
	)")
	{
		const TemplatedDI<double, int> di;
		di[0];
	}

	#pragma test expect_ir(R"(
		def IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>> :: function IMP__operator_subscript_ = (v1 : ref<int<4>,f,f,plain>) -> ref<real<8>,f,f,cpp_ref> {
			return data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), ref_kind_cast(instantiate_ctor(lit("target_type" : IMP_TemplatedRange<ref<int<4>,f,f,qualified>>::(int<4>, int<4>, int<4>)), lit("IMP_TemplatedRange::ctor" : IMP_TemplatedRange<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::('T_0_0, 'T_0_0, 'T_0_0)))(ref_temp(type_lit(IMP_TemplatedRange<ref<int<4>,f,f,qualified>>)), *v1, *v1*2, *v1+3), type_lit(cpp_ref)), type_lit(ref<real<8>,f,f,cpp_ref>));
		};
		{
			var ref<IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,plain> v0 = instantiate_ctor(lit("target_type" : IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>::()), lit("IMP_TemplatedDI::ctor" : IMP_TemplatedDI<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::()))(ref_decl(type_lit(ref<IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,plain>)));
			instantiate_member(lambda_name IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>::IMP__operator_subscript_, lit("IMP_TemplatedDI::IMP__operator_subscript_" : IMP_TemplatedDI<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::('T_0_1) -> ref<'T_0_0,f,f,cpp_ref>))(v0, 0) materialize  = 0.0E+0;
		}
	)")
	{
		TemplatedDI<double, int> di;
		di[0] = 0.0;
	}

	#pragma test expect_ir(R"(
		{
			var ref<IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,plain> v0 = instantiate_ctor(lit("target_type" : IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>::()), lit("IMP_TemplatedDI::ctor" : IMP_TemplatedDI<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::()))(ref_decl(type_lit(ref<IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>,f,f,plain>)));
			instantiate_member(lit("target_type" : const IMP_TemplatedDI<ref<real<8>,f,f,qualified>,ref<int<4>,f,f,qualified>>::() -> unit), lit("IMP_TemplatedDI::IMP_someOtherMethod" : const IMP_TemplatedDI<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,ref<'T_0_1,'T_0_1_a,'T_0_1_b,'T_0_1_c>>::() -> unit))(v0);
		}
	)")
	{
		TemplatedDI<double, int> di;
		di.someOtherMethod();
	}

}
