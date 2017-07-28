
#include "intercepted/data_item_sema.h"

int main() {

	#pragma test expect_ir(R"(
		def IMP_SimplestDI :: const function IMP_get = () -> real<8> {
			return *data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), 0, type_lit(ref<real<8>,t,f,cpp_ref>));
		};
		{
			var ref<IMP_SimplestDI,f,f,plain> v0 = lit("IMP_SimplestDI::ctor" : IMP_SimplestDI::())(ref_decl(type_lit(ref<IMP_SimplestDI,f,f,plain>)));
			instantiate(IMP_SimplestDI::IMP_get, lit("PARSER_UNRESOLVED_IMP_SimplestDI::IMP_get" : const IMP_SimplestDI::() -> real<8>))(v0);
		}
	)")
	{
		SimplestDI di;
		di.get();
	}

	#pragma test expect_ir(R"(
		def IMP_SimplestDI :: function IMP__operator_subscript_ = (v1 : ref<int<4>,f,f,plain>) -> ref<real<8>,f,f,cpp_ref> {
			return data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), ref_kind_cast(v1, type_lit(cpp_ref)), type_lit(ref<real<8>,f,f,cpp_ref>));
		};
		{
			var ref<IMP_SimplestDI,f,f,plain> v0 = lit("IMP_SimplestDI::ctor" : IMP_SimplestDI::())(ref_decl(type_lit(ref<IMP_SimplestDI,f,f,plain>)));
			instantiate(IMP_SimplestDI::IMP__operator_subscript_, lit("PARSER_UNRESOLVED_IMP_SimplestDI::IMP__operator_subscript_" : IMP_SimplestDI::(int<4>) -> ref<real<8>,f,f,cpp_ref>))(v0, 0) materialize  = num_cast(0, type_lit(real<8>));
		}
	)")
	{
		SimplestDI di;
		di[0] = 0;
	}

	#pragma test expect_ir(R"(
		{
			var ref<IMP_SimplestDI,f,f,plain> v0 = lit("IMP_SimplestDI::ctor" : IMP_SimplestDI::())(ref_decl(type_lit(ref<IMP_SimplestDI,f,f,plain>)));
			lit("IMP_SimplestDI::IMP_someOtherMethod" : const IMP_SimplestDI::() -> unit)(v0);
		}
	)")
	{
		SimplestDI di;
		di.someOtherMethod();
	}

}
