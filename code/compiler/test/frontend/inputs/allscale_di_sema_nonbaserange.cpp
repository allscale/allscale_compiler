
#include "intercepted/data_item_sema.h"

int main() {

	#pragma test expect_ir(R"(
		def IMP_NonBaseRangeDI :: const function IMP__operator_subscript_ = (v1 : ref<int<4>,f,f,plain>) -> ref<real<8>,t,f,cpp_ref> {
			return data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), ref_kind_cast(lit("IMP_NonBaseRange::ctor" : IMP_NonBaseRange::(int<4>, int<4>))(ref_temp(type_lit(IMP_NonBaseRange)), *v1, *v1+1), type_lit(cpp_ref)), type_lit(ref<real<8>,t,f,cpp_ref>));
		};
		{
			var ref<IMP_NonBaseRangeDI,t,f,plain> v0 = lit("IMP_NonBaseRangeDI::ctor" : IMP_NonBaseRangeDI::())(ref_cast(ref_decl(type_lit(ref<IMP_NonBaseRangeDI,t,f,plain>)), type_lit(f), type_lit(f), type_lit(plain)));
			instantiate_member(IMP_NonBaseRangeDI::IMP__operator_subscript_, lit("PARSER_UNRESOLVED_IMP_NonBaseRangeDI::IMP__operator_subscript_" : const IMP_NonBaseRangeDI::(int<4>) -> ref<real<8>,t,f,cpp_ref>))(v0, 0) ;
		}
	)")
	{
		const NonBaseRangeDI di;
		di[0];
	}

	#pragma test expect_ir(R"(
		def IMP_NonBaseRangeDI :: function IMP__operator_subscript_ = (v1 : ref<int<4>,f,f,plain>) -> ref<real<8>,f,f,cpp_ref> {
			return data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), ref_kind_cast(lit("IMP_NonBaseRange::ctor" : IMP_NonBaseRange::(int<4>, int<4>))(ref_temp(type_lit(IMP_NonBaseRange)), *v1-1, *v1), type_lit(cpp_ref)), type_lit(ref<real<8>,f,f,cpp_ref>));
		};
		{
			var ref<IMP_NonBaseRangeDI,f,f,plain> v0 = lit("IMP_NonBaseRangeDI::ctor" : IMP_NonBaseRangeDI::())(ref_decl(type_lit(ref<IMP_NonBaseRangeDI,f,f,plain>)));
			ref_kind_cast(instantiate_member(IMP_NonBaseRangeDI::IMP__operator_subscript_, lit("PARSER_UNRESOLVED_IMP_NonBaseRangeDI::IMP__operator_subscript_" : IMP_NonBaseRangeDI::(int<4>) -> ref<real<8>,f,f,cpp_ref>))(v0, 0), type_lit(plain))  = num_cast(0, type_lit(real<8>));
		}
	)")
	{
		NonBaseRangeDI di;
		di[0] = 0;
	}

	#pragma test expect_ir(R"(
		{
			var ref<IMP_NonBaseRangeDI,f,f,plain> v0 = lit("IMP_NonBaseRangeDI::ctor" : IMP_NonBaseRangeDI::())(ref_decl(type_lit(ref<IMP_NonBaseRangeDI,f,f,plain>)));
			lit("IMP_NonBaseRangeDI::IMP_someOtherMethod" : const IMP_NonBaseRangeDI::() -> unit)(v0);
		}
	)")
	{
		NonBaseRangeDI di;
		di.someOtherMethod();
	}

}
