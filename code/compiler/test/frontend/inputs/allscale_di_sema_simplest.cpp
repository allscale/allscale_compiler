
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

	{
		SimplestDI di;
		di[0] = 0;
	}

	{
		SimplestDI di;
		di.someOtherMethod();
	}

}
