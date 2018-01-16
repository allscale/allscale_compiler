
#include "intercepted/data_item_sema.h"

int main() {
	int magic;

	#pragma test expect_ir(R"(
		def IMP_SimplestDI :: const function IMP_getInvolvingCleanups = () -> real<8> {
			return *data_item_element_access(ref_kind_cast(this, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(0), type_lit(cpp_ref)), type_lit(ref<real<8>,f,f,cpp_ref>));
		};
		{
			var ref<IMP_SimplestDI,f,f,plain> v0 = lit("IMP_SimplestDI::ctor" : IMP_SimplestDI::())(ref_decl(type_lit(ref<IMP_SimplestDI,f,f,plain>)));
			instantiate_member(IMP_SimplestDI::IMP_getInvolvingCleanups, lit("PARSER_UNRESOLVED_IMP_SimplestDI::IMP_getInvolvingCleanups" : const IMP_SimplestDI::() -> real<8>))(v0);
		}
	)")
	{
		SimplestDI di;
		di.getInvolvingCleanups();
	}

}
