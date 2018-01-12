

#include "allscale/api/core/prec.h"

using namespace allscale::api::core;

void funAcceptingAllScaleTypeReference(const detail::completed_task<void>& t) {}

int main() {
	; // this is required because of the clang compound source location bug

	// here we are testing whether the frontend correctly adds the required casts and ref_temp_init calls for implicit materializations of AllScale types

	#pragma test expect_ir(R"(
		def IMP_funAcceptingAllScaleTypeReference = function (v0 : ref<treeture<unit,f>,t,f,cpp_ref>) -> unit { };
		{
			IMP_funAcceptingAllScaleTypeReference(
					ref_cast(
							ref_temp_init(treeture_done(unit)),
							type_lit(t),
							type_lit(f),
							type_lit(cpp_ref)
					)
			);
		}
	)")
	{
		funAcceptingAllScaleTypeReference(done());
	}

	return 0;
}
