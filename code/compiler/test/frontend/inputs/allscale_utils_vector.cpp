

#include "allscale/utils/vector.h"

template<typename T>
using Vector3 = allscale::utils::Vector<T, 3>;

int main() {
	; // this is required because of the clang compound source location bug

	#pragma test expect_ir(R"(
		{
			var ref<array<array<array<IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<real<8>,f,f,qualified>,3>,2u>,2u>,2u>,f,f,plain> v0 = <ref<array<array<array<IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<real<8>,f,f,qualified>,3>,2u>,2u>,2u>,f,f,plain>>(ref_decl(type_lit(ref<array<array<array<IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<real<8>,f,f,qualified>,3>,2u>,2u>,2u>,f,f,plain>))) {};
		}
	)")
	{
		Vector3<double> Es[2][2][2];
	}

	return 0;
}
