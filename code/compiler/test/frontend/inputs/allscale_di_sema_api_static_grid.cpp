
#include "allscale/api/user/algorithm/pfor.h"
#include "allscale/api/user/data/static_grid.h"

int main() {
	int magic;

	// using __any_type__ and __any_expr__ in the IR comparison here to reduce the compared IR to the minimum for what we actually want to test here
	#pragma test expect_ir(R"(
		def IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10> :: const function IMP__operator_subscript_ = (v1 : ref<__any_type__,t,f,cpp_ref>) -> ref<real<8>,t,f,cpp_ref> {
			return data_item_element_access(
				ref_kind_cast(this, type_lit(cpp_ref)),
				ref_kind_cast(instantiate(lit("target_type" : (ref<__any_type__,t,f,cpp_ref>) -> IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion<1>), lit("IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion_colon__colon_single" : (ref<IMP_allscale_colon__colon_utils_colon__colon_Vector<'TX_0,'TX_1>,t,f,cpp_ref>) -> IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion<'T_0_0>))(v1) materialize , type_lit(cpp_ref)),
				type_lit(ref<real<8>,f,f,cpp_ref>));
		};
		{
			var ref<IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>,t,f,plain> v0 = instantiate(lit("target_type" : IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>::()), lit("IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid::ctor" : IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,'V_T_0_1...>::()))(ref_cast(ref_decl(type_lit(ref<IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>,t,f,plain>)), type_lit(f), type_lit(f), type_lit(plain)));
			instantiate(lambda_name IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>::IMP__operator_subscript_, lit("PARSER_UNRESOLVED_IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid::IMP__operator_subscript_" : const IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,'V_T_0_1...>::(ref<IMP_allscale_colon__colon_utils_colon__colon_Vector<'TX_0,'TX_1>,t,f,cpp_ref>) -> ref<'T_0_0,t,f,cpp_ref>))(v0, ref_kind_cast(lit("__any_expr__" : ref<__any_type__,f,f,plain>), type_lit(cpp_ref))) materialize ;
		}
	)")
	{
		const allscale::api::user::data::StaticGrid<double, 10> g;
		g[0];
	}
}
