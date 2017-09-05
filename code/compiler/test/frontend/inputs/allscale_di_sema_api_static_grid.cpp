
#include "allscale/api/user/operator/pfor.h"
#include "allscale/api/user/data/static_grid.h"

int main() {
	int magic;

	#pragma test expect_ir(R"(
		def IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10> :: const function IMP__operator_subscript_ = (v1 : ref<IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<int<8>,f,f,qualified>,1>,t,f,cpp_ref>) -> ref<real<8>,t,f,cpp_ref> {
			return data_item_element_access(
				ref_kind_cast(this, type_lit(cpp_ref)),
				ref_kind_cast(instantiate(lit("target_type" : IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion<1>::(ref<IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<int<8>,f,f,qualified>,1>,t,f,cpp_ref>)), lit("IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion::ctor" : IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion<'T_0_0>::(ref<IMP_allscale_colon__colon_utils_colon__colon_Vector<int<8>,uint<8>>,t,f,cpp_ref>)))(ref_temp(type_lit(IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_GridRegion<1>)), v1), type_lit(cpp_ref)),
				type_lit(ref<real<8>,f,f,cpp_ref>));
		};
		{
			var ref<IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>,t,f,plain> v0 = instantiate(lit("target_type" : IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>::()), lit("IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid::ctor" : IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,'V_T_0_1...>::()))(ref_cast(ref_decl(type_lit(ref<IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>,t,f,plain>)), type_lit(f), type_lit(f), type_lit(plain)));
			instantiate(lambda_name IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<real<8>,f,f,qualified>,10>::IMP__operator_subscript_, lit("PARSER_UNRESOLVED_IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid::IMP__operator_subscript_" : const IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,'V_T_0_1...>::(ref<IMP_allscale_colon__colon_utils_colon__colon_Vector<int<8>,(type<enum_def<IMP___anon_tagtype__slash_home_slash_petert_slash_allscale_slash_git_slash_allscale_minus_compiler_slash_api_slash_code_slash_api_slash_include_slash_allscale_slash_api_slash_user_slash_data_slash_static_grid_dot_h_213_3,int<4>,enum_entry<IMP_allscale_colon__colon_api_colon__colon_user_colon__colon_data_colon__colon_StaticGrid_colon__colon__colon__colon_dimensions,0>>>, int<4>)>,t,f,cpp_ref>) -> ref<'T_0_0,t,f,cpp_ref>))(v0, ref_kind_cast(instantiate(lit("target_type" : IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<int<8>,f,f,qualified>,1>::(ref<int<8>,t,f,cpp_ref>)), lit("IMP_allscale_colon__colon_utils_colon__colon_Vector::ctor" : IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>,'T_0_1>::(ref<'T_0_0,t,f,cpp_ref>)))(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector<ref<int<8>,f,f,qualified>,1>)), num_cast(0, type_lit(int<8>))), type_lit(cpp_ref))) materialize ;
		}
	)")
	{
		const allscale::api::user::data::StaticGrid<double, 10> g;
		g[0];
	}

}
