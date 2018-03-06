#include "allscale_analysis.h"

#include "allscale/api/user/data/grid.h"

#include <vector>

using namespace allscale::api::user;
using namespace allscale::api::user::data;

struct Particle {};

struct Cell {
	std::vector<Particle> particles;
};

template<unsigned D>
using Coordinate = allscale::utils::Vector<int,D>;

void importParticles(Cell& cell, const Coordinate<3>& pos, Grid<std::vector<Particle>,3>& transfers) {

	// import particles sent to this cell
	Coordinate<3> size = transfers.size();
	Coordinate<3> centerIndex = pos * 3 + Coordinate<3>{1,1,1};

	for(int i = 0; i<3; i++) {
		for(int j = 0; j<3; j++) {
			for(int k = 0; k<3; k++) {
				// iterates through all buffers attached to the cell at the given position
				auto cur = centerIndex + Coordinate<3>{i-1,j-1,k-1};
				if (cur[0] < 0 || cur[0] >= size[0]) continue;
				if (cur[1] < 0 || cur[1] >= size[1]) continue;
				if (cur[2] < 0 || cur[2] >= size[2]) continue;

				auto& in = transfers[cur];
				cell.particles.insert(cell.particles.end(), in.begin(), in.end());
				in.clear();
			}
		}
	}

}

int main() {

	// some objects
	Cell cell;
	Coordinate<3> pos;
	Grid<std::vector<Particle>,3>& transfers = *(Grid<std::vector<Particle>,3>*)nullptr;
	Grid<Cell,3>& cells = *(Grid<Cell,3>*)nullptr;

	// start with something simple
	{
		cba_expect_data_requirements("{}");
		Coordinate<3> size;
	}

	// access to the size should be for free
	{
		cba_expect_data_requirements("{}");
		transfers.size();
	}

	// creating an iterator should be for free too
	{
		cba_expect_data_requirements("{}");
		transfers[pos].begin();
		transfers[pos].end();
	}

	// also creating copies of iterators should be for free
	{
		cba_expect_data_requirements("{}");
		auto a = transfers[pos].begin();
		auto b = transfers[pos].end();
	}

	// clearing an element should be a dependency
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		transfers[pos].clear();
	}

//	// reading values should cause a read dependency
	// TODO: known bug, fix this when needed
//	{
//		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
//		std::vector<Particle> copy;
//		auto& in = transfers[pos];
//		copy.insert(copy.end(),in.begin(),in.end());
//
//		cba_debug_requirements();
//	}

	// test a replacement variant for moving particles
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		std::vector<Particle> copy;
		auto& in = transfers[pos];

		// the current replacement for moving particles
		copy.reserve(in.size());
		for(std::size_t i=0; i<in.size(); i++) {
			copy.push_back(in[i]);
		}
	}

	// and a high-performance version
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RO }}");
		std::vector<Particle> copy;
		auto& in = transfers[pos];

		// the current replacement for moving particles
		copy.resize(in.size());
		std::memcpy(&copy[0],&in[0],sizeof(Particle)*in.size());
	}


	// also adding a particle
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		transfers[pos].push_back(Particle());
	}

	// compute a scaled position
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_kind_cast(v1, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		Coordinate<3> center = pos * 3;
		transfers[center].clear();
	}

	// compute a scaled position and offset it
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_kind_cast(v1, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), -1, 0, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		Coordinate<3> center = pos * 3 + Coordinate<3>{-1,0,1};
		transfers[center].clear();
	}

	// compute a scaled position, offset it and used it for an access in a loop
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[span(ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_kind_cast(v1, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 0-1, 0-2, 0-3), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)),ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_kind_cast(v1, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 3-1u-1, 3-1u-2, 3-1u-3), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)))] RW }}");
		Coordinate<3> center = pos * 3 + Coordinate<3>{1,1,1};
		for(int i=0; i<3; i++) {
			auto cur = center + Coordinate<3>{i-1,i-2,i-3};
			transfers[cur].clear();
		}
	}

	// clear a full list of elements
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[span(ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_kind_cast(v1, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 0-1, 0-1, 0-1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)),ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_kind_cast(v1, type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 3-1u-1, 3-1u-1, 3-1u-1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)))] RW }}");

		Coordinate<3> size = transfers.size();
		Coordinate<3> centerIndex = pos * 3 + Coordinate<3>{1,1,1};

		for(int i = 0; i<3; i++) {
			for(int j = 0; j<3; j++) {
				for(int k = 0; k<3; k++) {
					// iterates through all buffers attached to the cell at the given position
					auto cur = centerIndex + Coordinate<3>{i-1,j-1,k-1};
					if (cur[0] < 0 || cur[0] >= size[0]) continue;
					if (cur[1] < 0 || cur[1] >= size[1]) continue;
					if (cur[2] < 0 || cur[2] >= size[2]) continue;

					auto& in = transfers[cur];
					in.clear();
				}
			}
		}
	}

	// the almost full version
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[span(ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_cast(ref_kind_cast(v1, type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 0-1, 0-1, 0-1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)),ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_cast(ref_kind_cast(v1, type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 3-1u-1, 3-1u-1, 3-1u-1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)))] RW }}");
		importParticles(cell,pos,transfers);
	}

	// the full version
	{
		cba_expect_data_requirements("{Requirement { ref_kind_cast(ref_kind_cast(v2, type_lit(plain)), type_lit(cpp_ref))[span(ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_cast(ref_kind_cast(v1, type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 0-1, 0-1, 0-1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)),ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), ref_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_plus_(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon__operator_mult_(ref_cast(ref_kind_cast(v1, type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref)), ref_kind_cast(ref_temp_init(3), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 1, 1, 1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref)), ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_int_3)), 3-1u-1, 3-1u-1, 3-1u-1), type_lit(cpp_ref))) materialize , type_lit(f), type_lit(f), type_lit(cpp_rref))), type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref)))] RW },"
									  "Requirement { ref_kind_cast(ref_kind_cast(v3, type_lit(plain)), type_lit(cpp_ref))[ref_kind_cast(instantiate_fun(target_type, IMP_single)(ref_cast(ref_kind_cast(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3::(ref_temp(type_lit(IMP_allscale_colon__colon_utils_colon__colon_Vector_long_3)), ref_kind_cast(v1, type_lit(cpp_ref))), type_lit(cpp_ref)), type_lit(t), type_lit(f), type_lit(cpp_ref))) materialize , type_lit(cpp_ref))] RW }}");
		importParticles(cells[pos],pos,transfers);
	}

	return 0;
}
