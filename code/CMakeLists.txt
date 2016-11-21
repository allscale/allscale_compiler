project(allscale CXX)

include(${CMAKE_SOURCE_DIR}/insieme/code/environment_common.cmake)
include(${CMAKE_SOURCE_DIR}/insieme/code/environment.cmake)

# required for llvm
add_definitions(-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS)

# ================================================== AllScale Library ==================================================

file(GLOB_RECURSE allscale_srcs src/*.cpp include/*.h)
insieme_glob_headers(allscale__incs include)

add_library(allscale ${LINKING_TYPE} ${allscale_srcs} ${allscale_incs})
target_include_directories(allscale PUBLIC include)
target_include_directories(allscale PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/../ )

target_link_libraries(allscale insieme_common)
target_link_libraries(allscale insieme_utils)
target_link_libraries(allscale insieme_core)
target_link_libraries(allscale insieme_annotations)
target_link_libraries(allscale insieme_frontend)
target_link_libraries(allscale insieme_backend)
target_link_libraries(allscale insieme_analysis)
target_link_libraries(allscale insieme_transform)
target_link_libraries(allscale insieme_driver)

# ================================================== AllScale Driver ===================================================

file(GLOB_RECURSE allscale_executables src/*.cxx)
foreach(exec_file ${allscale_executables})
	get_filename_component(exec_name ${exec_file} NAME_WE)
	add_executable(${exec_name} ${exec_file})
	target_link_libraries(${exec_name} allscale)
endforeach(exec_file)

# ================================================== AllScale Tests ====================================================

set(ut_prefix ut_allscale)
file(GLOB_RECURSE allscale_tests test/*.cc)
foreach(case_file ${allscale_tests})
	get_filename_component(case_name ${case_file} NAME_WE)
	set(case_name ${ut_prefix}${case_subdir}_${case_name})
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} allscale)

	add_unit_test(${case_name} ${ut_prefix})
endforeach(case_file)
