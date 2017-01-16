include(${CMAKE_SOURCE_DIR}/insieme/code/environment_common.cmake)
include(${CMAKE_SOURCE_DIR}/insieme/code/environment.cmake)

# required for llvm
add_definitions(-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS)

msvc_file_completion(allscale_frontend_test_inputs test/frontend/inputs c cpp h)

# ================================================== AllScale Library ==================================================

file(GLOB_RECURSE allscale_srcs src/*.cpp include/*.h)
insieme_glob_headers(allscale__incs include)

add_library(allscale_compiler ${LINKING_TYPE} ${allscale_srcs} ${allscale_incs})
target_include_directories(allscale_compiler PUBLIC include)
target_include_directories(allscale_compiler PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/../ )

target_link_libraries(allscale_compiler insieme_common)
target_link_libraries(allscale_compiler insieme_utils)
target_link_libraries(allscale_compiler insieme_core)
target_link_libraries(allscale_compiler insieme_annotations)
target_link_libraries(allscale_compiler insieme_frontend)
target_link_libraries(allscale_compiler insieme_backend)
target_link_libraries(allscale_compiler insieme_analysis)
target_link_libraries(allscale_compiler insieme_transform)
target_link_libraries(allscale_compiler insieme_driver)

# ================================================== AllScale Driver ===================================================

file(GLOB_RECURSE allscale_executables src/*.cxx)
foreach(exec_file ${allscale_executables})
	get_filename_component(exec_name ${exec_file} NAME_WE)
	add_executable(${exec_name} ${exec_file})
	target_link_libraries(${exec_name} allscale_compiler)
	
	add_dependencies(${exec_name} allscale_runtime)
endforeach(exec_file)

# ================================================== AllScale Tests ====================================================

set(ut_prefix ut_allscale)
file(GLOB_RECURSE allscale_tests test/*.cc)
foreach(case_file ${allscale_tests})
	get_filename_component(case_name ${case_file} NAME_WE)
	set(case_name ${ut_prefix}${case_subdir}_${case_name})
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} allscale_compiler)

	if(${case_name} MATCHES ".*code_snippets.*" OR ${case_name} MATCHES ".*fe_conversion.*")
		add_unit_test(${case_name} ${ut_prefix} RUN_PARALLEL)
	else()
		add_unit_test(${case_name} ${ut_prefix})
	endif()
endforeach(case_file)


# ============================================ AllScale Integration Tests ==============================================

set(CONFIG_INTEGRATION_TESTS TRUE)

# configure the integration tests
if(CONFIG_INTEGRATION_TESTS)
	# create global integration test configuration file

	#FIXME: cpp/boost integration tests rely on boost to be set?
	insieme_find_package(NAME Boost)

	configure_file(allscale_integration_test_config.in ${CMAKE_CURRENT_BINARY_DIR}/../allscale_integration_test_config @ONLY)
endif()


# ================================================== AllScale Misc =====================================================


# Export the build directory
add_definitions("-DALLSCALE_BUILD_ROOT=\"${CMAKE_CURRENT_BINARY_DIR}/..\"")
