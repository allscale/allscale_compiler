# required for llvm
add_definitions(-D__STDC_CONSTANT_MACROS -D__STDC_LIMIT_MACROS)

# required for backend part
add_definitions("-DALLSCALE_BUILD_ROOT=\"${PROJECT_BINARY_DIR}\"")

add_module_library(compiler)

# Insieme Dependencies
target_link_libraries(compiler
	analysis
	annotations
	backend
	common
	core
	driver
	frontend
	transform
	utils
)

# Haskell dependency
if(NOT MSVC)
	add_subdirectory(src/analysis/allscale-hat)
	link_directories(${CMAKE_CURRENT_BINARY_DIR}/src/analysis/allscale-hat)
	link_directories(${LIBHSRTS_DIR})
	link_directories(${GMP_ROOT}/lib)

	add_dependencies(compiler allscale_hat)

	# Gold linker does not automatically resolve libHSrts depdency from analysis
	# lib, need to specify them explicitly.
	target_link_libraries(compiler ${LIBHSRTS})

	target_link_libraries(compiler ${allscale_hat})
endif()

# include generated configs
target_include_directories(compiler PUBLIC ${PROJECT_BINARY_DIR})

glob_executables(compiler_exes src)
foreach(exe ${compiler_exes})
	add_module_executable(compiler ${exe} OUTPUT_TARGET_NAME exe_tgt)
	add_dependencies(${exe_tgt} allscale_runtime)
endforeach(exe)

glob_tests(compiler_tests test)
foreach(test ${compiler_tests})
	if(test MATCHES "code_snippets" OR test MATCHES "fe_conversion")
		add_module_unittest(compiler ${test} PARALLEL)
	else()
		add_module_unittest(compiler ${test})
	endif()
endforeach(test)

# setup integration test config
configure_file(../allscale_integration_test_config.in ${PROJECT_BINARY_DIR}/allscale_integration_test_config @ONLY IMMEDIATE)

# integration test driver requires allscalecc
add_dependencies(compiler_allscale_integration_tests compiler_allscalecc)

# add test input sources
if(MSVC)
	glob_headers(allscale_frontend_test_input_incs test/frontend/inputs)
	glob_sources(allscale_frontend_test_input_srcs test/frontend/inputs)
	set(allscale_frontend_test_input_srcs ${allscale_frontend_test_input_srcs} ${allscale_frontend_test_input_incs})
	add_custom_target(allscale_frontend_test_input SOURCES ${allscale_frontend_test_input_srcs})
	msvc_source_group("Input Files" "${allscale_frontend_test_input_srcs}" STRIP test/frontend/inputs)
	set_target_properties(allscale_frontend_test_input PROPERTIES FOLDER "compiler/Tests")
endif()
