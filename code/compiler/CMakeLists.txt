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

# Allscale CBA Dependency
target_link_libraries(compiler cba)

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
