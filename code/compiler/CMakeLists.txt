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
	add_dependencies(compiler allscale_hat)
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

# Generate HTML report template
set(report_template ${PROJECT_BINARY_DIR}/report.out.html)
file(GLOB_RECURSE report_template_srcs LIST_DIRECTORIES false resources/report/*)
add_custom_command(
	OUTPUT ${report_template}
	COMMAND ./generate > ${report_template}
	DEPENDS ${report_template_srcs}
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/resources/report
	COMMENT "Generate HTML report template"
)
add_custom_target(html_report_template DEPENDS ${report_template})
add_dependencies(compiler_allscalecc html_report_template)

# add test input sources
if(MSVC)
	glob_headers(allscale_frontend_test_input_incs test/frontend/inputs)
	glob_sources(allscale_frontend_test_input_srcs test/frontend/inputs)
	set(allscale_frontend_test_input_srcs ${allscale_frontend_test_input_srcs} ${allscale_frontend_test_input_incs})
	add_custom_target(allscale_frontend_test_input SOURCES ${allscale_frontend_test_input_srcs})
	msvc_source_group("Input Files" "${allscale_frontend_test_input_srcs}" STRIP test/frontend/inputs)
	set_target_properties(allscale_frontend_test_input PROPERTIES FOLDER "compiler/Tests")
endif()
