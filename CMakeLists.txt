project(allscale_compiler CXX)
cmake_minimum_required(VERSION 3.2)

# For IDE project generation
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

enable_testing()

add_subdirectory(insieme)
add_subdirectory(runtime)

include(ExternalProject)
ExternalProject_Add(
	api
	SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/api
	BUILD_IN_SOURCE 0
	INSTALL_COMMAND ""
	CMAKE_ARGS
		-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
		-DCMAKE_C_COMPILER_ARG1=${CMAKE_C_COMPILER_ARG1}
		-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
		-DCMAKE_CXX_COMPILER_ARG1=${CMAKE_CXX_COMPILER_ARG1}
)

add_subdirectory(code)
add_subdirectory(test)
