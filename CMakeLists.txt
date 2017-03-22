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
)

add_subdirectory(code)
add_subdirectory(test)
