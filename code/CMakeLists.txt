cmake_minimum_required(VERSION 3.5)
project(allscale_compiler LANGUAGES C CXX)

# -- Module Path
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/../cmake ${PROJECT_SOURCE_DIR}/../insieme/cmake ${CMAKE_MODULE_PATH})

# -- Prefix Path
set(THIRD_PARTY_DIR ${CMAKE_BINARY_DIR}/third_party CACHE STRING "Third Party Library Directory")
if(NOT EXISTS ${THIRD_PARTY_DIR} AND NOT MSVC)
	message(FATAL_ERROR "`third_party` directory does not exist, did you run `scripts/dependencies/third_party_linker` ?")
endif()
file(GLOB prefix_paths ${THIRD_PARTY_DIR}/*)
list(APPEND CMAKE_PREFIX_PATH ${prefix_paths})

# -- Project Settings
include(build_settings)
include(coverage)
include(doxygen)

# -- Dependency Settings
include(dependencies/pthread)
include(dependencies/googletest)
include(dependencies/boost)
include(dependencies/hwloc)
include(dependencies/valgrind)
include(dependencies/gmp)
include(dependencies/zlib)
include(dependencies/cabal)
include(dependencies/ghc)

# -- CMake Modules
include(add_module)
include(file_globs)
include(msvc_source_group)
include(nproc)

# -- Insieme
set(ANALYSIS_HASKELL ON CACHE BOOL "Haskell-based Analysis Toolkit Switch" FORCE)
set(BUILD_RUNTIME OFF CACHE BOOL "Insieme Runtime Switch" FORCE)
list(APPEND HAT_PACKAGE_PATHS_LIST "${CMAKE_CURRENT_SOURCE_DIR}/compiler/src/analysis/allscale-hat/")
add_subdirectory(../insieme/code ${PROJECT_BINARY_DIR}/insieme)

# -- AllScale Runtime
include(allscale_runtime)

# -- AllScale API
include(allscale_api)

# -- AllScale Compiler
add_subdirectory(compiler)

# -- Integration Test Files
add_subdirectory(../test ${PROJECT_BINARY_DIR}/test)
