include(ExternalProject)

find_package( Boost )
find_package( Hwloc )

# add the external HPX project
ExternalProject_Add(
	hpx
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/hpx
	CMAKE_ARGS
		-DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
		-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
		-DCMAKE_C_COMPILER_ARG1=${CMAKE_C_COMPILER_ARG1}
		-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
		-DCMAKE_CXX_COMPILER_ARG1=${CMAKE_CXX_COMPILER_ARG1}
        -DBOOST_ROOT=${Boost_INCLUDE_DIRS}/..
        -DHWLOC_ROOT=${Hwloc_INCLUDE_DIRS}/.. 
        -DHPX_WITH_MALLOC=system
    INSTALL_COMMAND ""
)

ExternalProject_Get_Property(hpx source_dir binary_dir)
set( hpx_source_dir ${source_dir})
set( hpx_binary_dir ${binary_dir})


# add the external AllScale Runtime project
ExternalProject_Add(
	allscale_runtime
    DEPENDS hpx
    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/allscale-runtime
	CMAKE_ARGS
		-DCMAKE_MAKE_PROGRAM=${CMAKE_MAKE_PROGRAM}
		-DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
		-DCMAKE_C_COMPILER_ARG1=${CMAKE_C_COMPILER_ARG1}
		-DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
		-DCMAKE_CXX_COMPILER_ARG1=${CMAKE_CXX_COMPILER_ARG1}
        -DHPX_DIR=${hpx_binary_dir}/lib/cmake/HPX 
        -DALLSCALE_WITH_TESTS=off
    INSTALL_COMMAND ""
)

ExternalProject_Get_Property(allscale_runtime source_dir binary_dir)
set( allscale_runtime_source_dir ${source_dir})
set( allscale_runtime_binary_dir ${binary_dir})


