include(ExternalProject)

# locate depending libraries
#find_package(Boost)
if(NOT MSVC)
	find_package(Hwloc)
endif()

# add the external HPX project
ExternalProject_Add(
	hpx
	SOURCE_DIR ${PROJECT_SOURCE_DIR}/../runtime/hpx
	CMAKE_ARGS
		${CMAKE_EXTERNALPROJECT_FORWARDS}
		-DHPX_WITH_THREAD_IDLE_RATES=On
		-DBOOST_ROOT=${Boost_INCLUDE_DIRS}/..
		-DHWLOC_ROOT=${Hwloc_INCLUDE_DIRS}/..
		-DHPX_WITH_NETWORKING=${HPX_WITH_NETWORKING}
		-DHPX_WITH_MALLOC=system
		INSTALL_COMMAND ""
		EXCLUDE_FROM_ALL 1
		BUILD_ALWAYS 1
)

ExternalProject_Get_Property(hpx source_dir binary_dir)
set(hpx_source_dir ${source_dir})
set(hpx_binary_dir ${binary_dir})

# add the external AllScale Runtime project
ExternalProject_Add(
	allscale_runtime
	DEPENDS hpx
	SOURCE_DIR ${PROJECT_SOURCE_DIR}/../runtime/allscale-runtime
	CMAKE_ARGS
		${CMAKE_EXTERNALPROJECT_FORWARDS}
		-DHPX_DIR=${hpx_binary_dir}/lib/cmake/HPX
		-DHPX_WITH_MALLOC=system
		-DHPX_WITH_NETWORKING=${HPX_WITH_NETWORKING}
		-DALLSCALE_WITH_TESTS=off
		INSTALL_COMMAND ""
		EXCLUDE_FROM_ALL 1
		BUILD_ALWAYS 1
)

ExternalProject_Get_Property(allscale_runtime source_dir binary_dir)
set(allscale_runtime_source_dir ${source_dir})
set(allscale_runtime_binary_dir ${binary_dir})

# export configuration
configure_file(${PROJECT_SOURCE_DIR}/../runtime/config.inc.in ${PROJECT_BINARY_DIR}/runtime/config.inc)
