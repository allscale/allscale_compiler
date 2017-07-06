include(ExternalProject)

ExternalProject_Add(
	api
	SOURCE_DIR ${PROJECT_SOURCE_DIR}/../api
	BUILD_IN_SOURCE 0
	INSTALL_COMMAND ""
	CMAKE_ARGS
		${CMAKE_EXTERNALPROJECT_FORWARDS}
		-DINVOKED_AS_EXTERNAL_PROJECT=ON
)
