get_filename_component(GMP_LIBRARIES_DIR ${GMP_LIBRARIES} PATH)
get_filename_component(ZLIB_LIBRARIES_DIR ${ZLIB_LIBRARIES} PATH)

file(GLOB_RECURSE allscale_hat_srcs allscale-hat.cabal src/*.hs)

get_filename_component(CABAL_PATH_ENTRY "${CABAL_EXECUTABLE}" DIRECTORY)
set(CABAL_PATH_ENTRY "${CABAL_PATH_ENTRY}:")

get_target_property(INSIEME_HAT_SOURCE_DIR insieme_hat SOURCE_DIR)
get_target_property(INSIEME_HAT_BINARY_DIR insieme_hat BINARY_DIR)

if(DEFINED CABAL_ROOT)
  set(CABAL_PATH_ENTRY "${CABAL_ROOT}/bin:")
else()
  set(CABAL_PATH_ENTRY "${THIRD_PARTY_DIR}/cabal/bin/:")
endif()

set(haskell_env
  ${CMAKE_COMMAND} -E env
  "PATH=${CABAL_PATH_ENTRY}${THIRD_PARTY_DIR}/ghc/bin/:$ENV{PATH}"
  "LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
  "LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
  "HOME=${INSIEME_HAT_BINARY_DIR}/cabal-home" # reusing insieme-hat cabal-homedir
)

set(cabal ${haskell_env} ${CABAL_EXECUTABLE})
set(runhaskell ${haskell_env} runhaskell)
set(cabal_file ${CMAKE_CURRENT_SOURCE_DIR}/allscale-hat.cabal)

add_custom_target(allscale_hat DEPENDS liballscale-hat.so)

add_custom_command(
	OUTPUT liballscale-hat.so

	COMMAND ${cabal} new-build -j --builddir=${INSIEME_HAT_BINARY_DIR}/dist-newstyle --project-file=${INSIEME_HAT_BINARY_DIR}/cabal.project -v1 --enable-tests flib:allscale-hat

	# copy library to binary dir
	COMMAND ${runhaskell} "${INSIEME_HAT_SOURCE_DIR}/scripts/copyLib.hs" ${cabal_file} "%comp:allscale-hat" ${INSIEME_HAT_BINARY_DIR} ${CMAKE_CURRENT_BINARY_DIR}

	DEPENDS insieme_hat ${INSIEME_HAT_BINARY_DIR}/libinsieme-hat.so

	WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}

	COMMENT "Building AllScale HAT"
)

# cleanup
set_directory_properties(
	PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES
	"${CMAKE_CURRENT_BINARY_DIR}/liballscale-hat.so;${INSIEME_HAT_BINARY_DIR}/dist-newstyle"
)

# force cabal-home cleanup
add_custom_target(clean_allscale_hat
	COMMAND ${CMAKE_COMMAND} -E remove "${CMAKE_CURRENT_BINARY_DIR}/liballscale-hat.so"
	COMMAND ${CMAKE_COMMAND} -E remove_directory "${INSIEME_HAT_BINARY_DIR}/dist-newstyle/"
	COMMAND ${CMAKE_COMMAND} -E remove_directory "${INSIEME_HAT_BINARY_DIR}/cabal-home/.cabal"
)
add_dependencies(clean_allscale_hat clean_insieme_hat)

if(BUILD_DOCS)
	add_custom_command(
		TARGET allscale_hat
		POST_BUILD
		COMMAND ${haskell_env} "${INSIEME_HAT_SOURCE_DIR}/scripts/hat-haddock" ${INSIEME_HAT_BINARY_DIR} "%comp:allscale-hat"
		DEPENDS "${INSIEME_HAT_SOURCE_DIR}/scripts/copyLib.hs" ${allscale_hat_srcs} insieme_hat
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
endif()

set(allscale_hat ${CMAKE_CURRENT_BINARY_DIR}/liballscale-hat.so PARENT_SCOPE)
