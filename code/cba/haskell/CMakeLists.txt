get_filename_component(GMP_LIBRARIES_DIR ${GMP_LIBRARIES} PATH)
get_filename_component(ZLIB_LIBRARIES_DIR ${ZLIB_LIBRARIES} PATH)

file(GLOB_RECURSE allscale_hat_srcs allscale-hat.cabal src/*.hs)
file(GLOB_RECURSE insieme_hat_srcs
	${PROJECT_SOURCE_DIR}/../insieme/code/analysis/src/cba/haskell/insieme-hat/insieme-hat.cabal
	${PROJECT_SOURCE_DIR}/../insieme/code/analysis/src/cba/haskell/insieme-hat/src/*.hs
)

set(stack_data ${STACK_ROOT}/stack-root_$ENV{USER})

# calling stack
set(stack
	${CMAKE_COMMAND} -E env
	"STACK_ROOT=${stack_data}"
	"LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
	"LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
	${STACK_ROOT}/bin/stack --ghc-variant standard
)

add_custom_command(
	OUTPUT libHSallscale-hat.so

	# create STACK_ROOT folder to prevent hideous error
	COMMAND ${CMAKE_COMMAND} -E make_directory ${stack_data}

	# install adequate ghc / cabal lib
	COMMAND ${stack} setup

	# binarys like alex and c2hs have to be installed manually, do not
	# merge these lines
	COMMAND ${stack} build alex
	COMMAND ${stack} build c2hs

	# install dependencies and build package
	COMMAND ${stack} build

	# copy library (package key in name) to binary dir
	COMMAND ${stack} runhaskell utils/copyLib.hs ${CMAKE_CURRENT_BINARY_DIR}

	# AllScale-HAT depends on Insieme-HAT. Stating this target dependency here
	# prevents both toolkits from being built simultaneously which can
	# cause issues with stack. Furthermore, this invokation of stack also
	# depends on the sources of Insieme-HAT as each HAT has its own
	# .stack-work directory and Insieme-HAT is located in both of them.
	#
	# It is currently not possible to merge these two folders, as one is
	# not located in a subdirectory of another. See
	# <https://github.com/commercialhaskell/stack/issues/1731>
	DEPENDS ${allscale_hat_srcs} ${insieme_hat_srcs} insieme_hat

	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMENT "Building AllScale HAT"
)

add_custom_target(allscale_hat DEPENDS libHSallscale-hat.so)

# runtime library exports
set(LIBHSRTS_DIR ${stack_data}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(LIBHSRTS ${stack_data}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts/libHSrts-ghc7.10.3.so PARENT_SCOPE)

set(allscale_hat ${CMAKE_CURRENT_BINARY_DIR}/libHSallscale-hat.so PARENT_SCOPE)
