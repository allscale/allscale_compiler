get_filename_component(GMP_LIBRARIES_DIR ${GMP_LIBRARIES} PATH)
get_filename_component(ZLIB_LIBRARIES_DIR ${ZLIB_LIBRARIES} PATH)

set(stack_data ${STACK_ROOT}/stack-root_$ENV{USER})

# calling stack
set(stack
	${CMAKE_COMMAND} -E env
	"STACK_ROOT=${stack_data}"
	"LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
	"LD_LIBRARY_PATH=${GMP_LIBRARIES_DIR}:${ZLIB_LIBRARIES_DIR}"
	${STACK_ROOT}/bin/stack --ghc-variant standard
)

add_custom_target(
	allscale_hat

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

	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# allscale_hat depends on insieme_hat. Stating this dependency here also
# prevents both toolkits from being built simultaneously which can cause issues
# with stack.
add_dependencies(allscale_hat analysis_haskell)

# runtime library exports
set(LIBHSRTS_DIR ${stack_data}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts PARENT_SCOPE)
set(LIBHSRTS ${stack_data}/programs/x86_64-linux/ghc-7.10.3/lib/ghc-7.10.3/rts/libHSrts-ghc7.10.3.so PARENT_SCOPE)

set(allscale_hat ${CMAKE_CURRENT_BINARY_DIR}/libHSallscale-hat.so PARENT_SCOPE)
