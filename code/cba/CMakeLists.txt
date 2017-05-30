add_module_library(cba)

add_subdirectory(haskell)
link_directories(${CMAKE_CURRENT_BINARY_DIR}/haskell)
link_directories(${LIBHSRTS_DIR})
link_directories(${GMP_ROOT}/lib)

add_dependencies(cba allscale_hat)

# Gold linker does not automatically resolve libHSrts depdency from analysis
# lib, need to specify them explicitly.
target_link_libraries(cba ${LIBHSRTS})

target_link_libraries(cba ${allscale_hat})

glob_executables(cba_exes src)
foreach(exe ${cba_exes})
	add_module_executable(cba ${exe})
endforeach(exe)

glob_tests(cba_tests test)
foreach(test ${cba_tests})
	add_module_unittest(cba ${test})
endforeach(test)
