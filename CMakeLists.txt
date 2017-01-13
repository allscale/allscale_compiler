project(allscale_compiler CXX)
cmake_minimum_required(VERSION 3.2)

# For IDE project generation
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

enable_testing()

add_subdirectory(insieme)
add_subdirectory(runtime)

add_subdirectory(api)
msvc_file_completion(allscale_api_tests api/code/test inl)

add_subdirectory(code)
add_subdirectory(test)
