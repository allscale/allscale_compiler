project(allscale NONE)
cmake_minimum_required(VERSION 3.2)

enable_testing()

add_subdirectory(insieme)
add_subdirectory(code)
add_subdirectory(test)
