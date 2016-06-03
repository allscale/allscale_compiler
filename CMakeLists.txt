project(allscale-compiler NONE)

# ensure proper cmake version
cmake_minimum_required(VERSION 3.2)

# create a ctest configuration file for unit tests
enable_testing()

add_subdirectory(code)
add_subdirectory(test)
