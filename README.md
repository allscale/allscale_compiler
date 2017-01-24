# AllScale Compiler

The AllScale compiler is a source-to-source compiler for C++ applications that
use the AllScale API.

## Getting the Sources

Change to a working directory on your system and clone the repository
recursively using

    $ git clone --recursive https://github.com/allscale/allscale_compiler

### Structure

The AllScale compiler depends on three external projects, all of which are included as *git submodules*:

 - **API**: The AllScale API project
 - **Runtime**: The AllScale runtime project, which in turn depends on HPX.
 - **Insieme**: The Insieme compiler project, which is the technical foundation for the AllScale compiler.

## Building the Compiler

### Dependencies

AllScale is a project which depends heavily on modern C++ features, and thus requires a compiler implementing C++14.

The dependencies for Insieme and installation instructions for them are detailed [in the README.md file for Insieme](https://github.com/insieme/insieme#dependencies).

The prerequisites for the HPX runtime system are listed [here](https://github.com/STEllAR-GROUP/hpx#build-instructions).

The AllScale API submodule and the Allscale compiler itself do not require additional dependencies beyond those necessitated by Insieme and the HPX Runtime system.


### Building the Project

Once the prerequisites are fulfilled, you can build the entire project by calling

    $ mkdir allscale_compiler_build
    $ cd allscale_compiler_build
    $ cmake -DCMAKE_BUILD_TYPE=Debug /path/to/sources
    $ make -j 8

Note that you should use an out-of-source build for this project. Adjust the source path, build type and degree of build parallelism (-j) as required.

This will build the AllScale compiler as well as all the required submodules.

## Running Unit and Integration Tests

The command

    $ make test ARGS=-j8

will run all **unit tests** for the AllScale compiler as well as its direct dependencies.

To run the set of AllScale-specific **integration tests**, use

    $ code/allscale_integration_tests
