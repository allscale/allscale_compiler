# AllScale Compiler

The AllScale compiler is a source-to-source compiler for C++ applications that use the AllScale API.

## Quickstart Guide

This quickstart guide requires GCC 5 (or 6), m4, CMake, bison, flex, pkg-config and Ruby in addition to a standard Linux setup.
Install them via your distribution's package manager.
For example, on Ubuntu 16.04 LTS:

    $ sudo apt update
    $ sudo apt install git build-essential m4 cmake bison flex pkg-config ruby

Then run the following commands:

    $ git clone --recursive https://github.com/allscale/allscale_compiler
    $ cd allscale_compiler
    $ scripts/dependencies/installer
    $ mkdir build
    $ cd build
    $ ../scripts/dependencies/third_party_linker
    $ cmake ../code
    $ make -j8
    $ ctest -j8

This will retrieve, optionally patch and compile all required dependencies (including llvm and HPX) as well as the project itself, and then run the unit tests for the AllScale compiler and the AllScale API.
On a high-end desktop system, this full process takes about an hour.

## Full Build Instructions

If you are not interested in customizing your build, simply follow the quickstart instructions above.

### Getting the Sources

Change to a working directory on your system and clone the repository *recursively* using

    $ git clone --recursive https://github.com/allscale/allscale_compiler
    $ cd allscale_compiler

### Structure

The AllScale compiler depends on three external projects, all of which are included as *Git submodules*:

 - **API**: The AllScale API project
 - **Runtime**: The AllScale runtime project, which in turn depends on HPX.
 - **Insieme**: The Insieme compiler project, which is the technical foundation for the AllScale compiler.

### Dependencies

AllScale is a project which depends heavily on modern C++ features, and thus requires a compiler implementing C++14.

The dependencies for Insieme and installation instructions for them are detailed [in the README.md file for Insieme](https://github.com/insieme/insieme#dependencies).
This repository comes with a dependency installer (like Insieme) which takes care of installing the required dependencies.
See its [README](scripts/dependencies/README.md) for additional information.

The prerequisites for the HPX runtime system are listed [here](https://github.com/STEllAR-GROUP/hpx#build-instructions).
The AllScale API submodule and the Allscale compiler itself do not require additional dependencies beyond those necessitated by Insieme and the HPX Runtime system.

To setup the dependencies run the following command.

    $ scripts/dependencies/installer

This line will install the dependencies into `$HOME/third_party_libs`.

### Building the Project

Once the prerequisites are fulfilled, you can build the entire project by calling

    $ mkdir build
    $ cd build
    $ ../scripts/dependencies/third_party_linker
    $ cmake ../code
    $ make -j8

Note that you should use an out-of-source build for this project.
Adjust the build type and degree of build parallelism (`-j`) as required.

The `third_party_linker` script will create a folder `third_party` containing symlinks to the software installed by the dependency installer.
This software is considered by CMake.

This will build the AllScale compiler as well as all the required submodules.

### Running Unit and Integration Tests

The command

    $ ctest -j8

will run all **unit tests** for the AllScale compiler as well as its direct dependencies.

To run the set of AllScale-specific **integration tests**, use

    $ compiler/allscale_integration_tests

## Development

### Configuration

Following options can be supplied to CMake

| Option              | Values          |
| ------------------- | --------------- |
| -DCMAKE_BUILD_TYPE  | Release / Debug |
| -DBUILD_SHARED_LIBS | ON / OFF        |
| -DBUILD_TESTS       | ON / OFF        |
| -DBUILD_DOCS        | ON / OFF        |
| -DUSE_ASSERT        | ON / OFF        |
| -DUSE_VALGRIND      | ON / OFF        |
| -DTHIRD_PARTY_DIR   | \<path\>        |

The files `cmake/build_settings.cmake` and `code/CMakeLists.txt` state their default value.

### Executable Bit

When working on Windows via SMB share, consider setting following Git setting.

    $ git config core.filemode false

### Licensor

A script, together with a Git hook, is provided to automatically add a license header to each source file upon commit.
See `scripts/license`.

### Visual Studio Solution

    $ cmake -G "Visual Studio 14 Win64" -DBUILD_SHARED_LIBS=OFF Z:\path\to\project

Add path for third-party libraries when needed.

### Eclipse Project

    $ cmake -G "Eclipse CDT4 - Unix Makefiles" -DCMAKE_ECLIPSE_VERSION=4.4 /path/to/project
    $ sed '/__cplusplus/d' -i .cproject

After this, you can import this project using "Existing Projects into Workspace".
You will have to modify some settings of the project afterwards:
* Modify the source directory location
  * Right click on the *[Source directory]* entry in the *Project Explorer*
  * Go to *Properties*
  * On the *Resource* page:
    * *Edit* the location with the button on the right
    * Remove the trailing "code/" part of the path
    * Apply the changes
* Mark the *[Subprojects]* and *[Targets]* entries as derived:
  * Right click on them in the *Project Explorer*
  * Go to *Properties*
  * On the *Resource* page:
    * Check the "Derived" checkbox
    * Apply the changes
