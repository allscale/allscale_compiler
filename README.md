# AllScale Compiler

The AllScale compiler is a source-to-source compiler for C++ applications that use the AllScale API.

## Quickstart Guide

To build the AllScale compiler, clone this repository and take a peek at `QUICKSTART`.
You can directly execute it, given you are running a recent Ubuntu / Debian.

    $ git clone --recursive https://github.com/allscale/allscale_compiler
    $ cd insieme
    $ ./QUICKSTART    # prompts for sudo

If you are using a different distribution use your package manager to install all dependencies available.
A list of dependencies can be viewed in the [Dependencies](#dependencies) section of this README.

For software not available in your package manager (or the ones that require a specific patch) use the provided dependency installer inside `/insieme/scripts/dependencies`.

## Full Build Instructions

If you are not interested in customizing your build, simply follow the quickstart instructions above.

### Getting the Sources

Change to a working directory on your system and clone the repository *recursively* using

    $ git clone --recursive https://github.com/allscale/allscale_compiler
    $ cd allscale_compiler

### Structure

The AllScale compiler depends on three related projects, all of which are included as *Git submodules*:

 - **API**: The AllScale API project
 - **Runtime**: The AllScale runtime project, which in turn depends on HPX.
 - **Insieme**: The Insieme compiler project, which is the technical foundation for the AllScale compiler.

### Dependencies

AllScale is a project which depends heavily on modern C++ features, and thus requires a compiler implementing C++14.

All dependencies of the Insieme compiler are required as this project builds on top of it.
The list of Insieme's dependencies and instructions on how to use its dependency installer can be found in `/insieme/scripts/dependencies/README.md`.

The prerequisites for the HPX runtime system are listed in `/runtime/hpx/README.rst` under *build-instructions*.

Assuming you are running a recent Ubunut / Debian you can use the provided environment setup script to install these dependencies.

    $ scripts/environment/setup

Otherwise consult the dependency installer's README (`/insieme/scripts/dependencies/README.md`).

### Building the Project

Once the prerequisites are fulfilled, you can build the entire project by calling

    $ mkdir build
    $ cd build
    $ ../insieme/scripts/dependencies/third_party_linker
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

    $ cmake -G "Visual Studio 14 Win64" -DBUILD_SHARED_LIBS=OFF Z:\path\to\project\code

Add path for third-party libraries when needed.

### Eclipse Project

    $ cmake -G "Eclipse CDT4 - Unix Makefiles" -DCMAKE_ECLIPSE_VERSION=4.4 /path/to/project/code
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
