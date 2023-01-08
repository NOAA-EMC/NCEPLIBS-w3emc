# This is the main CMake file for NCEPLIBS-w3emc.
#
# Kyle Gerheiser, Ed Hartnett
cmake_minimum_required(VERSION 3.15)

# Get the version from the VERSION file.
file(STRINGS "VERSION" pVersion)

# Set up CMake project.
project(w3emc VERSION ${pVersion} LANGUAGES C Fortran)

# Handle user options.
option(ENABLE_DOCS "Enable generation of doxygen-based documentation." OFF)
option(BUILD_4 "Build the 4-byte real version of the library, libw3emc_4.a" ON)
option(BUILD_D "Build the 8-byte real version of the library, libw3emc_d.a" ON)

# Figure whether user wants a _4, a _d, or both libraries.
if(BUILD_4 AND BUILD_D)
  set(kinds "4" "d")
elseif(BUILD_4 AND NOT BUILD_D)
  set(kinds "4")
elseif(BUILD_D AND NOT BUILD_4)
  set(kinds "d")
else()
  message(FATAL_ERROR "At least one of BUILD_4 or BUILD_D must be turned on")
endif()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Set standard GNU install directories.
include(GNUInstallDirs)

# Deal with build type.
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Bacio is required.
find_package(bacio 2.4.0 REQUIRED)

# The name of the bacio library changed with NCEPLIBS-bacio-2.5.0.
if(bacio_VERSION GREATER_EQUAL 2.5.0)
  set(bacio_name bacio)
else()
  set(bacio_name bacio_4)
endif()

# Compile the source code in the src directory.
add_subdirectory(src)

# Add tests.
enable_testing()
add_subdirectory(tests)

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
  add_subdirectory(docs)  
endif()
