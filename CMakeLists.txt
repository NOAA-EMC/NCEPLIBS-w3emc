# This is the main CMake file for NCEPLIBS-w3emc.
#
# Kyle Gerheiser
cmake_minimum_required(VERSION 3.15)

# Get the version from the VERSION file.
file(STRINGS "VERSION" pVersion)

project(
  w3emc
  VERSION ${pVersion}
  LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(GNUInstallDirs)

if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# Find required packages.
find_package(nemsio CONFIG REQUIRED)
find_package(sigio CONFIG REQUIRED)

find_package(NetCDF REQUIRED Fortran)

# Compile the source code in the src directory.
add_subdirectory(src)

# Add tests.
enable_testing()
add_subdirectory(tests)

