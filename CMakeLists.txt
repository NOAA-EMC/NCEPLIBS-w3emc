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
option(BUILD_SHARED_LIBS "Build shared libraries" OFF)
option(BUILD_4 "Build the 4-byte real version of the library, libw3emc_4.a" ON)
option(BUILD_D "Build the 8-byte real version of the library, libw3emc_d.a" ON)
option(BUILD_8 "Build the 8-byte integer version of the library, libsp_8.a" OFF)
option(BUILD_TESTING "Build tests of the library" ON)
option(BUILD_WITH_EXTRA_DEPS "Build w3emc with subprograms which call unknown dependencies" ON)
option(BUILD_WITH_BUFR "Build w3emc with subprograms which call NCEPLIBS-bufr" ON)

# If building shared, extra dependencies must be eliminated.
if(BUILD_SHARED_LIBS)
  if(BUILD_WITH_EXTRA_DEPS)
      message(FATAL_ERROR "Shared libraries cannot be built without setting -DBUILD_WITH_EXTRA_DEPS=OFF.")
  endif()
endif()

# Figure whether user wants _4, _d, _8 kinds of library.
if(BUILD_4)
  set(kinds "4")
endif()
if(BUILD_D)
  set(kinds ${kinds} "d")
endif()
if(BUILD_8)
  set(kinds ${kinds} "8")
endif()
if(NOT BUILD_4 AND NOT BUILD_D AND NOT BUILD_8)
  message(FATAL_ERROR "At least one of BUILD_4 or BUILD_D must be turned on")
endif()
message(STATUS "Library kinds that will be build: ${kinds}")

# Find CMake files.
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Set standard GNU install directories.
include(GNUInstallDirs)

# Deal with build type.
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

# NCEPLIBS-bacio is required.
find_package(bacio 2.4.0 REQUIRED)

# NCEPLIBS-bufr may be required.
if(BUILD_WITH_BUFR)
  find_package(bufr REQUIRED)
endif()

# The name of the bacio library changed with NCEPLIBS-bacio-2.5.0.
if(bacio_VERSION GREATER_EQUAL 2.5.0)
  set(bacio_name bacio)
else()
  set(bacio_name bacio_4)
endif()

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|IntelLLVM)$")
  set(CMAKE_Fortran_FLAGS "-g -traceback ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(fortran_d_flags "-r8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS
      "-g -fno-range-check -fbacktrace -funroll-loops ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
  set(fortran_d_flags "-fdefault-real-8")
endif()

# Deal with argument mismatch on GNU compilers version 10 or later.
if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch -fallow-invalid-boz")
endif()

# Compile the source code in the src directory.
add_subdirectory(src)

# Add tests.
if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(tests)
endif()

# Determine whether or not to generate documentation.
if(ENABLE_DOCS)
  find_package(Doxygen REQUIRED)
  add_subdirectory(docs)  
endif()
