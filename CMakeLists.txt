cmake_minimum_required(VERSION 3.15)

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

find_package(nemsio CONFIG REQUIRED)
find_package(sigio CONFIG REQUIRED)

find_package(NetCDF REQUIRED Fortran)

add_subdirectory(src)

if (ENABLE_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()

