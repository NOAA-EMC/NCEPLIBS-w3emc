cmake_minimum_required(VERSION 3.16)
project(w3emc VERSION 2.2.0)
enable_language (Fortran)

if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are: PRODUCTION Debug Release."
    FORCE)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  message("setting intel true")
  set(IntelComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU*" OR CMAKE_CXX_COMPILER_ID MATCHES "Clang*")
  set(GNUComp true )
elseif(CMAKE_CXX_COMPILER_ID MATCHES "pgc*")
  set(PGIComp true )
endif()

STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RelWithDebInfo" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "RELEASE" BUILD_RELEASE)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "PRODUCTION" BUILD_PRODUCTION)
STRING(COMPARE EQUAL ${CMAKE_BUILD_TYPE} "DEBUG" BUILD_DEBUG)

if( (BUILD_RELEASE) OR (BUILD_PRODUCTION) )
  if(IntelComp)
    set(shared_fortran_flags "-O2" "-g" "-traceback" "-fixed")
    set(fortran_d_flags "-r8")
    set(fortran_8_flags "-r8" "-i8")
    set(fortran_4_flags)
  elseif(GNUComp)
    set(shared_fortran_flags "-fno-range-check" "-O2" "-g" "-fbacktrace"
      "-ffixed-form" "-ggdb" "-funroll-loops")
    set(fortran_d_flags "-fdefault-real-8")
    set(fortran_8_flags "-fdefault-integer-8" "-fdefault-real-8")
    set(CMAKE_Fortran_4_FLAGS)
  else()
    message("unknown compiler!")
    exit()
  endif()
endif()


if(NOT TARGET nemsio)
  find_package(nemsio REQUIRED CONFIG)
endif()

if(NOT TARGET sigio)
  find_package(sigio REQUIRED CONFIG)
endif()

file(GLOB fortran_src ${CMAKE_CURRENT_SOURCE_DIR}/src/*.f90 ${CMAKE_CURRENT_SOURCE_DIR}/src/*.F)

set(kinds "4" "8" "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(versioned_lib_name ${PROJECT_NAME}_v${PROJECT_VERSION}_${kind})
  add_library(${lib_name} STATIC ${fortran_src})
  
  set_target_properties(${lib_name} PROPERTIES OUTPUT_NAME "${versioned_lib_name}")

  target_compile_options(${lib_name} PRIVATE "${shared_fortran_flags};${fortran_${kind}_flags}")
  
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")
  set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY "${module_dir}")

  target_include_directories(${lib_name} PUBLIC
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include_${kind}>)

  target_link_libraries(${lib_name} nemsio sigio)

  install(TARGETS ${lib_name}
    EXPORT ${PROJECT_NAME}-config
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib) 
  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

install(EXPORT ${PROJECT_NAME}-config DESTINATION ${CMAKE_INSTALL_PREFIX})



