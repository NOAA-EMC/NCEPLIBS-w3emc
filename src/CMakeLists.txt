include("list_of_files.cmake")

if(APPLE)
  add_compile_definitions(APPLE)
elseif(UNIX)
  add_compile_definitions(LINUX)
endif()

if(CMAKE_C_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_C_FLAGS "-g ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
elseif(CMAKE_C_COMPILER_ID MATCHES "^(GNU|Clang|AppleClang)$")
  set(CMAKE_C_FLAGS "-ggdb ${CMAKE_C_FLAGS}")
  set(CMAKE_C_FLAGS_RELEASE "-O3")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "-g -traceback -fixed ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(fortran_d_flags "-r8")
  set(fortran_8_flags "-i8 -r8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS
      "-g -fno-range-check -fbacktrace -funroll-loops -ffixed-form ${CMAKE_Fortran_FLAGS}")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-ggdb -Wall")
  set(fortran_d_flags "-fdefault-real-8")
  set(fortran_8_flags "-fdefault-integer-8 -fdefault-real-8")
endif()

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "^(GNU)$" AND ${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -w -fallow-argument-mismatch -fallow-invalid-boz")
endif()
set_source_files_properties(${c_src} PROPERTIES COMPILE_OPTIONS "${c_flags}")
 
set(kinds "4" "8" "d")
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")

  add_library(${lib_name} STATIC ${fortran_src})
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  set_target_properties(${lib_name} PROPERTIES COMPILE_FLAGS
    "${fortran_${kind}_flags}")
  set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY
    ${module_dir})
add_library(${lib_name}_c OBJECT ${c_src})

#  add_library(${lib_name} STATIC $<TARGET_OBJECTS:${lib_name}_f>
#    $<TARGET_OBJECTS:${lib_name}_c>)

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)
  target_link_libraries(${lib_name} PRIVATE nemsio::nemsio sigio::sigio
    NetCDF::NetCDF_Fortran)
  list(APPEND LIB_TARGETS ${lib_name})

  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION lib/cmake/${PROJECT_NAME})

export(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake)

configure_package_config_file(
  ${CMAKE_SOURCE_DIR}/cmake/PackageConfig.cmake.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION})
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

write_basic_package_version_file(
  ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})

install(EXPORT ${PROJECT_NAME}Exports
  NAMESPACE ${PROJECT_NAME}::
  FILE ${PROJECT_NAME}-targets.cmake
  DESTINATION ${CONFIG_INSTALL_DESTINATION})
