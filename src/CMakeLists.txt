# This is the CMake build file for the src directory of the
# NCEPLIBS-w3emc project.
#
# Kyle Gerheiser, Ed Hartnett

# This is the Fortran source code.
set(fortran_src getgbens.f isrchne.f iw3mat.f
mersenne_twister.f mova2i.f orders.f putgbens.f w3ai40.f w3ai41.f
w3ctzdat.f w3fa01.f w3fa03.f w3fa03v.f w3fa04.f w3fa06.f w3fa09.f
w3fa11.f w3fa12.f w3fa13.f w3fb00.f w3fb01.f w3fb02.f w3fb03.f
w3fb04.f w3fb05.f w3fb06.f w3fb07.f w3fb08.f w3fb09.f w3fb10.f
w3fb11.f w3fb12.f w3fc02.f w3fc05.f w3fc06.f w3fc07.f w3fc08.f
w3fi02.f w3fi03.f w3fi18.f w3fi19.f w3fi20.f w3fi32.f w3fi47.f
w3fi48.f w3fi61.f w3fi65.f w3fi66.f w3fi67.f w3fi70.f
w3fi78.f w3fi85.f w3fm07.f w3fm08.f w3fp04.f w3fp05.f w3fp06.f
w3fp10.f w3ft00.f w3ft01.f w3ft02.f w3ft03.f w3ft05.f
w3ft05v.f w3ft06.f w3ft06v.f w3ft07.f w3ft08.f w3ft09.f w3ft10.f
w3ft11.f w3ft12.f w3ft16.f w3ft17.f w3ft201.f w3ft202.f w3ft203.f
w3ft204.f w3ft205.f w3ft206.f w3ft207.f w3ft208.f w3ft209.f w3ft210.f
w3ft211.f w3ft212.f w3ft213.f w3ft214.f w3ft21.f w3ft26.f w3ft33.f
w3ft38.f w3ft39.f w3ft40.f w3ft41.f w3ft43v.f w3log.f 
xdopen.f aea.f errexit.f errmsg.f fparsei.f fparser.f gbytec.f gbyte.f
gbytesc.f gbytes.f getbit.f getgb1.f getgb1re.f getgb1r.f getgb1s.f
getgbe.f getgbeh.f getgbem.f getgbemh.f getgbemn.f getgbemp.f
getgbep.f getgbex.f getgbexm.f getgb.f getgbh.f getgbm.f getgbmh.f
getgbmp.f getgbp.f getgi.f getgir.f gtbits.f idsdef.f instrument.f
iw3jdn.f iw3pds.f ixgb.f lengds.f makwmo.f mkfldsep.f
pdsens.f pdseup.f putgbe.f putgben.f putgbex.f putgb.f putgbn.f
q9ie32.f r63w72.f sbytec.f sbyte.f sbytesc.f sbytes.f skgb.f w3ai00.f
w3ai01.f w3ai08.f w3ai15.f w3ai18.f w3ai19.f w3ai24.f w3ai38.f
w3ai39.f w3aq15.f w3difdat.f w3doxdat.f w3fi01.f w3fi04.f
w3fi58.f w3fi59.f w3fi62.f w3fi63.f w3fi64.f w3fi68.f w3fi69.f
w3fi71.f w3fi72.f w3fi73.f w3fi74.f w3fi75.f w3fi76.f w3fi82.f
w3fi83.f w3fi88.f w3fi92.f w3fp11.f w3fp12.f w3fp13.f w3fs13.f
w3fs15.f w3fs21.f w3fs26.f w3ft32.f w3kind.f w3locdat.f w3movdat.f
w3nogds.f w3pradat.f w3reddat.f w3trnarg.f w3utcdat.f
w3valdat.f w3ymdh4.f xmovex.f xstore.f w3tagb.f makgds.f90)

# These functions call NCEPLIBS-bufr.
if(BUILD_WITH_BUFR)
  set(fortran_src ${fortran_src} w3miscan.f iw3unp29.f w3unpk77.f)
endif()

# These functions call external, mysterious dependencies.
if(BUILD_WITH_EXTRA_DEPS)
  set(fortran_src ${fortran_src} args_mod.f w3as00.f w3fi52.f w3fq07.f)
endif()

# This is the C source code.
set(c_src summary.c)
add_library(c_obj_lib OBJECT ${c_src})
if(BUILD_SHARED_LIBS)
  set_property(TARGET c_obj_lib PROPERTY POSITION_INDEPENDENT_CODE 1)
endif()

# We buld a _4 and a _d by default.
foreach(kind ${kinds})
  set(lib_name ${PROJECT_NAME}_${kind})
  set(module_dir "${CMAKE_CURRENT_BINARY_DIR}/include_${kind}")

  # Create library. It will be STATIC by default, but SHARED if option
  # BUILD_SHARED_LIBS is on.
  add_library(${lib_name} ${fortran_src} $<TARGET_OBJECTS:c_obj_lib>)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  # For shared, set SOVERSION; increment it if API changes (other than
  # adding new functions).
  if(BUILD_SHARED_LIBS)
    set_target_properties(${lib_name} PROPERTIES SOVERSION 0)
  endif()

  # Set the flags for _4 or _d.
  set_target_properties(${lib_name} PROPERTIES COMPILE_FLAGS
    "${fortran_${kind}_flags}")
  set_target_properties(${lib_name} PROPERTIES Fortran_MODULE_DIRECTORY
    ${module_dir})

  target_include_directories(${lib_name} INTERFACE
    $<BUILD_INTERFACE:${module_dir}>
    $<INSTALL_INTERFACE:include_${kind}>)
  list(APPEND LIB_TARGETS ${lib_name})

  # Link with bacio.
  target_link_libraries(${lib_name} PUBLIC bacio::${bacio_name})

  # Optionally link with bufr.
  if(BUILD_WITH_BUFR)
    target_link_libraries(${lib_name} PUBLIC bufr::bufr_4)
  endif()

  install(DIRECTORY ${module_dir} DESTINATION ${CMAKE_INSTALL_PREFIX})
endforeach()

# Install the library.
install(
  TARGETS ${LIB_TARGETS}
  EXPORT ${PROJECT_NAME}Exports
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})

### Package config
include(CMakePackageConfigHelpers)
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

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
