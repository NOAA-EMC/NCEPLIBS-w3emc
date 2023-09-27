# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett

# This function builds, links, and runs a test program.
function(create_test name kind)
  add_executable(${name}_${kind} ${name}.F90)
  target_link_libraries(${name}_${kind} PRIVATE w3emc_${kind})
  set_target_properties(${name}_${kind} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${name}_${kind} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  add_test(NAME ${name}_${kind} COMMAND ${name}_${kind})
  target_compile_definitions(${name}_${kind} PUBLIC -DKIND_${kind})  
endfunction()

# These are the tests.
foreach(kind ${kinds})
  create_test(test_summary ${kind})
  create_test(test_gbytec ${kind})
  create_test(test_w3tagb ${kind})
  create_test(test_w3fi71 ${kind})
#  create_test(test_w3fi72 ${kind})
  create_test(test_w3fi73 ${kind})
  create_test(test_w3fi74 ${kind})
endforeach()  
