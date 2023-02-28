# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett

# We only have tests for the _d version of the library.
if(BUILD_D)

  # This function builds and runs a test.
  function(w3emc_test name)
    add_executable(${name} ${name}.F90)
    target_link_libraries(${name} PRIVATE w3emc_d)
    add_test(NAME ${name} COMMAND ${name})
  endfunction()

  # These are the tests.
  w3emc_test(test_summary)
  w3emc_test(test_w3tagb)
  w3emc_test(test_w3fi71)
endif()
