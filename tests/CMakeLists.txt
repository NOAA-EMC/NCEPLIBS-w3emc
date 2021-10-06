# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett

function(w3emc_test name)
  add_executable(${name} ${name}.f90)
  target_link_libraries(${name} PRIVATE w3emc_d)
  add_test(NAME ${name} COMMAND ${name})
endfunction()

w3emc_test(test_summary)
w3emc_test(test_w3tagb)
