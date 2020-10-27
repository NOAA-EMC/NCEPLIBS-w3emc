# This is the CMake build file for the test directory of NCEPLIBS-w3.

# Kyle Gerheiser, Ed Hartnett

add_executable (tst_w3 tst_w3.F)
add_test(tst_w3 tst_w3)
target_link_libraries (tst_w3 w3_d)

