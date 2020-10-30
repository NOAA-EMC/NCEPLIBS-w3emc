# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett

add_executable (tst_w3emc tst_w3emc.F)
add_test(tst_w3emc tst_w3emc)
target_link_libraries (tst_w3emc w3emc_d)

