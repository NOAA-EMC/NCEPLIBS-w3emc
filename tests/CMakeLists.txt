# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett

execute_process(COMMAND cmake -E create_symlink
  "${CMAKE_CURRENT_SOURCE_DIR}/data"
  "${CMAKE_CURRENT_BINARY_DIR}/data" # New name
  )

add_executable(test_w3emc test_w3emc.f)

add_test(test_w3emc test_w3emc "data/gfs.t00z.pgrb2.0p25.f012_grib1" "data/gfs.t00z.pgrb2.0p25.f012_grib1.idx")


target_link_libraries(test_w3emc bacio::bacio_4 w3nco::w3nco_4)
