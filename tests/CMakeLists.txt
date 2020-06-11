set(test_srcs test_w3emc.pf)

add_pfunit_ctest (w3emc_test
  TEST_SOURCES test_w3emc_mod.pf
  LINK_LIBRARIES w3emc::w3emc_d NetCDF::NetCDF_Fortran sigio::sigio nemsio::nemsio
)
