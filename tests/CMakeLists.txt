set(test_srcs test_w3.pf)

# link to bacio and w3nco as dependencies of nemsio manually because they can't be transitive
add_pfunit_ctest (w3_test
  TEST_SOURCES test_w3_mod.pf
  LINK_LIBRARIES w3::w3_d bacio::bacio_4 w3nco::w3nco_d
)
