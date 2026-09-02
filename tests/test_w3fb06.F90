program test_w3fb06
  implicit none

  real :: alat, alon, alat1, alon1, dx, alonv
  real :: xi, xj
  integer :: fail_count = 0
  real :: tol = 1e-4

  print *, "Starting test_w3fb06..."

  ! Case 1: Northern Hemisphere
  alat = 60.0
  alon = -90.0
  alat1 = 45.0
  alon1 = -100.0
  dx = 381000.0
  alonv = -105.0

  call w3fb06(alat, alon, alat1, alon1, dx, alonv, xi, xj)
  
  if (abs(xi - 2.03745842) > tol .or. abs(xj - 5.79981613) > tol) then
    print *, "FAIL Case 1: Expected xi, xj= 2.03745842, 5.79981613, got ", xi, xj
    fail_count = fail_count + 1
  end if

  ! Case 2: Southern Hemisphere
  alat = -60.0
  alon = 110.0
  alat1 = -45.0
  alon1 = 100.0
  dx = -381000.0
  alonv = 105.0

  call w3fb06(alat, alon, alat1, alon1, dx, alonv, xi, xj)
  
  if (abs(xi - (-0.855215073)) > tol .or. abs(xj - 5.54672909) > tol) then
    print *, "FAIL Case 2: Expected xi, xj= -0.855215073, 5.54672909, got ", xi, xj
    fail_count = fail_count + 1
  end if

  ! Case 3: South Pole
  alat = -90.0
  alon = 0.0
  alat1 = -45.0
  alon1 = 100.0
  dx = -381000.0
  alonv = 105.0

  call w3fb06(alat, alon, alat1, alon1, dx, alonv, xi, xj)
  
  if (abs(xi - (-0.126516774)) > tol .or. abs(xj - 13.8760300) > tol) then
    print *, "FAIL Case 3: Expected xi, xj= -0.126516774, 13.8760300, got ", xi, xj
    fail_count = fail_count + 1
  end if

  ! Case 4: North Pole
  alat = 90.0
  alon = 0.0
  alat1 = 45.0
  alon1 = -100.0
  dx = 381000.0
  alonv = -105.0

  call w3fb06(alat, alon, alat1, alon1, dx, alonv, xi, xj)
  
  if (abs(xi - (-0.126709297)) > tol .or. abs(xj - 13.8760128) > tol) then
    print *, "FAIL Case 4: Expected xi, xj= -0.126709297, 13.8760128, got ", xi, xj
    fail_count = fail_count + 1
  end if

  if (fail_count > 0) then
    print *, fail_count, " test(s) failed."
    stop 1
  else
    print *, "All tests passed successfully."
  end if

end program test_w3fb06
