program test_w3fb09
  implicit none

  real :: xi, xj, alat1, alon1, alatin, dx, alat, alon
  integer :: num_fails
  real, parameter :: tol = 1e-4

  num_fails = 0

  ! Test 1: ALAT1 != 0, positive
  alat1 = 20.0
  alon1 = 260.0
  alatin = 30.0
  dx = 10000.0
  xi = 10.0
  xj = 10.0
  call w3fb09(xi, xj, alat1, alon1, alatin, dx, alat, alon)
  print *, "Test 1:", alat, alon
  if (abs(alat - 20.8757) > tol .or. abs(alon - 260.9346) > tol) then
     print *, "FAILED Test 1"
     num_fails = num_fails + 1
  end if

  ! Test 2: ALAT1 == 0
  alat1 = 0.0
  alon1 = 0.0
  alatin = 0.0
  dx = 10000.0
  xi = -10.0
  xj = -10.0
  call w3fb09(xi, xj, alat1, alon1, alatin, dx, alat, alon)
  print *, "Test 2:", alat, alon
  if (abs(alat - (-0.9894)) > tol .or. abs(alon - (-0.9892)) > tol) then
     print *, "FAILED Test 2"
     num_fails = num_fails + 1
  end if

  ! Test 3: ALAT1 != 0, negative
  alat1 = -20.0
  alon1 = 260.0
  alatin = -30.0
  dx = 10000.0
  xi = 10.0
  xj = 10.0
  call w3fb09(xi, xj, alat1, alon1, alatin, dx, alat, alon)
  print *, "Test 3:", alat, alon
  if (abs(alat - (-19.1194)) > tol .or. abs(alon - 260.9346) > tol) then
     print *, "FAILED Test 3"
     num_fails = num_fails + 1
  end if

  if (num_fails > 0) then
     print *, "Some tests failed!"
     stop 1
  else
     print *, "All tests passed."
  end if

end program test_w3fb09
