program test_w3fb11
  implicit none

  real :: xi, xj, alat1, elon1, dx, elonv, alatan, alat, elon
  integer :: num_fails
  real, parameter :: tol = 1e-4

  num_fails = 0

  ! Test 1: basic test, Northern hemisphere, lower left point
  alat1 = 20.0
  elon1 = 260.0
  dx = 10000.0
  elonv = 260.0
  alatan = 30.0
  alat = 20.0
  elon = 260.0
  call w3fb11(alat,elon,alat1,elon1,dx,elonv,alatan,xi,xj)
  print *, "Test 1:", xi, xj
  if (abs(xi - 1.0) > tol .or. abs(xj - 1.0) > tol) then
     print *, "FAILED Test 1"
     num_fails = num_fails + 1
  end if

  ! Test 2: Southern hemisphere, ALATAN < 0
  alatan = -30.0
  alat1 = -20.0
  alat = -20.0
  elon = 260.0
  call w3fb11(alat,elon,alat1,elon1,dx,elonv,alatan,xi,xj)
  print *, "Test 2:", xi, xj
  if (abs(xi - 1.0) > tol .or. abs(xj - 1.0) > tol) then
     print *, "FAILED Test 2"
     num_fails = num_fails + 1
  end if

  ! Test 3: ELON1 - ELONV > 180 and ELON - ELONV > 180
  alatan = 30.0
  alat1 = 20.0
  elon1 = 350.0
  elonv = 100.0
  alat = 20.0
  elon = 350.0
  call w3fb11(alat,elon,alat1,elon1,dx,elonv,alatan,xi,xj)
  print *, "Test 3:", xi, xj
  if (abs(xi - 1.0) > tol .or. abs(xj - 1.0) > tol) then
     print *, "FAILED Test 3"
     num_fails = num_fails + 1
  end if

  ! Test 4: ELON1 - ELONV < -180 and ELON - ELONV < -180
  elon1 = 10.0
  elonv = 200.0
  alat = 20.0
  elon = 10.0
  call w3fb11(alat,elon,alat1,elon1,dx,elonv,alatan,xi,xj)
  print *, "Test 4:", xi, xj
  if (abs(xi - 1.0) > tol .or. abs(xj - 1.0) > tol) then
     print *, "FAILED Test 4"
     num_fails = num_fails + 1
  end if

  ! Test 5: Verify behavior when XI < 1.0 and XJ < 1.0
  alat1 = 20.0
  elon1 = 260.0
  dx = 10000.0
  elonv = 260.0
  alatan = 30.0
  alat = 10.0
  elon = 250.0
  call w3fb11(alat,elon,alat1,elon1,dx,elonv,alatan,xi,xj)
  print *, "Test 5:", xi, xj
  if (abs(xi - (-115.948425)) > tol .or. abs(xj - (-110.019043)) > tol) then
     print *, "FAILED Test 5"
     num_fails = num_fails + 1
  end if

  if (num_fails > 0) then
     print *, "Some tests failed!"
     stop 1
  else
     print *, "All tests passed."
  end if

end program test_w3fb11
