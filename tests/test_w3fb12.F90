program test_w3fb12
  implicit none

  real :: xi, xj, alat1, elon1, dx, elonv, alatan, alat, elon
  integer :: ierr
  integer :: num_fails
  real, parameter :: tol = 1e-4

  num_fails = 0

  ! Test 1: basic test, Northern hemisphere
  alat1 = 20.0
  elon1 = 260.0
  dx = 10000.0
  elonv = 260.0
  alatan = 30.0
  xi = 1.0
  xj = 1.0
  call w3fb12(xi,xj,alat1,elon1,dx,elonv,alatan,alat,elon,ierr)
  print *, "Test 1:", alat, elon, ierr
  if (abs(alat - 19.9999) > tol .or. abs(elon - 260.0000) > tol .or. ierr /= 0) then
     print *, "FAILED Test 1"
     num_fails = num_fails + 1
  end if

  ! Test 2: Southern hemisphere, ALATAN < 0
  alatan = -30.0
  alat1 = -20.0
  xi = 1.0
  xj = 1.0
  call w3fb12(xi,xj,alat1,elon1,dx,elonv,alatan,alat,elon,ierr)
  print *, "Test 2:", alat, elon, ierr
  if (abs(alat - (-19.9999)) > tol .or. abs(elon - 260.0000) > tol .or. ierr /= 0) then
     print *, "FAILED Test 2"
     num_fails = num_fails + 1
  end if

  ! Test 3: ELON1 - ELONV > 180
  alatan = 30.0
  alat1 = 20.0
  elon1 = 350.0
  elonv = 100.0
  xi = 1.0
  xj = 1.0
  call w3fb12(xi,xj,alat1,elon1,dx,elonv,alatan,alat,elon,ierr)
  print *, "Test 3:", alat, elon, ierr
  if (abs(alat - 19.9999) > tol .or. abs(elon - 100.0000) > tol .or. ierr /= 0) then
     print *, "FAILED Test 3"
     num_fails = num_fails + 1
  end if

  ! Test 4: ELON1 - ELONV < -180
  elon1 = 10.0
  elonv = 200.0
  call w3fb12(xi,xj,alat1,elon1,dx,elonv,alatan,alat,elon,ierr)
  print *, "Test 4:", alat, elon, ierr
  if (abs(alat - 19.9999) > tol .or. abs(elon - 200.0000) > tol .or. ierr /= 0) then
     print *, "FAILED Test 4"
     num_fails = num_fails + 1
  end if

  ! Test 5: Forbidden zone
  xj = 10000.0
  call w3fb12(xi,xj,alat1,elon1,dx,elonv,alatan,alat,elon,ierr)
  print *, "Test 5:", alat, elon, ierr
  if (abs(alat - 999.0) > tol .or. abs(elon - 999.0) > tol .or. ierr /= 1) then
     print *, "FAILED Test 5"
     num_fails = num_fails + 1
  end if

  ! Test 6: Same map, NEWMAP = .FALSE.
  xj = 5.0
  call w3fb12(xi,xj,alat1,elon1,dx,elonv,alatan,alat,elon,ierr)
  print *, "Test 6:", alat, elon, ierr
  if (abs(alat - 20.3545) > tol .or. abs(elon - 200.0000) > tol .or. ierr /= 0) then
     print *, "FAILED Test 6"
     num_fails = num_fails + 1
  end if

  if (num_fails > 0) then
     print *, "Some tests failed!"
     stop 1
  else
     print *, "All tests passed."
  end if

end program test_w3fb12
