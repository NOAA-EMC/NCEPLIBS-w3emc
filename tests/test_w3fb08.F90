program test_w3fb08
  implicit none
  real :: alat, alon, alat1, alon1, alatin, dx, xi, xj
  integer :: num_fails
  real, parameter :: tol = 1e-4

  num_fails = 0

  ! Test 1: alat = alat1, alon = alon1
  alat1 = 20.0
  alon1 = 260.0
  alatin = 0.0
  dx = 10000.0
  alat = 20.0
  alon = 260.0
  call w3fb08(alat,alon,alat1,alon1,alatin,dx,xi,xj)
  print *, "Test 1:", xi, xj
  if (abs(xi - 1.0) > tol .or. abs(xj - 1.0) > tol) then
     print *, "FAILED Test 1"
     num_fails = num_fails + 1
  end if

  ! Test 2: alat1 = 0.0
  alat1 = 0.0
  alon1 = 260.0
  alatin = 0.0
  dx = 10000.0
  alat = 0.0
  alon = 260.0
  call w3fb08(alat,alon,alat1,alon1,alatin,dx,xi,xj)
  print *, "Test 2:", xi, xj
  if (abs(xi - 1.0) > tol .or. abs(xj - 1.0) > tol) then
     print *, "FAILED Test 2"
     num_fails = num_fails + 1
  end if

  ! Test 3: arbitrary values
  alat1 = -20.0
  alon1 = -260.0
  alatin = 0.0
  dx = 10000.0
  alat = 10.0
  alon = -250.0
  call w3fb08(alat,alon,alat1,alon1,alatin,dx,xi,xj)
  print *, "Test 3:", xi, xj

end program test_w3fb08
