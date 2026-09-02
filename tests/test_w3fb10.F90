program test_w3fb10
  implicit none
  real :: dlat1, dlon1, dlat2, dlon2, beard, gcdkm
  integer :: num_fails
  real, parameter :: tol_ang = 2.0  ! generous for SP/DP differences
  real, parameter :: tol_dist = 5.0

  num_fails = 0

  ! Test 1: General points
  dlat1 = 40.0
  dlon1 = 70.0
  dlat2 = 40.0
  dlon2 = 75.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 1:", beard, gcdkm
  if (abs(beard - 88.37) > tol_ang .or. abs(gcdkm - 425.86) > tol_dist) then
     print *, "FAILED Test 1"
     num_fails = num_fails + 1
  end if

  ! Test 2: North pole to general
  dlat1 = 90.0
  dlon1 = 70.0
  dlat2 = 40.0
  dlon2 = 75.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 2:", beard, gcdkm
  if (abs(beard - (-75.0)) > tol_ang .or. abs(gcdkm - 5559.92) > tol_dist) then
     print *, "FAILED Test 2"
     num_fails = num_fails + 1
  end if

  ! Test 3: South pole to general
  dlat1 = -90.0
  dlon1 = 70.0
  dlat2 = 40.0
  dlon2 = 75.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 3:", beard, gcdkm
  if (abs(beard - 75.0) > tol_ang .or. abs(gcdkm - 14455.79) > tol_dist) then
     print *, "FAILED Test 3"
     num_fails = num_fails + 1
  end if

  ! Test 4: General to North pole
  dlat1 = 40.0
  dlon1 = 70.0
  dlat2 = 90.0
  dlon2 = 75.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 4:", beard, gcdkm
  if (abs(beard - 0.0) > tol_ang .or. abs(gcdkm - 5559.92) > tol_dist) then
     print *, "FAILED Test 4"
     num_fails = num_fails + 1
  end if

  ! Test 5: Exact match
  dlat1 = 40.0
  dlon1 = 70.0
  dlat2 = 40.0
  dlon2 = 70.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 5:", beard, gcdkm
  if (gcdkm > 3.0) then
     print *, "FAILED Test 5"
     num_fails = num_fails + 1
  end if

  ! Test 6: Antipodal points
  ! Bearing is undefined, so just check distance (~ PI * radius)
  dlat1 = 40.0
  dlon1 = 70.0
  dlat2 = -40.0
  dlon2 = -110.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 6:", beard, gcdkm
  if (abs(gcdkm - 20015.68) > 5.0) then
     print *, "FAILED Test 6"
     num_fails = num_fails + 1
  end if

  ! Test 7: Longitude wraparound
  dlat1 = 40.0
  dlon1 = 10.0
  dlat2 = 40.0
  dlon2 = 250.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 7:", beard, gcdkm
  if (abs(beard - (-41.93)) > tol_ang .or. abs(gcdkm - 9242.98) > tol_dist) then
     print *, "FAILED Test 7"
     num_fails = num_fails + 1
  end if

  ! Test 8: NP to NP
  dlat1 = 90.0
  dlon1 = 0.0
  dlat2 = 90.0
  dlon2 = 0.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 8:", beard, gcdkm
  if (abs(beard) > tol_ang .or. gcdkm > 3.0) then
     print *, "FAILED Test 8"
     num_fails = num_fails + 1
  end if

  ! Test 9: NP to SP
  dlat1 = 90.0
  dlon1 = 0.0
  dlat2 = -90.0
  dlon2 = 0.0
  call w3fb10(dlat1, dlon1, dlat2, dlon2, beard, gcdkm)
  print *, "Test 9:", beard, gcdkm
  if (abs(beard) > tol_ang .or. abs(gcdkm - 20015.68) > 5.0) then
     print *, "FAILED Test 9"
     num_fails = num_fails + 1
  end if

  if (num_fails > 0) then
     print *, "Some tests failed!"
     stop 1
  else
     print *, "All tests passed."
  end if

end program test_w3fb10
