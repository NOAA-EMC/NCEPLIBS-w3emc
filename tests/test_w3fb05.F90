program test_w3fb05
  implicit none

  real :: xi, xj, xmeshl, orient, alat, along
  integer :: fail_count = 0
  real :: tol = 1e-4

  print *, "Starting test_w3fb05..."

  ! Case 1: R2 = 0, XMESHL > 0 (North Pole)
  xi = 0.0
  xj = 0.0
  xmeshl = 381.0
  orient = 80.0
  call w3fb05(xi, xj, xmeshl, orient, alat, along)
  
  if (abs(alat - 90.0) > tol .or. abs(along - 0.0) > tol) then
    print *, "FAIL Case 1: Expected alat, along= 90.0, 0.0, got ", alat, along
    fail_count = fail_count + 1
  end if

  ! Case 2: R2 = 0, XMESHL < 0 (South Pole)
  xi = 0.0
  xj = 0.0
  xmeshl = -381.0
  orient = 260.0
  call w3fb05(xi, xj, xmeshl, orient, alat, along)
  
  if (abs(alat - (-90.0)) > tol .or. abs(along - 0.0) > tol) then
    print *, "FAIL Case 2: Expected alat, along= -90.0, 0.0, got ", alat, along
    fail_count = fail_count + 1
  end if

  ! Case 3: Northern Hemisphere typical point
  xi = 10.0
  xj = 10.0
  xmeshl = 381.0
  orient = 80.0
  call w3fb05(xi, xj, xmeshl, orient, alat, along)
  
  if (abs(alat - 41.2389221) > tol .or. abs(along - 305.0) > tol) then
    print *, "FAIL Case 3: Expected alat, along= 41.2389221, 305.0, got ", alat, along
    fail_count = fail_count + 1
  end if

  ! Case 4: Southern Hemisphere typical point
  xi = -5.0
  xj = 5.0
  xmeshl = -381.0
  orient = 260.0
  call w3fb05(xi, xj, xmeshl, orient, alat, along)
  
  if (abs(alat - (-64.4642029)) > tol .or. abs(along - 125.0) > tol) then
    print *, "FAIL Case 4: Expected alat, along= -64.4642029, 125.0, got ", alat, along
    fail_count = fail_count + 1
  end if

  ! Case 5: Wrap around longitude (ALONG >= 360)
  ! We want 270.0 + ORIENT - ANGLE >= 360.0
  ! Let's make ANGLE small negative or zero, ORIENT large.
  ! If XJ = -10, XI = 10, ANGLE is -45 degrees (which is converted to 315 by if (ANGLE.lt.0))
  ! Wait, in the code:
  ! ANGLE = DEGPRD * ATAN2(XJ,XI)
  ! IF (ANGLE.LT.0.0) ANGLE = ANGLE + 360.0
  ! So ANGLE is in [0, 360)
  ! For ALONG = 270.0 + ORIENT - ANGLE to be >= 360.0
  ! Let's choose ORIENT = 180.0, ANGLE = 0 (XI=10, XJ=0)
  ! Then ALONG = 270 + 180 - 0 = 450 -> 90.0
  xi = 10.0
  xj = 0.0
  xmeshl = 381.0
  orient = 180.0
  call w3fb05(xi, xj, xmeshl, orient, alat, along)
  
  if (abs(alat - 54.4619827) > tol .or. abs(along - 90.0) > tol) then
    print *, "FAIL Case 5: Expected alat, along= 54.4619827, 90.0, got ", alat, along
    fail_count = fail_count + 1
  end if

  if (fail_count > 0) then
    print *, fail_count, " test(s) failed."
    stop 1
  else
    print *, "All tests passed successfully."
  end if

end program test_w3fb05
