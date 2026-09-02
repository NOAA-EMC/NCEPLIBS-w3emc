program test_w3fb07
  implicit none

  real :: alat_in, alon_in
  real :: xi, xj, alat1, alon1, dx, alonv
  real :: alat_out, alon_out
  integer :: fail_count = 0
  real :: tol = 1e-3
  real :: diff_lon
  
  real :: PI, RADPD, DXL, REFLON, ALA1, RMLL, ALO1, POLEI, POLEJ, H, RERTH, SS60

  PI = 3.1416
  RADPD = PI / 180.0
  RERTH = 6.3712E+6
  SS60 = 1.86603

  print *, "Starting test_w3fb07..."

  ! Case 1: Northern Hemisphere, YY > 0
  alat1 = 45.0
  alon1 = -100.0
  dx = 381000.0
  alonv = -105.0
  alat_in = 60.0
  alon_in = -90.0

  call w3fb06(alat_in, alon_in, alat1, alon1, dx, alonv, xi, xj)
  call w3fb07(xi, xj, alat1, alon1, dx, alonv, alat_out, alon_out)
  diff_lon = abs(mod(alon_out - alon_in + 3600.0, 360.0))
  if (diff_lon > 180.0) diff_lon = 360.0 - diff_lon
  
  if (abs(alat_out - alat_in) > tol .or. diff_lon > tol) then
    print *, "FAIL Case 1: Expected ALAT, ALON=", alat_in, alon_in, " got ", alat_out, alon_out
    fail_count = fail_count + 1
  end if

  ! Case 2: Southern Hemisphere, YY < 0
  alat1 = -45.0
  alon1 = 100.0
  dx = -381000.0
  alonv = 105.0
  alat_in = -60.0
  alon_in = 110.0

  call w3fb06(alat_in, alon_in, alat1, alon1, dx, alonv, xi, xj)
  call w3fb07(xi, xj, alat1, alon1, dx, alonv, alat_out, alon_out)
  diff_lon = abs(mod(alon_out - alon_in + 3600.0, 360.0))
  if (diff_lon > 180.0) diff_lon = 360.0 - diff_lon
  
  if (abs(alat_out - alat_in) > tol .or. diff_lon > tol) then
    print *, "FAIL Case 2: Expected ALAT, ALON=", alat_in, alon_in, " got ", alat_out, alon_out
    fail_count = fail_count + 1
  end if

  ! Case 3: North Pole (R2 == 0 exactly)
  alat1 = 45.0
  alon1 = -100.0
  dx = 381000.0
  alonv = -105.0
  
  ! Compute exactly POLEI and POLEJ
  H = 1.0
  DXL = dx
  REFLON = alonv - 270.0
  ALA1 = alat1 * RADPD
  RMLL = (RERTH/DXL) * COS(ALA1) * SS60/(1. + H * SIN(ALA1))
  ALO1 = (alon1 - REFLON) * RADPD
  POLEI = 1.0 - RMLL * COS(ALO1)
  POLEJ = 1.0 - H * RMLL * SIN(ALO1)
  
  xi = POLEI
  xj = POLEJ
  alat_in = 90.0
  alon_in = REFLON ! Because the subroutine sets it to REFLON when R2 == 0

  call w3fb07(xi, xj, alat1, alon1, dx, alonv, alat_out, alon_out)
  
  diff_lon = abs(mod(alon_out - alon_in + 3600.0, 360.0))
  if (diff_lon > 180.0) diff_lon = 360.0 - diff_lon
  
  if (abs(alat_out - alat_in) > tol .or. diff_lon > tol) then
    print *, "FAIL Case 3: Expected ALAT, ALON=", alat_in, alon_in, " got ", alat_out, alon_out
    fail_count = fail_count + 1
  end if

  ! Case 4: South Pole (R2 == 0 exactly)
  alat1 = -45.0
  alon1 = 100.0
  dx = -381000.0
  alonv = 105.0
  
  H = -1.0
  DXL = -dx
  REFLON = alonv - 90.0
  ALA1 = alat1 * RADPD
  RMLL = (RERTH/DXL) * COS(ALA1) * SS60/(1. + H * SIN(ALA1))
  ALO1 = (alon1 - REFLON) * RADPD
  POLEI = 1.0 - RMLL * COS(ALO1)
  POLEJ = 1.0 - H * RMLL * SIN(ALO1)

  xi = POLEI
  xj = POLEJ
  alat_in = -90.0
  alon_in = REFLON

  call w3fb07(xi, xj, alat1, alon1, dx, alonv, alat_out, alon_out)
  
  diff_lon = abs(mod(alon_out - alon_in + 3600.0, 360.0))
  if (diff_lon > 180.0) diff_lon = 360.0 - diff_lon
  
  if (abs(alat_out - alat_in) > tol .or. diff_lon > tol) then
    print *, "FAIL Case 4: Expected ALAT, ALON=", alat_in, alon_in, " got ", alat_out, alon_out
    fail_count = fail_count + 1
  end if

  ! Case 5: Force ALON < 0 inside w3fb07 to test the IF (ALON.LT.0) branch
  ! Just run a regular point that produces a very negative ALON before the IF
  alat1 = 45.0
  alon1 = 10.0
  dx = 381000.0
  alonv = 0.0
  alat_in = 60.0
  alon_in = -10.0
  
  call w3fb06(alat_in, alon_in, alat1, alon1, dx, alonv, xi, xj)
  call w3fb07(xi, xj, alat1, alon1, dx, alonv, alat_out, alon_out)
  
  diff_lon = abs(mod(alon_out - alon_in + 3600.0, 360.0))
  if (diff_lon > 180.0) diff_lon = 360.0 - diff_lon
  
  if (abs(alat_out - alat_in) > tol .or. diff_lon > tol) then
    print *, "FAIL Case 5: Expected ALAT, ALON=", alat_in, alon_in, " got ", alat_out, alon_out
    fail_count = fail_count + 1
  end if

  if (fail_count > 0) then
    print *, fail_count, " test(s) failed."
    stop 1
  else
    print *, "All tests passed successfully."
  end if

end program test_w3fb07
