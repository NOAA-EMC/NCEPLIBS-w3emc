! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fc06 subroutine.

program test_w3fc06
  implicit none
  real :: dir, spd, u, v
  real :: exp_u, exp_v
  integer :: errors
  real, parameter :: epsilon = 0.0001
  real, parameter :: pio180 = 0.0174533

  errors = 0
  print *, "Testing w3fc06..."

  ! Test case 1: Wind from North (360 deg)
  dir = 360.0
  spd = 10.0
  call w3fc06(dir, spd, u, v)
  exp_u = -10.0 * sin(360.0 * pio180)
  exp_v = -10.0 * cos(360.0 * pio180)
  if (abs(u - exp_u) > epsilon .or. abs(v - exp_v) > epsilon) then
     print *, "Test 1 failed. u=", u, " v=", v, " exp_u=", exp_u, " exp_v=", exp_v
     errors = errors + 1
  end if

  ! Test case 2: Wind from East (90 deg)
  dir = 90.0
  spd = 15.0
  call w3fc06(dir, spd, u, v)
  exp_u = -15.0 * sin(90.0 * pio180)
  exp_v = -15.0 * cos(90.0 * pio180)
  if (abs(u - exp_u) > epsilon .or. abs(v - exp_v) > epsilon) then
     print *, "Test 2 failed. u=", u, " v=", v, " exp_u=", exp_u, " exp_v=", exp_v
     errors = errors + 1
  end if

  ! Test case 3: Wind from SouthWest (225 deg)
  dir = 225.0
  spd = 20.0
  call w3fc06(dir, spd, u, v)
  exp_u = -20.0 * sin(225.0 * pio180)
  exp_v = -20.0 * cos(225.0 * pio180)
  if (abs(u - exp_u) > epsilon .or. abs(v - exp_v) > epsilon) then
     print *, "Test 3 failed. u=", u, " v=", v, " exp_u=", exp_u, " exp_v=", exp_v
     errors = errors + 1
  end if

  ! Test case 4: Zero speed
  dir = 180.0
  spd = 0.0
  call w3fc06(dir, spd, u, v)
  exp_u = 0.0
  exp_v = 0.0
  if (abs(u - exp_u) > epsilon .or. abs(v - exp_v) > epsilon) then
     print *, "Test 4 failed. u=", u, " v=", v, " exp_u=", exp_u, " exp_v=", exp_v
     errors = errors + 1
  end if

  if (errors > 0) then
     print *, "TEST FAILED. Total errors:", errors
     stop 1
  else
     print *, "SUCCESS"
  end if

end program test_w3fc06
