! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fc05 subroutine.

program test_w3fc05
  implicit none
  real :: u, v, dir, spd
  integer :: errors
  real, parameter :: epsilon = 0.1

  errors = 0
  print *, "Testing w3fc05..."

  ! Test case 1: u=0, v=0 -> dir=0, spd=0
  u = 0.0
  v = 0.0
  call w3fc05(u, v, dir, spd)
  if (abs(dir - 0.0) > epsilon .or. abs(spd - 0.0) > epsilon) then
     print *, "Test 1 failed. dir=", dir, " spd=", spd
     errors = errors + 1
  end if

  ! Test case 2: u=0, v=-1 -> dir=360, spd=1
  u = 0.0
  v = -1.0
  call w3fc05(u, v, dir, spd)
  if (abs(dir - 360.0) > epsilon .or. abs(spd - 1.0) > epsilon) then
     print *, "Test 2 failed. dir=", dir, " spd=", spd
     errors = errors + 1
  end if

  ! Test case 3: u=1, v=0 -> dir=270, spd=1
  u = 1.0
  v = 0.0
  call w3fc05(u, v, dir, spd)
  if (abs(dir - 270.0) > epsilon .or. abs(spd - 1.0) > epsilon) then
     print *, "Test 3 failed. dir=", dir, " spd=", spd
     errors = errors + 1
  end if

  ! Test case 4: u=0, v=1 -> dir=180, spd=1
  u = 0.0
  v = 1.0
  call w3fc05(u, v, dir, spd)
  if (abs(dir - 180.0) > epsilon .or. abs(spd - 1.0) > epsilon) then
     print *, "Test 4 failed. dir=", dir, " spd=", spd
     errors = errors + 1
  end if

  ! Test case 5: u=-1, v=0 -> dir=90, spd=1
  u = -1.0
  v = 0.0
  call w3fc05(u, v, dir, spd)
  if (abs(dir - 90.0) > epsilon .or. abs(spd - 1.0) > epsilon) then
     print *, "Test 5 failed. dir=", dir, " spd=", spd
     errors = errors + 1
  end if

  if (errors > 0) then
     print *, "TEST FAILED. Total errors:", errors
     stop 1
  else
     print *, "SUCCESS"
  end if

end program test_w3fc05
