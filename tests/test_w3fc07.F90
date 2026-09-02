! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fc07 subroutine.

program test_w3fc07
  implicit none
  real :: ffid, ffjd, fgu, fgv, fu, fv
  integer :: errors
  real, parameter :: epsilon = 0.0001

  errors = 0
  print *, "Testing w3fc07..."

  ! Test case 1: DFP = 0 (North Pole)
  ffid = 0.0
  ffjd = 0.0
  fgu = 1.0
  fgv = 2.0
  call w3fc07(ffid, ffjd, fgu, fgv, fu, fv)
  ! Expected values computed from source
  ! XFU = -(1.0 * 0.1736482 + 2.0 * 0.9848078) = -2.1432638
  ! FV  = -(2.0 * 0.1736482 - 1.0 * 0.9848078) = 0.6375114
  if (abs(fu - (-2.1432638)) > epsilon .or. abs(fv - 0.6375114) > epsilon) then
     print *, "Test 1 failed. fu=", fu, " fv=", fv
     errors = errors + 1
  end if

  ! Test case 2: DFP > 0 (Normal case)
  ffid = 3.0
  ffjd = 4.0
  fgu = 1.0
  fgv = 2.0
  call w3fc07(ffid, ffjd, fgu, fgv, fu, fv)
  ! Expected values:
  ! XFU = (1.0 * 4.0 - 2.0 * 3.0) / 5.0 = -0.4
  ! FV  = (1.0 * 3.0 + 2.0 * 4.0) / 5.0 = 2.2
  if (abs(fu - (-0.4)) > epsilon .or. abs(fv - 2.2) > epsilon) then
     print *, "Test 2 failed. fu=", fu, " fv=", fv
     errors = errors + 1
  end if

  if (errors > 0) then
     print *, "TEST FAILED. Total errors:", errors
     stop 1
  else
     print *, "SUCCESS"
  end if

end program test_w3fc07
