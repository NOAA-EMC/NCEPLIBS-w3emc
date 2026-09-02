program test_w3fa03
  implicit none
  real :: press, height, temp, theta
  integer :: errors
  real, parameter :: epsilon = 0.1

  errors = 0
  print *, "Testing w3fa03..."

  ! Test case 1: PRESS < 54.7487 (e.g., 10.0)
  press = 10.0
  call w3fa03(press, height, temp, theta)
  if (abs(height - 31054.6) > epsilon .or. abs(temp - 227.7) > epsilon .or. abs(theta - 848.8) > epsilon) then
     print *, "Test 1 failed. height=", height, " temp=", temp, " theta=", theta
     errors = errors + 1
  end if

  ! Test case 2: 54.7487 <= PRESS <= 226.321 (e.g., 100.0)
  press = 100.0
  call w3fa03(press, height, temp, theta)
  if (abs(height - 16179.7) > epsilon .or. abs(temp - 216.6) > epsilon .or. abs(theta - 418.3) > epsilon) then
     print *, "Test 2 failed. height=", height, " temp=", temp, " theta=", theta
     errors = errors + 1
  end if

  ! Test case 3: PRESS > 226.321 (e.g., 500.0)
  press = 500.0
  call w3fa03(press, height, temp, theta)
  if (abs(height - 5574.4) > epsilon .or. abs(temp - 251.9) > epsilon .or. abs(theta - 307.1) > epsilon) then
     print *, "Test 3 failed. height=", height, " temp=", temp, " theta=", theta
     errors = errors + 1
  end if

  if (errors > 0) then
     print *, "TEST FAILED. Total errors:", errors
     stop 1
  else
     print *, "SUCCESS"
  end if

end program test_w3fa03
