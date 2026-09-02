program test_w3ft16
  implicit none
  real :: alola(95, 91)
  real :: bthin(3447)
  integer :: interp, i, j
  real :: sum_bthin
  integer :: num_fails
  real, parameter :: tol = 1e-4

  num_fails = 0

  ! Initialize alola with non-linear pattern
  do j = 1, 91
    do i = 1, 95
      alola(i, j) = sin(real(i)) * cos(real(j)*0.1)
    end do
  end do

  ! Test 1: Linear interpolation (interp = 1)
  interp = 1
  bthin = 0.0
  call w3ft16(alola, bthin, interp)
  
  sum_bthin = sum(bthin)
  print *, "Test 1 sum:", sum_bthin
  print *, "bthin(100):", bthin(100)
  
  if (abs(sum_bthin - 4.61810732) > tol) then
     print *, "FAILED Test 1 sum"
     num_fails = num_fails + 1
  end if
  if (abs(bthin(100) - (-0.691438973)) > tol) then
     print *, "FAILED Test 1 bthin(100)"
     num_fails = num_fails + 1
  end if

  ! Test 2: Biquadratic interpolation (interp = 2)
  interp = 2
  bthin = 0.0
  call w3ft16(alola, bthin, interp)

  sum_bthin = sum(bthin)
  print *, "Test 2 sum:", sum_bthin
  print *, "bthin(100):", bthin(100)

  if (abs(sum_bthin - 4.93098593) > tol) then
     print *, "FAILED Test 2 sum"
     num_fails = num_fails + 1
  end if
  if (abs(bthin(100) - (-0.771619737)) > tol) then
     print *, "FAILED Test 2 bthin(100)"
     num_fails = num_fails + 1
  end if

  if (num_fails > 0) then
     stop 1
  end if
  print *, "All tests passed!"

end program test_w3ft16
