program test_w3ft17
  implicit none
  real :: tol
  real :: alola(95,91)
  real :: bthin(3447)
  integer :: interp
  integer :: i, j

  ! Use an appropriate tolerance depending on the kind of real being used
  ! NCEPLIBS supports KIND=4 and KIND=8 usually. 
  ! For safety in comparing REAL variables across potential kind changes:
  tol = 5e-3

  ! Initialize ALOLA with some deterministic pseudo-random or continuous data
  ! We use a quadratic expression to ensure differences between linear and biquadratic interpolations
  do j = 1, 91
    do i = 1, 95
      alola(i,j) = real(i)*real(i)*0.01 + real(j)*0.1 + real(i)*real(j)*0.001
    end do
  end do

  ! 1) Test linear interpolation
  interp = 1
  bthin = 0.0
  call w3ft17(alola, bthin, interp)
  if (abs(bthin(1) - 0.1930) > tol) stop 1
  if (abs(bthin(1723) - 27.2241) > tol) stop 2
  if (abs(bthin(3447) - 104.0530) > tol) stop 3

  ! 2) Test biquadratic interpolation
  ! Changes interp from 1 to 2, testing branches INTERP.NE.INTRPO and LIN=false
  interp = 2
  bthin = 0.0
  call w3ft17(alola, bthin, interp)
  if (abs(bthin(1) - 0.1930) > tol) stop 4
  if (abs(bthin(1723) - 27.2240) > tol) stop 5
  if (abs(bthin(3447) - 104.0530) > tol) stop 6

  ! 3) Test linear again
  ! Changes interp from 2 to 1, testing branches INTERP.NE.INTRPO and LIN=true
  interp = 1
  bthin = 0.0
  call w3ft17(alola, bthin, interp)
  if (abs(bthin(1) - 0.1930) > tol) stop 7
  if (abs(bthin(1723) - 27.2241) > tol) stop 8
  if (abs(bthin(3447) - 104.0530) > tol) stop 9

  print *, "test_w3ft17 passed"
end program test_w3ft17
