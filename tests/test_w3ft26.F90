program test_w3ft26
  implicit none
  real :: fld(360, 181)
  real :: hi(3447)
  integer :: igpts, nstop
  integer :: i, j
  integer :: num_fails
  real, parameter :: tol = 1e-3

  num_fails = 0

  ! Initialize field with non-linear pattern
  do j = 1, 181
    do i = 1, 360
      fld(i, j) = sin(real(i)*0.1) * cos(real(j)*0.05)
    end do
  end do

  ! Test 1: MAPNUM = 37 (NH, Quad 1, W3FT16)
  call w3ft26(37, fld, hi, igpts, nstop)
  if (abs(sum(hi) - (-0.851646423)) > tol .or. abs(hi(100) - (-0.0764656886)) > tol .or. igpts /= 3447 .or. nstop /= 0) then
     print *, "FAILED Test 1 (MAPNUM=37)"
     num_fails = num_fails + 1
  end if

  ! Test 2: MAPNUM = 42 (SH, Quad 2, W3FT17)
  call w3ft26(42, fld, hi, igpts, nstop)
  if (abs(sum(hi) - (351.792725)) > tol .or. abs(hi(100) - (-0.404677331)) > tol .or. igpts /= 3447 .or. nstop /= 0) then
     print *, "FAILED Test 2 (MAPNUM=42)"
     num_fails = num_fails + 1
  end if

  ! Test 3: MAPNUM = 39 (NH, Quad 3, W3FT16)
  call w3ft26(39, fld, hi, igpts, nstop)
  if (abs(sum(hi) - (265.559082)) > tol .or. abs(hi(100) - (0.106824182)) > tol .or. igpts /= 3447 .or. nstop /= 0) then
     print *, "FAILED Test 3 (MAPNUM=39)"
     num_fails = num_fails + 1
  end if

  ! Test 4: MAPNUM = 44 (SH, Quad 4, W3FT17)
  call w3ft26(44, fld, hi, igpts, nstop)
  if (abs(sum(hi) - (122.035034)) > tol .or. abs(hi(100) - (0.00184853585)) > tol .or. igpts /= 3447 .or. nstop /= 0) then
     print *, "FAILED Test 4 (MAPNUM=44)"
     num_fails = num_fails + 1
  end if

  ! Test 5: MAPNUM = 99 (Invalid)
  hi = 0.0
  igpts = -1
  nstop = -1
  call w3ft26(99, fld, hi, igpts, nstop)
  if (abs(sum(hi) - 0.0) > tol .or. abs(hi(100) - 0.0) > tol .or. igpts /= 0 .or. nstop /= 24) then
     print *, "FAILED Test 5 (MAPNUM=99)"
     num_fails = num_fails + 1
  end if

  if (num_fails > 0) then
     stop 1
  end if
  print *, "All tests passed!"

end program test_w3ft26
