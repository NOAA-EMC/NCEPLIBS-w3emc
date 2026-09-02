program test_w3fi58
  implicit none
  integer, parameter :: max_pts = 10
  integer :: ifield(max_pts)
  integer :: nwork(max_pts)
  character(len=1) :: npfld(max_pts)
  integer :: npts, nbits, len, kmin
  integer :: i

  ! 1) Test NPTS <= 0
  npts = 0
  nbits = -1
  len = -1
  kmin = -1
  call w3fi58(ifield, npts, nwork, npfld, nbits, len, kmin)
  if (nbits /= 0) stop 1
  if (len /= 0) stop 2

  ! 2) Test KMAX == KMIN (constant field)
  npts = 5
  ifield(1:5) = 42
  nbits = -1
  len = -1
  kmin = -1
  call w3fi58(ifield, npts, nwork, npfld, nbits, len, kmin)
  if (nbits /= 0) stop 3
  if (len /= 0) stop 4
  if (kmin /= 42) stop 5

  ! 3) Test nz > 0 (NBITS*NPTS not multiple of 8)
  npts = 2
  ifield(1) = 10
  ifield(2) = 17
  nwork(:) = -1
  npfld(:) = char(0)
  call w3fi58(ifield, npts, nwork, npfld, nbits, len, kmin)
  if (nbits /= 3) stop 6
  if (len /= 1) stop 7
  if (kmin /= 10) stop 8
  if (any(nwork(1:2) /= [0, 7])) stop 9
  if (ichar(npfld(1)) /= 28) stop 10

  ! 4) Test nz == 0 (NBITS*NPTS multiple of 8)
  npts = 4
  ifield(1) = 100
  ifield(2) = 101
  ifield(3) = 102
  ifield(4) = 103
  nwork(:) = -1
  npfld(:) = char(0)
  call w3fi58(ifield, npts, nwork, npfld, nbits, len, kmin)
  if (nbits /= 2) stop 11
  if (len /= 1) stop 12
  if (kmin /= 100) stop 13
  if (any(nwork(1:4) /= [0, 1, 2, 3])) stop 14
  if (ichar(npfld(1)) /= 27) stop 15

  print *, "test_w3fi58 passed"
end program test_w3fi58
