! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi74() function.
!
! Ed Hartnett, 2/28/23
program test_w3fi74
  implicit none
  integer igrid
  integer igds(200)
  integer icomp
  integer npts
  character*1 gds(200)
  integer lengds
  integer ierr
  integer i
  character expected_gds(32)
  expected_gds(:) = (/ char(0), char(0), char(32), char(0), &
       char(255), char(5), char(2), char(178), char(2), &
       char(198), char(128), char(144), char(35), char(131), &
       char(92), char(34), char(0), char(129), char(56), &
       char(128), char(0), char(49), char(156), char(0), &
       char(49), char(156), char(128), char(64), char(0), &
       char(0), char(0), char(0) /)

  print *, "Testing w3fi74..."

  ! Fill the igds array. This call comes from test_w3fi71.F90.
  igrid = 172
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 1

  ! Fill the igds array. This call comes from w3if72.f.
  icomp = 0
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
  if (ierr .ne. 0) stop 1
  if (lengds .ne. 32 .or. npts .ne. 489900) stop 2
  do i = 1, 32
     if (gds(i) .ne. expected_gds(i)) stop 4
     !print *,'char(', ichar(gds(i)), '), '
  end do

  print *, "SUCCESS"
end program test_w3fi74
