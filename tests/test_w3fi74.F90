! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi74() function.
!
! Ed Hartnett, 2/28/23
program test_w3fi74
  implicit none
  integer :: i
!  integer :: iret
  integer :: kf, nbit
  parameter(kf = 4)
  parameter(nbit = 8)
  integer maxbit
  parameter(maxbit=24)  
!  character pds(400)
!  character grib(1000+kf*12)  
  real f(kf)
  integer igflag, igrid
  parameter (igflag = 0)
  integer ipds(200),igds(200)
  integer icomp
!  integer kfo, lgrib
  integer ierr
  integer npts
  character*1 gds(200)
  integer lengds

  print *, "Testing w3fi74..."

  ! Fill up some test data.
  do i = 1, 200
     ipds(i) = 0
  end do
  do i = 1, kf
     f(i) = i / 2
  end do

  ! Fill the igds array. This call comes from test_w3fi71.F90.
  icomp = 0
  igrid = 172
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 1

  ! Fill the igds array. This call comes from w3if72.f.
  npts = 4
  call w3fi74(igds, icomp, gds, lengds, npts, ierr)
!  print *, lengds, npts
  if (ierr .ne. 0) stop 1
  if (lengds .ne. 32 .or. npts .ne. 489900) stop 2

  print *, "SUCCESS"
end program test_w3fi74
