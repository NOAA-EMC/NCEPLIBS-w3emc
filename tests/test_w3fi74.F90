! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi74() function.
!
! Ed Hartnett, 2/28/23
program test_w3fi74
  implicit none
  integer :: i, iret
  integer :: kf, nbit
  parameter(kf = 4)
  parameter(nbit = 8)
  integer maxbit
  parameter(maxbit=24)  
  character pds(400),grib(1000+kf*12)  
  real f(kf)
  integer igflag, igrid
  parameter (igflag = 0)
  integer ibm(kf),ipds(200),igds(200),ibds(200)
  integer kfo, lgrib, icomp
  integer ierr

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

  ! This call comes from NCEPLIBS-grib_util cnvgrb, putbexn.F90.
  ! call w3fi72(0, f, 0, nbit, 1, ipds, pds,  &
  !      igflag, igrid, igds, icomp, 0, ibm, kf, ibds,  &
  !      kfo, grib, lgrib, iret)
  ! print *, kfo, lgrib, iret

  print *, "SUCCESS"
end program test_w3fi74
