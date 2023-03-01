! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi72() function.
!
! Ed Hartnett, 2/28/23
program test_w3fi72
  implicit none
  integer :: i, iret
  integer :: kf, nbit
  parameter(kf = 489900)
  parameter(nbit = 8)
  integer maxbit
  parameter(maxbit=24)  
  character pds(40000)
  real, dimension(:), allocatable :: fld
  character*1, dimension(:), allocatable :: grib
  integer igflag, igrid
  parameter (igflag = 0)
  integer ibm(kf),ipds(200),igds(200),ibds(200)
  integer kfo, lgrib, icomp
  integer ierr

  print *, "Testing w3fi72..."

  allocate(fld(kf))
  allocate(grib(1000+kf*12))

  ! Fill up some test data.
  do i = 1, 200
     ipds(i) = 0
  end do
  do i = 1, kf
     fld(i) = i / 2
  end do

  ! Fill the igds array. This call comes from test_w3fi71.F90.
  icomp = 0
  igrid = 172
  call w3fi71(igrid, igds, ierr)
  if (ierr .ne. 0) stop 1

  ! This call comes from NCEPLIBS-grib_util cnvgrb, putbexn.F90.
  call w3fi72(0, fld, 0, nbit, 1, ipds, pds,  &
       igflag, igrid, igds, icomp, 0, ibm, kf, ibds,  &
       kfo, grib, lgrib, iret)
!  print *, kfo, lgrib, iret
  if (kfo .ne. 489900) stop 2
  if (lgrib .ne. 489956) stop 3
  if (iret .ne. 0) stop 4

  deallocate(fld)
  deallocate(grib)

  print *, "SUCCESS"
end program test_w3fi72
