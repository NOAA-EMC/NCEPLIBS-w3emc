! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi72() function.
!
! Ed Hartnett, 2/28/23
program test_w3fi72
  implicit none
  integer :: i, iret
  integer :: kf, nbit
  parameter(kf = 4)
  parameter(nbit = 8)
  
  real f(kf)

  print *, "Testing w3fi72..."

  ! Fill up some test data.
  do i = 1, kf
     f(i) = i / 2
  end do
  
  ! ! This call comes from NCEPLIBS-grib_util cnvgrb, putbexn.F90.
  ! call w3fi72(0, f, 0, nbit, 1, ipds, pds,  &
  !      igflag, igrid, igds, icomp, 0, ibm, kf, ibds,  &
  !      kfo, grib, lgrib, iret)

  print *, "SUCCESS"
end program test_w3fi72
