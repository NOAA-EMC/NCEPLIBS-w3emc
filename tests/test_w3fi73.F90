! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi73() subroutine.
!
! Ed Hartnett, 9/27/23
program test_w3fi73
  implicit none
  integer :: ibflag, iblen, lenbms
  integer :: BLEN, BMSLEN
  parameter (BLEN = 3)
  parameter (BMSLEN = 3)
  integer :: ibmap(BLEN), bms(BMSLEN)
  character :: cbms(4)
  integer :: i
  integer :: ierr

  print*,"Testing w3fi73..."

  ! Return error code 8 if all ibmap values are 0.
  ibflag = 0
  do i = 1, BLEN
     ibmap(i) = 0
  end do
  iblen = BLEN
  call w3fi73(ibflag, ibmap, iblen, bms, lenbms, ierr)
  if (ierr .ne. 8) stop 2

  ! Return error code 8 if all ibmap values are 0.
  ibflag = 0
  do i = 1, BLEN
     ibmap(i) = 1
  end do
  iblen = BLEN
  call w3fi73(ibflag, ibmap, iblen, bms, lenbms, ierr)
  if (ierr .ne. 0) stop 4
  if (lenbms .ne. 8) stop 5
  cbms = transfer(bms, cbms)
  do i = 0, 8
     print '(z1)', cbms(i)
  end do
!  if (bms(1) .ne. 218628096 .or. bms(2) .ne. 14680064) stop 7

  print*,"SUCCESS"
end program test_w3fi73
