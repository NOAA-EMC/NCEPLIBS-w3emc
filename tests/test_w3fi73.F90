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
  character :: cbms(8)
  integer :: expected_cbms(8) = (/ 0, 0, 8, 13, 0, 0, 224, 0 /)
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
  do i = 1, 8
     cbms(i) = achar(0)
  end do
  iblen = BLEN
  call w3fi73(ibflag, ibmap, iblen, bms, lenbms, ierr)
  if (ierr .ne. 0) stop 4
  if (lenbms .ne. 8) stop 5
  cbms = transfer(bms, cbms)
  do i = 1, 8
     print *, ichar(cbms(i))
     if (ichar(cbms(i)) .ne. expected_cbms(i)) stop 100
  end do
!  if (bms(1) .ne. 218628096 .or. bms(2) .ne. 14680064) stop 7

  print*,"SUCCESS"
end program test_w3fi73
