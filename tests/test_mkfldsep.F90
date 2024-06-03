! This is a test in the NCEPLIBS-w3emc project.
!
! Test the mkfldsep subroutine. This is used in grip_utils utilities tocgrib2 and tocgrib2super.
!
! Edward Hartnett, 6/3/24
program test_mkflsep
  implicit none
  
  character * 1   csep(80), lenbull_cin(4)
  integer :: iopt, lenin, lenbull, lenbull_in
  integer :: lenout, itot, i
  integer, parameter :: lenhead = 21
  character(len=4),parameter :: cstar='****', clb='####'
  
  print *, "Testing mkfldsep(), expect and ignore error messages..."

  ! This will succeed.
  print *, 'testing with iopt 1...'
  iopt = 1
  lenin = 18
  itot = 5000
  lenbull = itot + lenhead
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  do i = 1, lenin
     print *, i, ichar(csep(i))
  end do

  ! Check every byte of result.
  do i = 1, 4
     if (csep(i) .ne. '#') stop 100
  end do
  if (csep(5) .ne. '0' .or. csep(6) .ne. '1' .or. csep(7) .ne. '8') stop 101
  ! This is the length of message plus header as an i6.6.
  if (csep(8) .ne. '0' .or. csep(9) .ne. '0' .or. csep(10) .ne. '5') stop 102
  if (csep(11) .ne. '0' .or. csep(12) .ne. '2' .or. csep(13) .ne. '1') stop 103
  do i = 14, 17
     if (csep(i) .ne. '#') stop 105
  end do
  if (csep(18) .ne. char(10)) stop 107
  print *, 'OK!'
  
  ! ! This will fail, because iopt must be 1 or 2. csep will be set to
  ! ! spaces.
  ! iopt = 3
  ! lenin = 19
  ! itot = 5000
  ! lenbull = itot + lenhead
  ! call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  ! do i = 1, lenin
  !    if (csep(i) .ne. ' ') stop 10
  ! end do
  
  print *, "SUCCESS"
end program test_mkflsep

