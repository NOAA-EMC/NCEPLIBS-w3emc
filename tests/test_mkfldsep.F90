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

  print *, 'testing iopt failure...'
  iopt = 3
  lenin = 19
  itot = 5000
  lenbull = itot + lenhead
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  do i = 1, lenin
     if (csep(i) .ne. ' ') stop 10
  end do
  print *, 'OK!'

  print *, 'testing with iopt 1, lenin = 18...'
  iopt = 1
  lenin = 18
  itot = 5000
  lenbull = itot + lenhead
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  if (lenout .ne. 18) stop 199
  ! do i = 1, lenin
  !    print *, i, ichar(csep(i))
  ! end do

  ! Check every byte of result.
  do i = 1, 4
     if (csep(i) .ne. '#') stop 200
  end do
  if (csep(5) .ne. '0' .or. csep(6) .ne. '1' .or. csep(7) .ne. '8') stop 201
  ! This is the length of message plus header as an i6.6.
  if (csep(8) .ne. '0' .or. csep(9) .ne. '0' .or. csep(10) .ne. '5') stop 202
  if (csep(11) .ne. '0' .or. csep(12) .ne. '2' .or. csep(13) .ne. '1') stop 203
  do i = 14, 17
     if (csep(i) .ne. '#') stop 205
  end do
  if (csep(18) .ne. char(10)) stop 207
  print *, 'OK!'
  
  print *, 'testing with iopt 1, lenin = 23...'
  iopt = 1
  lenin = 23
  itot = 5000
  lenbull = itot + lenhead
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  if (lenout .ne. 23) stop 299
  ! do i = 1, lenin
  !    print *, i, ichar(csep(i))
  ! end do

  ! Check every byte of result.
  do i = 1, 4
     if (csep(i) .ne. '#') stop 100
  end do
  if (csep(5) .ne. '0' .or. csep(6) .ne. '2' .or. csep(7) .ne. '3') stop 101
  ! This is the length of message plus header as an i6.6.
  if (csep(8) .ne. '0' .or. csep(9) .ne. '0' .or. csep(10) .ne. '0') stop 102
  if (csep(11) .ne. '0' .or. csep(12) .ne. '0' .or. csep(13) .ne. '0') stop 103
  if (csep(14) .ne. '0' .or. csep(15) .ne. '5' .or. csep(16) .ne. '0') stop 104
  if (csep(17) .ne. '2' .or. csep(18) .ne. '1') stop 105
  do i = 19, lenin - 5 ! not used in this case
     if (csep(i) .ne. '0') stop 110
  end do
  do i = lenin - 4, lenin - 1
     if (csep(i) .ne. '#') stop 115
  end do
  if (csep(lenin) .ne. char(10)) stop 120
  print *, 'OK!'
  
  print *, 'testing with iopt 2...'
  iopt = 2
  lenin = 23
  itot = 5000
  lenbull = itot + lenhead
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  if (lenout .ne. 19) stop 399
  ! do i = 1, lenout
  !    print *, i, ichar(csep(i))
  ! end do

  ! Check every byte of result.
  do i = 1, 4
     if (csep(i) .ne. '*') stop 300
  end do
  if (csep(5) .ne. '0' .or. csep(6) .ne. '0' .or. csep(7) .ne. '0') stop 302
  if (csep(8) .ne. '0' .or. csep(9) .ne. '0' .or. csep(10) .ne. '0') stop 303
  if (csep(11) .ne. '5' .or. csep(12) .ne. '0' .or. csep(13) .ne. '2') stop 304
  if (csep(14) .ne. '1') stop 305
  if (csep(15) .ne. '*' .or. csep(16) .ne. '*' .or. csep(17) .ne. '*') stop 320
  if (csep(18) .ne. '*' .or. csep(19) .ne. char(10)) stop 305
  print *, 'OK!'
  
  print *, "SUCCESS"
end program test_mkflsep

