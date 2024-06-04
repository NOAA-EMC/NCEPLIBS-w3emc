! This is a test in the NCEPLIBS-w3emc project.
!
! Test the mkfldsep subroutine. This is used in grip_utils utilities tocgrib2 and tocgrib2super.
!
! Edward Hartnett, 6/3/24
program test_mkflsep
  implicit none
  
  character * 1   csep(80), lenbull_cin(4)
  integer :: iopt, lenin, lenbull, lenbull_in
  integer :: lenin_values(3) = (/ 21, 23, 25 /)
  integer :: lenout, itot, i, j, nnn
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

  ! Test iopt 1 with 3 different values for lenin.
  do j = 1, 3
     iopt = 1
     lenin = lenin_values(j)
     nnn = lenin
     print *, 'testing with iopt 1, lenin = ', lenin
     if (nnn .lt. 23) nnn = 23
     itot = 5000
     lenbull = itot + lenhead
     call mkfldsep(csep, iopt, lenin, lenbull, lenout)
     if (lenout .ne. nnn) stop 299
     ! do i = 1, lenout
     !    print '(i3, a1, z2.2, a1, i2, a1, a)', i, ' ', csep(i), ' ', ichar(csep(i)), ' ', csep(i)
     ! end do

     ! Check every byte of result.
     do i = 1, 4
        if (csep(i) .ne. '#') stop 100
     end do
     if (csep(5) .ne. '0' .or. csep(6) .ne. '2') stop 101
     if (j .le. 2) then
        if (csep(7) .ne. '3') stop 102
     else
        if (csep(7) .ne. '5') stop 102
     endif
     ! This is the length of message plus header as an i6.6.
     if (csep(8) .ne. '0' .or. csep(9) .ne. '0' .or. csep(10) .ne. '0') stop 105
     if (csep(11) .ne. '0' .or. csep(12) .ne. '0' .or. csep(13) .ne. '0') stop 113
     if (csep(14) .ne. '0' .or. csep(15) .ne. '5' .or. csep(16) .ne. '0') stop 114
     if (csep(17) .ne. '2' .or. csep(18) .ne. '1') stop 115
     ! I think this was intended by the original programmer to be a
     ! range of 19:nnn-5. But in the standard these bytes are
     ! "reserved for future use" so it does not matter. So only 
     if (nnn > 23) then
        if (csep(19) .ne. '0') stop 130
     endif
     do i = nnn - 4, nnn - 1
        if (csep(i) .ne. '#') stop 135
     end do
     if (csep(nnn) .ne. char(10)) stop 140
     print *, 'OK!'
  end do
  
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

