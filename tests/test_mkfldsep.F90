! This is a test in the NCEPLIBS-w3emc project.
!
! Test the mkfldsep subroutine. This is used in grip_utils utilities tocgrib2 and tocgrib2super.
!
! Edward Hartnett, 6/3/24
program test_mkflsep
  implicit none
  
  character * 1   csep(80)
  integer :: iopt, lenin, lenbull
  integer :: lenout, itot, i
  integer, parameter :: lenhead = 21
  
  print *, "Testing mkfldsep(), expect and ignore error messages..."

  ! This will fail, because iopt must be 1 or 2. csep will be set to
  ! spaces.
  iopt = 3
  lenin = 19
  itot = 5000
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  do i = 1, lenin
     if (csep(i) .ne. ' ') stop 10
  end do
  
  ! This will succeed.
  iopt = 1
  lenin = 19
  itot = 5000
  call mkfldsep(csep, iopt, lenin, lenbull, lenout)
  do i = 1, lenin
     print *, i, ichar(csep(i))
  end do
  print *, "SUCCESS"
end program test_mkflsep

