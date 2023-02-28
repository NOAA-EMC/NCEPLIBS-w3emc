! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3tagb function.
!
! Kyle Gerheiser
program test_w3tagb
  implicit none
  integer :: year, julian_day, hundreths_of_julian_day

  print *, "Testing w3tagb..."
  
  year = 2021
  julian_day = 21278
  hundreths_of_julian_day = 0

  ! prints information
  call w3tagb("test_w3tagb", year, julian_day, hundreths_of_julian_day, "emc")
  print *, "SUCCESS"
end program test_w3tagb
