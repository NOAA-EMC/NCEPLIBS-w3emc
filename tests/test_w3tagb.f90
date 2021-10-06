program test_w3tagb
  implicit none
  character(len=:), allocatable :: prog, org
  integer :: year, julian_day, hundreths_of_julian_day

  print *, "Testing w3tagb..."
  
  prog = "test_w3tagb"
  org = "emc"
  year = 2021
  julian_day = 21278
  hundreths_of_julian_day = 0

  ! prints information
  call w3tagb(prog, year, julian_day, hundreths_of_julian_day, org)
  print *, "SUCCESS"
end program test_w3tagb
