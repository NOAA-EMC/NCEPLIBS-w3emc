! This is a test in the NCEPLIBS-w3emc project.
!
! Test the summary subroutine.
!
! Kyle Gerheiser
program test_summary
  implicit none
  print *, "Testing summary()..."
  call start()
  call summary()
  print *, "SUCCESS"
end program test_summary

