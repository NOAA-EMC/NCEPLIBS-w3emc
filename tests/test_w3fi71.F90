! This is a test in the NCEPLIBS-w3emc project.
!
! Test the w3fi71() subroutine.
!
! George Gayno
 program test_w3fi71

! Call routine w3fi71 for NCEP grids 172 and 220.
! Compare the Grib1 grid description section
! against expected values.

   implicit none
   integer :: igrid, igds(18), ierr

   print*,"Testing w3fi71..."

   print*,"Check grid 172"
   igrid = 172
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 2
   if (igds(1) /= 0) stop 4
   if (igds(2) /= 255) stop 6
   if (igds(3) /= 5) stop 8
   if (igds(4) /= 690) stop 10
   if (igds(5) /= 710) stop 12
   if (igds(6) /= -36899) stop 14
   if (igds(7) /= -220194) stop 16
   if (igds(8) /= 0) stop 18
   if (igds(9) /= -80000) stop 20
   if (igds(10) /= 12700) stop 22
   if (igds(11) /= 12700) stop 24
   if (igds(12) /= 128) stop 26
   if (igds(13) /= 64) stop 28
   if (igds(14) /= 0) stop 30
   if (igds(15) /= 0) stop 32
   if (igds(16) /= 0) stop 34
   if (igds(17) /= 0) stop 36
   if (igds(18) /= 0) stop 38

   print*,"Check grid 220"
   igrid = 220
   call w3fi71(igrid, igds, ierr)
   if (ierr /= 0) stop 42
   if (igds(1) /= 0) stop 44
   if (igds(2) /= 255) stop 46
   if (igds(3) /= 5) stop 48
   if (igds(4) /= 345) stop 50
   if (igds(5) /= 355) stop 52
   if (igds(6) /= -36899) stop 54
   if (igds(7) /= -220194) stop 56
   if (igds(8) /= 0) stop 58
   if (igds(9) /= -80000) stop 60
   if (igds(10) /= 25400) stop 62
   if (igds(11) /= 25400) stop 64
   if (igds(12) /= 128) stop 66
   if (igds(13) /= 64) stop 68
   if (igds(14) /= 0) stop 70
   if (igds(15) /= 0) stop 72
   if (igds(16) /= 0) stop 74
   if (igds(17) /= 0) stop 76
   if (igds(18) /= 0) stop 78

   print*,"SUCCESS"
 end program test_w3fi71
