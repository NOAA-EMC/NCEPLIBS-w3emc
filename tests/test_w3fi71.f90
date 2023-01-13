 program test_w3fi71
   implicit none
   integer :: igrid, igds(18), ierr
   print*,"Testing w3fi71..."
   igrid = 172
   call w3fi71(igrid, igds, ierr)
   print*,'ierr ',ierr
   print*,'igds ',igds

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


   print*,"SUCCESS"
 end program test_w3fi71
