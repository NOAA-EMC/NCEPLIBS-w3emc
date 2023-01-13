 program test_w3fi71
   implicit none
   integer :: igrid, igds(18), ierr
   print*,"Testing w3fi71..."
   igrid = 172
   call w3fi71(igrid, igds, ierr)
   print*,'ierr ',ierr
   print*,'igds ',igds
   print*,"SUCCESS"
 end program test_w3fi71
