!> @file
!> @brief Return the real kind and integer kind used in w3 lib.
!> @author Jun Wang @date 2011-06-24

!> This subprogram returns the real kind and the integer kind that the w3 lib
!> is compiled with.
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 2011-06-24 | Jun Wang | Initial
!>
!> @param[out] KINDREAL Kind of real number in w3 lib
!> @param[out] KINDINT Kind of integer number in w3 lib
!>
!> @author Jun Wang @date 2011-06-24
      subroutine w3kind(kindreal,kindint)
      IMPLICIT NONE
!
      integer,intent(out) :: kindreal,kindint
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get real kind from a real number
      kindreal=kind(1.0)
      kindint=kind(1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
