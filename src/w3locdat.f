!> @file
!> @brief Return the local date and time
!> @author Mark Iredell @date 1998-01-05

!> This subprogram returns the local date and time
!> in the ncep absolute date and time data structure.
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 1998-01-05 | Mark Iredell | Initial.
!> 1999-04-28 | Stephen Gilbert | Added a patch to check for the proper
!> UTC offset. Needed until the IBM bug in date_and_time is fixed. The patch
!> can then be removed. See comments in the section blocked with "&&&&&&&&&&&".
!> 1999-08-12 | Stephen Gilbert | Changed so that czone variable is saved
!> and the system call is only done for first invocation of this routine.
!>
!> @param[in] IDAT (8) NCEP absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3locdat(idat)
      integer idat(8)
      character cdate*8,ctime*10,czone*5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get local date and time but use the character time zone
      call date_and_time(cdate,ctime,czone,idat)
      read(czone,'(i5)') idat(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
