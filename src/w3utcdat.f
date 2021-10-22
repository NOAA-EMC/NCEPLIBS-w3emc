!> @file
!> @brief Return the utc date and time
!> @author Mark Iredell @date 1998-01-05

!> This subprogram returns the utc (greenwich) date and time in the NCEP
!> absolute date and time data structure.
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 1998-01-05 | Mark Iredell | Initial.
!> 1999-04-28 | Stephen Gilbert | Added a patch to check for the proper UTC
!> offset. Needed until the IBM bug in date_and_time is fixed. The patch can
!> then be removed. See comments in the section blocked with "&&&&&&&&&&&".
!> 1999-08-12 | Stephen Gilbert | Changed so that czone variable is saved and
!> the system call is only done for first invocation of this routine.
!>
!> @param[in] IDAT NCEP absolute date and time (year, month, day, time zone,
!> hour, minute, second, millisecond)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3utcdat(idat)
      integer idat(8)
      character cdate*8,ctime*10,czone*5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get local date and time but use the character time zone
      call date_and_time(cdate,ctime,czone,idat)
      read(czone,'(i5)') idat(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  convert to hours and minutes to UTC time
!  and possibly adjust the date as well
      idat(6)=idat(6)-mod(idat(4),100)
      idat(5)=idat(5)-idat(4)/100
      idat(4)=0
      if(idat(6).lt.00) then
        idat(6)=idat(6)+60
        idat(5)=idat(5)-1
      elseif(idat(6).ge.60) then
        idat(6)=idat(6)-60
        idat(5)=idat(5)+1
      endif
      if(idat(5).lt.00) then
        idat(5)=idat(5)+24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))-1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      elseif(idat(5).ge.24) then
        idat(5)=idat(5)-24
        jldayn=iw3jdn(idat(1),idat(2),idat(3))+1
        call w3fs26(jldayn,idat(1),idat(2),idat(3),idaywk,idayyr)
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
