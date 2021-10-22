!> @file
!> @brief Determine the validity of a date and time
!> @author Mark Iredell @date 1998-01-05

!> This logical function returns true if the input is a valid NCEP absolute date and time.
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 1998-01-05 | Mark Iredell | Initial.
!>
!> @param[in] IDAT NCEP absolute date and time (year, month, day, time zone,
!> hour, minute, second, millisecond)
!> @return W3VALDAT True if idat is a valid NCEP date and time
!>
!> @author Mark Iredell @date 1998-01-05
      logical function w3valdat(idat)
      integer idat(8)
      real rinc1(5),rinc2(5)
      integer jdat(8)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  essentially move the date and time by a zero time interval
!  and see if the same date and time is returned
      rinc1(1)=0
      rinc1(2:5)=idat(5:8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
!  the time zone is valid if it is in signed hhmm format
!  with hh between -23 and 23 and mm equal to 00 or 30
      jdat(4)=mod(idat(4)/100,24)*100+mod(mod(idat(4),100),60)/30*30
      jdat(5:8)=nint(rinc2(2:5))
      w3valdat=all(idat.eq.jdat)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
