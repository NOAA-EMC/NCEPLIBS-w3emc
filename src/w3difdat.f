!> @file
!> @brief Return a time interval between two dates.
!> @author Mark Iredell @date 1998-01-05

!> Returns the elapsed time interval from
!> an NCEP absolute date and time given in the second argument until
!> an NCEP absolute date and time given in the first argument.
!> The output time interval is in one of seven canonical forms
!> of the ncep relative time interval data structure.
!>
!> Program history log:
!> - Mark Iredell 1998-01-05
!>
!> @param[in] JDAT Integer (8) ncep absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!> @param[in] IDAT Integer (8) ncep absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!> @param[in] IT Integer relative time interval format type
!> (-1 for first reduced type (hours always positive),
!> 0 for second reduced type (hours can be negative),
!> 1 for days only, 2 for hours only, 3 for minutes only,
!> 4 for seconds only, 5 for milliseconds only)
!> @param[out] RINC Real (5) ncep relative time interval
!> (days, hours, minutes, seconds, milliseconds)
!> (time interval is positive if jdat is later than idat.)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3difdat(jdat,idat,it,rinc)
      integer jdat(8),idat(8)
      real rinc(5)
      real rinc1(5)
!  difference the days and time and put into canonical form
      rinc1(1)=iw3jdn(jdat(1),jdat(2),jdat(3))-
     &         iw3jdn(idat(1),idat(2),idat(3))
      rinc1(2:5)=jdat(5:8)-idat(5:8)
      call w3reddat(it,rinc1,rinc)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
