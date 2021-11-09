!> @file
!> @brief Return a date from a time interval and date
!> @author Mark Iredell @date 1998-08-01

!> This subprogram returns the date and time that is a given
!> NCEP relative time interval from an NCEP absolute date and time.
!> The output is in the NCEP absolute date and time data structure.
!>
!> ### Program History Log:
!> Date | Programmer | Comment
!> -----|------------|--------
!> 1998-01-05 | Mark Iredell | Initial.
!>
!> @param[in] RINC NCEP relative time interval (days, hours, minutes, seconds
!> milliseconds)
!> @param[in] IDAT NCEP absolute date and time (year, month, day, time zone,
!> hour, minute, second, millisecond)
!> @param[in] JDAT NCEP absolute date and time (year, month, day, time zone,
!> hour, minute, second, millisecond) (jdat is later than idat if time
!> interval is positive.)
!>
!> @author Mark Iredell @date 1998-08-01
      subroutine w3movdat(rinc,idat,jdat)

      real rinc(5)
      integer idat(8),jdat(8)
      real rinc1(5),rinc2(5)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  add the interval to the input time of day and put into reduced form
!  and then compute new date using julian day arithmetic.
      rinc1(1)=rinc(1)
      rinc1(2:5)=rinc(2:5)+idat(5:8)
      call w3reddat(-1,rinc1,rinc2)
      jldayn=iw3jdn(idat(1),idat(2),idat(3))+nint(rinc2(1))
      call w3fs26(jldayn,jdat(1),jdat(2),jdat(3),jdow,jdoy)
      jdat(4)=idat(4)
      jdat(5:8)=nint(rinc2(2:5))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end
