!> @file
!> @brief Returns the integer day of week, the day
!> of year, and julian day given an NCEP absolute date and time.
!> @author Mark Iredell @date 1998-01-05

!> Program history log:
!> - Mark Iredell 1998-01-05
!>
!> @param[in] IDAT Integer (8) NCEP absolute date and time
!> (year, month, day, time zone, hour, minute, second, millisecond)
!> @param[out] JDOW Integer day of week (1-7, where 1 is sunday)
!> @param[out] JDOY Integer day of year (1-366, where 1 is january 1)
!> @param[out] JDAY Integer julian day (day number from jan. 1,4713 b.c.)
!>
!> @author Mark Iredell @date 1998-01-05
      subroutine w3doxdat(idat,jdow,jdoy,jday)
      integer idat(8)
!  get julian day and then get day of week and day of year
      jday=iw3jdn(idat(1),idat(2),idat(3))
      call w3fs26(jday,jy,jm,jd,jdow,jdoy)
      end
