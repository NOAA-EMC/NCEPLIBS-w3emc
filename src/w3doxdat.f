!> @file
!! @brief RETURNS THE INTEGER DAY OF WEEK, THE DAY
!!   OF YEAR, AND JULIAN DAY GIVEN AN NCEP ABSOLUTE DATE AND TIME.
      
!> RETURN WEEK DAY, YEAR DAY, AND JULIAN DAY
!! @author MARK IREDELL @date 98-01-05
!!
!! THIS SUBPROGRAM RETURNS THE INTEGER DAY OF WEEK, THE DAY
!! OF YEAR, AND JULIAN DAY GIVEN AN NCEP ABSOLUTE DATE AND TIME.
!!
!! PROGRAM HISTORY LOG:
!! -  98-01-05  MARK IREDELL
!!
!! @param[in] IDAT INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!!                (YEAR, MONTH, DAY, TIME ZONE,
!!                 HOUR, MINUTE, SECOND, MILLISECOND)
!! @param[out] JDOW INTEGER DAY OF WEEK (1-7, WHERE 1 IS SUNDAY)
!! @param[out] JDOY INTEGER DAY OF YEAR (1-366, WHERE 1 IS JANUARY 1)
!! @param[out] JDAY INTEGER JULIAN DAY (DAY NUMBER FROM JAN. 1,4713 B.C.)
!!
!! SUBPROGRAMS CALLED:
!! - iw3jdn() COMPUTE JULIAN DAY NUMBER     
!! - w3fs26() YEAR, MONTH, DAY FROM JULIAN DAY NUMBER
!!
      subroutine w3doxdat(idat,jdow,jdoy,jday)
      integer idat(8)
!  get julian day and then get day of week and day of year
      jday=iw3jdn(idat(1),idat(2),idat(3))
      call w3fs26(jday,jy,jm,jd,jdow,jdoy)
      end
