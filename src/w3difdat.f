!> @file
!! @brief RETURNS THE ELAPSED TIME INTERVAL FROM
!! AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE SECOND ARGUMENT UNTIL
!! AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE FIRST ARGUMENT.      

!> RETURN A TIME INTERVAL BETWEEN TWO DATES
!! @author MARK IREDELL @date 98-01-05
!!
!! THIS SUBPROGRAM RETURNS THE ELAPSED TIME INTERVAL FROM
!! AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE SECOND ARGUMENT UNTIL
!! AN NCEP ABSOLUTE DATE AND TIME GIVEN IN THE FIRST ARGUMENT.
!! THE OUTPUT TIME INTERVAL IS IN ONE OF SEVEN CANONICAL FORMS
!! OF THE NCEP RELATIVE TIME INTERVAL DATA STRUCTURE.
!!
!! PROGRAM HISTORY LOG:
!! - 98-01-05  MARK IREDELL
!!
!! @param[in] JDAT INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!!                (YEAR, MONTH, DAY, TIME ZONE,
!!                 HOUR, MINUTE, SECOND, MILLISECOND)
!! @param[in] IDAT INTEGER (8) NCEP ABSOLUTE DATE AND TIME
!!                (YEAR, MONTH, DAY, TIME ZONE,
!!                 HOUR, MINUTE, SECOND, MILLISECOND)
!! @param[in] IT INTEGER RELATIVE TIME INTERVAL FORMAT TYPE
!!                (-1 FOR FIRST REDUCED TYPE (HOURS ALWAYS POSITIVE),
!!                 0 FOR SECOND REDUCED TYPE (HOURS CAN BE NEGATIVE),
!!                 1 FOR DAYS ONLY, 2 FOR HOURS ONLY, 3 FOR MINUTES ONLY,
!!                 4 FOR SECONDS ONLY, 5 FOR MILLISECONDS ONLY)
!! @param[out] RINC REAL (5) NCEP RELATIVE TIME INTERVAL
!!                (DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS)
!!                (TIME INTERVAL IS POSITIVE IF JDAT IS LATER THAN IDAT.)
!!
!! SUBPROGRAMS CALLED:
!! - iw3jdn() COMPUTE JULIAN DAY NUMBER     
!! - w3reddat() REDUCE A TIME INTERVAL TO A CANONICAL FORM
!!
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
