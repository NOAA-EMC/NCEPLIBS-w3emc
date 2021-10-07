C> @file
C> @brief Year, month, day from julian day number
C> @author Ralph Jones @date 1987-03-29

C> Computes year (4 digits), month, day, day of week, day of year from julian
C> day number. this subroutine will work from 1583 a.d. to 3300 a.d.
C>
C> ### Program History Log:
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1987-03-29 | Ralph Jones |
C> 1989-10-25 | Ralph Jones | Convert to cray cft77 fortran
C>
C> @param[in] JLDAYN (INT) Julian day number
C> @param[out] IYEAR (INT) Year (4 digits)
C> @param[out] MONTH (INT) Month
C> @param[out] IDAY (INT) Day
C> @param[out] IDAYWK (INT) Day of week (1 is sunday, 7 is sat)
C> @param[out] IDAYYR (INT) Day of year (1 to 366)
C>
C> @note A julian day number can be computed by using one of the following
C> statement functions. A day of week can be computed from the julian day
C> number. A day of year can be computed from a julian day number and year.
C>
C> JDN(IYEAR,MONTH,IDAY) = IDAY - 32075
C> + 1461 * (IYEAR + 4800 + (MONTH - 14) / 12) / 4
C> + 367 * (MONTH - 2 - (MONTH -14) / 12 * 12) / 12
C> - 3 * ((IYEAR + 4900 + (MONTH - 14) / 12) / 100) / 4
C>
C> IYR (4 DIGITS) , IDYR(1-366) Day of year
C>
C> JULIAN(IYR,IDYR) = -31739 + 1461 * (IYR + 4799) / 4
C>                    -3 * ((IYR + 4899) / 100) / 4 + IDYR
C>
C> Day of week from julian day number, 1 is sunday, 7 is saturday.
C>
C> JDAYWK(JLDAYN) = MOD((JLDAYN + 1),7) + 1
C>
C> Day of year from julian day number and 4 digit year.
C>
C> JDAYYR(JLDAYN,IYEAR) = JLDAYN -
C>   (-31739+1461*(IYEAR+4799)/4-3*((IYEAR+4899)/100)/4)
C>
C> The first function was in a letter to the editor communications
C> of the acm volume 11 / number 10 / october, 1968. the 2nd
C> function was derived from the first. This subroutine was also
C> included in the same letter. Julian day number 1 is
C> jan 1,4713 b.c. a julian day number can be used to replace a
C> day of century, this will take care of the date problem in
C> the year 2000, or reduce program changes to one line change
C> of 1900 to 2000. Julian day numbers can be used for finding
C> record numbers in an archive or day of week, or day of year.
C>
C> @author Ralph Jones @date 1987-03-29
      SUBROUTINE W3FS26(JLDAYN,IYEAR,MONTH,IDAY,IDAYWK,IDAYYR)
C
       L      = JLDAYN + 68569
       N      = 4 * L / 146097
       L      = L - (146097 * N + 3) / 4
       I      = 4000 * (L + 1) / 1461001
       L      = L - 1461 * I / 4 + 31
       J      = 80 * L / 2447
       IDAY   = L - 2447 * J / 80
       L      = J / 11
       MONTH  = J + 2 - 12 * L
       IYEAR  = 100 * (N - 49) + I + L
       IDAYWK = MOD((JLDAYN + 1),7) + 1
       IDAYYR = JLDAYN -
     &  (-31739 +1461 * (IYEAR+4799) / 4 - 3 * ((IYEAR+4899)/100)/4)
       RETURN
       END
