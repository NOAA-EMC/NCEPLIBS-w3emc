C> @file
C> @brief Number of minutes since jan 1, 1978
C> @author A. Desmarais @date 1984-06-21

C> Calculates the number of minutes since 0000, 1 January 1978.
C>
C> ### Program History Log:
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1984-06-21 | A. Desmarais | Initial.
C> 1989-07-14 | Ralph Jones | Convert to cyber 205 fortran 200, change logic so it will work in 21 century.
C> 1989-11-02 | Ralph Jones | Convert to cray cft77 fortran.
C>
C> @param[in] IDATE (INTEGER Size 5) Array containing year of century, month,
C> day, hour and minute. IDATE(1) may be a two digit year or 4. If 2 digits
c> and GE than 78 1900 is added to it. If LT 78 then 2000 is added to it. If 4
C> digits the subroutine will work correctly to the year 3300 A.D.
C> @param[out] NMIN (INTEGER) Number of minutes since 1 January 1978.
C>
C> @author A. Desmarais @date 1984-06-21
      SUBROUTINE W3FS21(IDATE, NMIN)
C
      INTEGER  IDATE(5)
      INTEGER  NMIN
      INTEGER  JDN78
C
      DATA  JDN78 / 2443510 /
C
C***   IDATE(1)       YEAR OF CENTURY
C***   IDATE(2)       MONTH OF YEAR
C***   IDATE(3)       DAY OF MONTH
C***   IDATE(4)       HOUR OF DAY
C***   IDATE(5)       MINUTE OF HOUR
C
      NMIN  = 0
C
      IYEAR = IDATE(1)
C
      IF (IYEAR.LE.99) THEN
        IF (IYEAR.LT.78) THEN
          IYEAR = IYEAR + 2000
        ELSE
          IYEAR = IYEAR + 1900
        ENDIF
      ENDIF
C
C     COMPUTE JULIAN DAY NUMBER FROM YEAR, MONTH, DAY
C
      IJDN  = IW3JDN(IYEAR,IDATE(2),IDATE(3))
C
C     SUBTRACT JULIAN DAY NUMBER OF JAN 1,1978 TO GET THE
C     NUMBER OF DAYS BETWEEN DATES
C
      NDAYS = IJDN - JDN78
C
C***  NUMBER OF MINUTES
C
      NMIN = NDAYS * 1440 + IDATE(4) * 60 + IDATE(5)
C
      RETURN
      END
