C> @file
C> @brief Year, month, and day to day of year.
C> @author Ralph Jones @date 1985-08-31

C> 0converts year, month and day to day of year.
C>
C> ### Program History Log:
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1985-07-31 | Ralph Jones | Initial.
C> 1989-11-02 | Ralph Jones | Convert to cray cft77 fortran.
C>
C> @param[in] IYR Year of century, 00-99 or year of era, 1901-2099
C> @param[in] IMO Month of year, 1-12
C> @param[in] IDA Day of month, 1-31
C> @param[out] JDY Day of year, 1-366
C>
C> @note This procedure is valid only from the years 1901-2099 inclusive.
C>
      SUBROUTINE W3FS13(IYR,IMO,IDA,JDY)
C
       INTEGER JTABLE(24)
C
       DATA  JTABLE/0,0,31,31,60,59,91,90,121,120,152,151,
     &  182,181,213,212,244,243,274,273,305,304,335,334/
C
       ISET = 0
       IF (IAND(IYR,3).EQ.0) ISET = 1
       I    = IMO * 2 - ISET
       JDY  = JTABLE(I) + IDA
       RETURN
      END
