C> @file
C
C> SUBPROGRAM: W3FS13         YEAR, MONTH, AND DAY TO DAY OF YEAR
C>   AUTHOR: CHASE, P.        ORG: W345       DATE: 85-07-31
C>
C> ABSTRACT: CONVERTS YEAR, MONTH AND DAY TO DAY OF YEAR.
C>
C> PROGRAM HISTORY LOG:
C>   85-07-31  R.E.JONES
C>   89-11-02  R.E.JONES  CONVERT TO CRAY CFT77 FORTRAN
C>
C> USAGE:  CALL W3FS13(IYR, IMO, IDA, JDY)
C>
C>   INPUT VARIABLES:
C>     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C>     ------ --------- -----------------------------------------------
C>     IYR    ARG LIST  INTEGER YEAR OF CENTURY, 00-99 OR YEAR OF ERA,
C>                      1901-2099
C>     IMO    ARG LIST  INTEGER MONTH OF YEAR, 1-12
C>     IDA    ARG LIST  INTEGER DAY OF MONTH,  1-31
C>
C>   OUTPUT VARIABLES:
C>     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C>     ------ --------- -----------------------------------------------
C>     JDY    ARG LIST  INTEGER DAY OF YEAR,  1-366
C>
C>   SUBPROGRAMS CALLED:
C>     NAMES                                                   LIBRARY
C>     ------------------------------------------------------- --------
C>     IAND                                                    SYSTEM
C>
C>   REMARKS: THIS PROCEDURE IS VALID ONLY FROM THE YEARS 1901-2099
C>            INCLUSIVE.
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY Y-MP8/832
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
