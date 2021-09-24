C> @file
C> @brief Convert to ibm370 floating point
C> @author John Hennessy @date 1985-09-15

C> Converts floating point number from machine
C> representation to grib representation (ibm370 32 bit f.p.).
C>
C> Program history log:
C> - John Hennessy 1985-09-15
C> - Ralph Jones 1992-09-23 Change name, add doc block
C> - Ralph Jones 1993-10-27 Change to agree with hennessy changes
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive
C>
C> @param[in] PVAL Floating point number to be converted
C> @param[in] KBITS Number of bits in computer word (32 or 64)
C> @param[out] KEXP 8 Bit signed exponent
C> @param[out] KMANT 24 Bit mantissa (fraction)
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author John Hennessy @date 1985-09-15
      SUBROUTINE W3FI76(PVAL,KEXP,KMANT,KBITS)
C
C********************************************************************
C*
C*    NAME      : CONFP3
C*
C*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
C*                REPRESENTATION TO GRIB REPRESENTATION.
C*
C*    INPUT     : PVAL  - FLOATING POINT NUMBER TO BE CONVERTED.
C*    KBITS     : KBITS - NUMBER OF BITS IN COMPUTER WORD
C*
C*    OUTPUT    : KEXP  - 8 BIT SIGNED EXPONENT
C*                KMANT - 24 BIT MANTISSA
C*                PVAL  - UNCHANGED.
C*
C*    JOHN HENNESSY , ECMWF   18.06.91
C*
C********************************************************************
C
C
C     IMPLICIT NONE
C
      INTEGER IEXP
      INTEGER ISIGN
C
      INTEGER KBITS
      INTEGER KEXP
      INTEGER KMANT
C
      REAL PVAL
      REAL ZEPS
      REAL ZREF
C
C     TEST FOR FLOATING POINT ZERO
C
      IF (PVAL.EQ.0.0) THEN
        KEXP  = 0
        KMANT = 0
        GO TO 900
      ENDIF
C
C     SET ZEPS TO 1.0E-12 FOR 64 BIT COMPUTERS (CRAY)
C     SET ZEPS TO 1.0E-8  FOR 32 BIT COMPUTERS
C
      IF (KBITS.EQ.32) THEN
        ZEPS = 1.0E-8
      ELSE
        ZEPS = 1.0E-12
      ENDIF
      ZREF = PVAL
C
C     SIGN OF VALUE
C
      ISIGN = 0
      IF (ZREF.LT.0.0) THEN
        ISIGN =   128
        ZREF    = - ZREF
      ENDIF
C
C     EXPONENT
C
      IEXP = INT(ALOG(ZREF)*(1.0/ALOG(16.0))+64.0+1.0+ZEPS)
C
      IF (IEXP.LT.0  ) IEXP = 0
      IF (IEXP.GT.127) IEXP = 127
C
C     MANTISSA
C
C     CLOSEST NUMBER IN GRIB FORMAT TO ORIGINAL NUMBER
C     (EQUAL TO, GREATER THAN OR LESS THAN ORIGINAL NUMBER).
C
      KMANT = NINT (ZREF/16.0**(IEXP-70))
C
C     CHECK THAT MANTISSA VALUE DOES NOT EXCEED 24 BITS
C     16777215 = 2**24 - 1
C
      IF (KMANT.GT.16777215) THEN
         IEXP  = IEXP + 1
C
C     CLOSEST NUMBER IN GRIB FORMAT TO ORIGINAL NUMBER
C     (EQUAL TO, GREATER THAN OR LESS THAN ORIGINAL NUMBER).
C
         KMANT = NINT (ZREF/16.0**(IEXP-70))
C
C        CHECK MANTISSA VALUE DOES NOT EXCEED 24 BITS AGAIN
C
         IF (KMANT.GT.16777215) THEN
           PRINT *,'BAD MANTISSA VALUE FOR PVAL = ',PVAL
         ENDIF
      ENDIF
C
C     ADD SIGN BIT TO EXPONENT.
C
      KEXP = IEXP + ISIGN
C
  900 CONTINUE
C
      RETURN
      END
