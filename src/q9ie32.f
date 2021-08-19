C> @file
C> @brief Convert IBM370 F.P. to IEEE F.P.
C> @author Ralph Jones @date 1990-06-04

C> Convert ibm370 32 bit floating point numbers to ieee
C> 32 bit task 754 floating point numbers.
C>
C> Program history log:
C> - Ralph Jones 1990-06-04 Change to sun fortran 1.3
C> - Ralph Jones 1990-07-14 Change ishft to lshift or lrshft
C> - Ralph Jones 1991-03-09 Change to silicongraphics fortran
C> - Ralph Jones 1992-07-20 Change to ibm aix xl fortran
C> - Ralph Jones 1995-11-15 Add save statement
C> - Stephen Gilbert 1998-11-15 Specified 4-byte integers for IBM SP
C>
C> @param[in] A REAL*4 Array of ibm370 32 bit floating point numbers.
C> @param[out] N Number of points to convert.
C> @param[out] B REAL*4 Array of ieee 32 bit floating point numbers.
C> @param[out] ISTAT Number of point greater than 10e+38, numbers are set to
c> ieee infinity, one is added to istat. Numbers less than
c> e-38 are set to zero, one is  not added to istat.
C>
C> @note See ieee task 754 standard floating point arithmetic
C> for more information about IEEE F.P.
C>
C> @author Ralph Jones @date 1990-06-04
      SUBROUTINE Q9IE32(A,B,N,ISTAT)
C
       INTEGER(4)      A(*)
       INTEGER(4)      B(*)
       INTEGER(4)      SIGN
       INTEGER(4) INFIN,MASKFR,MASKSN,MASK21,MASK22,MASK23
       INTEGER(4) ITEMP,ISIGN,IEEEXP,K,LTEMP
C
       SAVE
C
       DATA  INFIN /Z'7F800000'/
       DATA  MASKFR/Z'007FFFFF'/
       DATA  MASKSN/Z'7FFFFFFF'/
       DATA  MASK21/Z'00200000'/
       DATA  MASK22/Z'00400000'/
       DATA  MASK23/Z'00800000'/
       DATA  SIGN  /Z'80000000'/
C
           IF (N.LT.1) THEN
             ISTAT = -1
             RETURN
           ENDIF
C
           ISTAT = 0
C
         DO 40 I = 1,N
           ISIGN = 0
           ITEMP = A(I)
C
C          TEST SIGN BIT
C
           IF (ITEMP.EQ.0) GO TO 30
C
           IF (ITEMP.LT.0) THEN
C
             ISIGN = SIGN
C
C            SET SIGN BIT TO ZERO
C
             ITEMP = IAND(ITEMP,MASKSN)
C
           END IF
C
C
C          CONVERT IBM EXPONENT TO IEEE EXPONENT
C
           IEEEXP = (ISHFT(ITEMP,-24_4) - 64_4) * 4 + 126
C
           K = 0
C
C          TEST BIT 23, 22, 21
C          ADD UP NUMBER OF ZERO BITS IN FRONT OF IBM370 FRACTION
C
           IF (IAND(ITEMP,MASK23).NE.0) GO TO 10
           K = K + 1
           IF (IAND(ITEMP,MASK22).NE.0) GO TO 10
           K = K + 1
           IF (IAND(ITEMP,MASK21).NE.0) GO TO 10
           K = K + 1
C
 10      CONTINUE
C
C          SUBTRACT ZERO BITS FROM EXPONENT
C
           IEEEXP = IEEEXP - K
C
C          TEST FOR OVERFLOW
C
           IF (IEEEXP.GT.254) GO TO 20
C
C          TEST FOR UNDERFLOW
C
           IF (IEEEXP.LT.1) GO TO 30
C
C          SHIFT IEEE EXPONENT TO BITS 1 TO 8
C
           LTEMP = ISHFT(IEEEXP,23_4)
C
C          SHIFT IBM370 FRACTION LEFT K BIT, AND OUT BITS 0 - 8
C          OR TOGETHER THE EXPONENT AND THE FRACTION
C          OR IN SIGN BIT
C
           B(I)  = IOR(IOR(IAND(ISHFT(ITEMP,K),MASKFR),LTEMP),ISIGN)
C
           GO TO 40
C
 20      CONTINUE
C
C          OVERFLOW , SET TO IEEE INFINITY, ADD 1 TO OVERFLOW COUNTER
C
           ISTAT  = ISTAT + 1
           B(I)   = IOR(INFIN,ISIGN)
           GO TO 40
C
 30      CONTINUE
C
C          UNDERFLOW , SET TO ZERO
C
           B(I)   =  0
C
 40      CONTINUE
C
         RETURN
       END
