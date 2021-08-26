C> @file
C> @brief Real array to 16 bit packed format.
C> @author Ralph Jones @date 1985-07-31

C> Converts IEEE floating point numbers to 16 bit
C> packed office note 84 format. The floating point number are
C> converted to 16 bit signed scaled integers.
C>
C> Program history log:
C> - Ralph Jones 1989-10-20 Convert cyber 205 version of w3ai00 to cray.
C> - Ralph Jones 1990-03-18 Change to use cray integer*2 packer.
C> - Ralph Jones 1990-10-11 Special version to pack grids larger than
C> 32743 words. Will do old and new version.
C> - Ralph Jones 1991-02-16 Changes so equivalence of pack and real8
C> arrays will work.
C> - Ralph Jones 1993-06-10 Changes for array size (512,512) 262144 words.
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive.
C> - Stephen Gilbert 1998-11-18 Changed to pack IEEE values for the IBM SP
C>
C> @param[in] REAL8 Array of cray floating point numbers.
C> @param[in] LABEL Six 8-byte integer words. Must have first 8 of 12 32 bit
C> word office note 84 label. word 6 must have in bits 31-00 the number of
C> real words in array real8 if j is greater than 32743.
C> j in bits 15-0 of the 4th id word is set zero.
C> @param[out] PACK Packed output array of integer words of size 6 + (j+3)/4 ,
C> j = no. points in label (from word 4 bits 15-00). Label will be copied to pack words 1-4.
C> - Pack will contain the following in words 5-6:
C>  - word 5 bits 63-48 Number of bytes in whole record. will not be correct if j > 32743.
C>  - word 5 bits 47-32 Exclusive-or checksum by 16 bit words of whole array pack excluding checksum itself.
C>  - word 5 bits 31-00 Center value a = mean of max and min values. converted to ibm 32 floating point number.
C>  - word 6 bits 63-48 Zero.
C>  - word 6 bits 47-32 16 bit shift value n. the least integer such that abs(x-a)/2**n lt 1 for all x in real8. limited to +-127.
C>  - word 6 bits 31-00 Number of words in real8 if > 32743, right adjusted if <= 32743 set zero.
C>
C> @note Pack and label may be equivalenced.  n, the number of points
C> in a grid is now in 32 bit id word 12.
C>
C> @author Ralph Jones @date 1985-07-31
      SUBROUTINE W3AI00(REAL8,PACK,LABEL)
C
       REAL           REAL8(*)
       REAL           XX(262144)
C
       INTEGER(8)     KK(262144)
       INTEGER(8)     LABEL(6)
       INTEGER(8)     PACK(*)
       INTEGER(8)     TPACK(6)
       INTEGER(8)     MASK16,MASK32,MASKN,IBYTES,IXOR
       INTEGER(8)     IB,N
       REAL(8)        B
       REAL(4)        X,A
       real(4)        rtemp(2)
       integer(8)     irtemp
       equivalence    (irtemp,rtemp(1))
C
       SAVE
C
       EQUIVALENCE    (B,IB)
C
       DATA  MASK16/Z'000000000000FFFF'/
       DATA  MASK32/Z'00000000FFFFFFFF'/
       DATA  MASKN /Z'0000FFFF00000000'/
C
C TRANSFER LABEL DATA TO WORDS 1-4.  GET WORD COUNT, COMPUTE BYTES.
C
       DO 10 I = 1,4
         TPACK(I) = LABEL(I)
 10    CONTINUE
C
       TPACK(5) = 0
       TPACK(6) = 0
C
C      GET J, THE NUMBER OF WORDS IN A GRID, IF ZERO GET THE
C      GET J FROM OFFICE NOTE 84 ID WORD 12.
C
       J       = IAND(MASK16,TPACK(4))
       IF (J.EQ.0) THEN
         TPACK(6) = LABEL(6)
         J       = IAND(MASK32,TPACK(6))
         IF (J.EQ.0) THEN
           PRINT *,' W3AI00: ERROR, NO. OF WORDS IN GRID = 0'
           RETURN
         ENDIF
         IF (J.GT.262144) THEN
           PRINT *,' W3AI00: ERROR, NO. OF WORDS IN GRID = ',J
           PRINT *,' THERE IS A LIMIT OF 262144 WORDS.'
           RETURN
         ENDIF
       ENDIF
       M       = J + 24
C
C      COMPUTE THE NUMBER OF 64 BIT INTEGER CRAY WORDS NEEDED FOR
C      PACKED DATA.
C
       IF (MOD(M,4).NE.0) THEN
         IWORD = (M + 3) / 4
       ELSE
         IWORD = M / 4
       ENDIF
C
       IBYTES = M + M
C
C      STORE NUMBER OF BYTES IN RECORD IN BITS 63-48 OF WORD 5.
C      BITS ARE NUMBERED LEFT TO RIGHT 63 T0 00
C
       TPACK(5) = ISHFT(IBYTES,48_8)
C
C FIND MAX, MIN OF DATA, COMPUTE A AND N.
C
         RMAX = REAL8(1)
         RMIN = RMAX
         DO 20 I = 2,J
           RMAX = AMAX1(RMAX,REAL8(I))
           RMIN = AMIN1(RMIN,REAL8(I))
 20      CONTINUE
C
         A = 0.5 * (RMAX + RMIN)
         X = RMAX - A
         IF (RMAX.NE.RMIN) THEN
C           CALL USDCTI(X,B,1,1,ISTAT)
           CALL Q9E3I6(X,B,1,ISTAT)
           IF (ISTAT.NE.0) PRINT *,' W3AI00-USDCTI OVERFLOW ERROR 1'
           N = IAND(ISHFT(IB,-56_8),127_8)
           N = 4 * (N - 64)
           IF (BTEST(IB,55_8)) GO TO  30
           N = N - 1
           IF (BTEST(IB,54_8)) GO TO  30
           N = N - 1
           IF (BTEST(IB,53_8)) GO TO  30
           N = N - 1
 30      CONTINUE
           N = MAX(-127_8,MIN(127_8,N))
         ELSE
C
C        FIELD IS ZERO OR A CONSTANT
C
           N = 0
         ENDIF
C
C     CONVERT AVERAGE VALUE FROM IEEE F.P. TO IBM370 32 BIT
C     STORE IBM370 32 BIT F.P. AVG. VALUE IN BITS 31 - 00 OF WORD 5.
C
C         CALL USSCTI(A,TPACK(5),5,1,ISTAT)
         CALL Q9EI32(A,rtemp(2),1,ISTAT)
           IF (ISTAT.NE.0) PRINT *,' W3AI00-USDCTI OVERFLOW ERROR 2'
         TPACK(5)=IOR(TPACK(5),irtemp)
C
C      STORE SCALING VALUE N IN BITS 47 - 32 OF WORD 6.
C
         TPACK(6)  = IOR(IAND(MASKN,ISHFT(N,32_8)),TPACK(6))
C
C NOW PACK UP THE DATA, AND SCALE IT TO FIT AN INTEGER*2 WORD
C
         TWON = 2.0 ** (15 - N)
         DO 40 I = 1,J
           XX(I) = (REAL8(I) - A) * TWON
           KK(I) = XX(I) + SIGN(0.5,XX(I))
           IF (KK(I).GE.(-32767)) THEN
             KK(I) = MIN(32767_8,KK(I))
           ELSE
             KK(I) = -32767
           ENDIF
             KK(I) = IAND(KK(I),MASK16)
 40      CONTINUE
C
C        SHIFT THE INTEGER*2 DATA TO FIT 4 IN A 64 BIT WORD
C
         LIM  = (J / 4 ) * 4
         IREM = J - LIM
         DO 50 I = 1,LIM,4
           KK(I)   = ISHFT(KK(I),  48_8)
           KK(I+1) = ISHFT(KK(I+1),32_8)
           KK(I+2) = ISHFT(KK(I+2),16_8)
 50      CONTINUE
C
C        SHIFT THE REMAINING 1, 2, OR 3 INTEGER*2 WORDS
C
         IF (IREM.EQ.1) THEN
           KK(LIM+1)   = ISHFT(KK(LIM+1),48_8)
         ENDIF
C
         IF (IREM.EQ.2) THEN
           KK(LIM+1)   = ISHFT(KK(LIM+1),48_8)
           KK(LIM+2)   = ISHFT(KK(LIM+2),32_8)
         ENDIF
C
         IF (IREM.EQ.3) THEN
           KK(LIM+1)   = ISHFT(KK(LIM+1),48_8)
           KK(LIM+2)   = ISHFT(KK(LIM+2),32_8)
           KK(LIM+3)   = ISHFT(KK(LIM+3),16_8)
         ENDIF
C
C        PACK THE DATA BY USE OF IOR FOUR TO A WORD
C
         II = 7
         DO 60 I = 1,LIM,4
           PACK(II) = IOR(IOR(IOR(KK(I),KK(I+1)),KK(I+2)),KK(I+3))
           II       = II + 1
 60      CONTINUE
C
C        PACK THE LAST 1, 2, OR 3 INTEGER*2 WORDS
C
         IF (IREM.EQ.1) THEN
           PACK(IWORD) = KK(LIM+1)
         ENDIF
C
         IF (IREM.EQ.2) THEN
           PACK(IWORD) = IOR(KK(I),KK(I+1))
         ENDIF
C
         IF (IREM.EQ.3) THEN
           PACK(IWORD) = IOR(IOR(KK(I),KK(I+1)),KK(I+2))
         ENDIF
C
C      MOVE LABEL FROM TEMPORARY ARRAY TO PACK
C
       DO 70 I = 1,6
         PACK(I) = TPACK(I)
 70    CONTINUE
C
C      COMPUTE CHECKSUM AND STORE
C
         IXOR = 0
C
C      COMPUTES A 64 BIT CHECKSUM 1ST
C
         DO 80 I = 1,IWORD
           IXOR = IEOR(IXOR,PACK(I))
 80      CONTINUE
C
C      COMPUTES A 32 BIT CHECKSUM 2ND
C
         IXOR = IEOR(ISHFT(IXOR,-32_8),IAND(IXOR,MASK32))
C
C      COMPUTES A 16 BIT CHECKSUM 3RD
C
         IXOR = IEOR(ISHFT(IXOR,-16_8),IAND(IXOR,MASK16))
C
C      STORE 16 BIT CHECK SUM OF RECORD IN BITS 47-32 OF WORD 5.
C
       PACK(5) = IOR(ISHFT(IXOR,32_8),PACK(5))
C
       RETURN
       END


C> Convert IEEE 32 bit task 754 floating point numbers
C> to IBM370 32 bit floating point numbers.
C>
C> Program history log:
C> - Ralph Jones 1990-06-04 Convert to sun fortran 1.3.
C> - Ralph Jones 1990-07-14 Change ishft to lshift or lrshft.
C> - Ralph Jones 1991-03-28 Change to silicongraphics 3.3 fortran 77.
C> - Ralph Jones 1992-07-20 Change to ibm aix xl fortran.
C> - Ralph Jones 1995-11-15 Add save statement.
C> - Stepen Gilbert 1998-11-18 Specified 4-byte Integer values.
C>
C> @param[in] A - Real*4 array of IEEE 32 bit floating point numbers.
C> @param[in] N - Number of words to convert to IBM370 32 bit F.P.
C> @param[out] B - Real*4 array of IBM370 32 bit floating point numbers.
C> @param[out] ISTAT:
C> - 0: All numbers converted.
C> - -1: N is less than one.
C> - +K: K infinity or nan numbers were found.
C>
C> @note See IEEE task 754 standard floating point arithmetic for
C> more information about IEEE F.P.
C>
C> @author Ralph Jones @date 1990-06-04
      SUBROUTINE Q9EI32(A,B,N,ISTAT)
C
       INTEGER(4)      A(*)
       INTEGER(4)      B(*)
       INTEGER(4)      SIGN,MASKFR,IBIT8,MASKSN,ITEMP,IBMEXP,IBX7
       INTEGER(4)      ISIGN
C
       SAVE
C
       DATA  MASKFR/Z'00FFFFFF'/
       DATA  IBIT8 /Z'00800000'/
       DATA  MASKSN/Z'7FFFFFFF'/
       DATA  SIGN  /Z'80000000'/
C
           IF (N.LT.1) THEN
             ISTAT = -1
             RETURN
           ENDIF
C
           ISTAT = 0
C
         DO 30 I = 1,N
C
C          SIGN BIT OFF
C
           ISIGN = 0
           ITEMP = A(I)
C
C          TEST SIGN BIT
C
           IF (ITEMP.EQ.0) GO TO 20
C
           IF (ITEMP.LT.0)  THEN
C
C            SIGN BIT ON
C
             ISIGN = SIGN
C
C            TURN SIGN BIT OFF
C
             ITEMP = IAND(ITEMP,MASKSN)
C
           END IF
C
           IBMEXP = ISHFT(ITEMP,-23_4)
C
C          TEST FOR INDIFINITE OR NAN NUMBER
C
           IF (IBMEXP.EQ.255) GO TO 10
C
C          TEST FOR ZERO EXPONENT AND FRACTION (UNDERFLOW)
C
           IF (IBMEXP.EQ.0) GO TO 20
           IBMEXP = IBMEXP + 133
           IBX7   = IAND(3_4,IBMEXP)
           IBMEXP = IEOR(IBMEXP,IBX7)
           IBX7   = IEOR(3_4,IBX7)
           ITEMP  = IOR(ITEMP,IBIT8)
           ITEMP  = IOR(ISHFT(IBMEXP,22_4),ISHFT(IAND(ITEMP,MASKFR),
     &     -IBX7))
           B(I)   = IOR(ITEMP,ISIGN)
           GO TO  30
C
 10      CONTINUE
C
C          ADD 1 TO ISTAT FOR INDEFINITE OR NAN NUMBER
C
           ISTAT = ISTAT + 1
C
 20      CONTINUE
           B(I) = 0
C
 30      CONTINUE
C
         RETURN
       END

C> Convert ieee 32 bit task 754 floating point numbers
C> to ibm370 64 bit floating point numbers.
C>
C> Program history log:
C> - Ralph Jones 1992-08-02
C> - Ralph Jones 1995-11-15 Add save statement.
C>
C> @param[in] A Real*4 array of IEEE 32 bit floating point numbers.
C> @param[in] N Number of words to convert to IBM370 64 bit F.P.
C> @param[out] B Real*8 array of IBM370 64 bit floating point numbers.
C> @param[out] ISTAT
C> - 0 All numbers converted.
C> - -1 N is less than one.
C> - +K K infinity or nan numbers were found.
C>
C> @note See IEEE task 754 standard floating point arithmetic for
C> more information about IEEE F.P.
C>
C> @author Ralph Jones @date 1992-08-02
      SUBROUTINE Q9E3I6(A,B,N,ISTAT)

C
       INTEGER(4)      A(N)
       INTEGER(4)      B(2,N)
       INTEGER(4)      SIGN,MASKFR,IBIT8,MASKSN,ITEMP,IEEEXP
       INTEGER(4)      IBMEXP,IBX7,JTEMP,ISIGN
C
       SAVE
C
       DATA  MASKFR/Z'00FFFFFF'/
       DATA  IBIT8 /Z'00800000'/
       DATA  MASKSN/Z'7FFFFFFF'/
       DATA  SIGN  /Z'80000000'/
C
           IF (N.LT.1) THEN
             ISTAT = -1
             RETURN
           ENDIF
C
           ISTAT = 0
C
         DO 30 I = 1,N
           ISIGN = 0
           ITEMP = A(I)
C
C          TEST SIGN BIT
C
           IF (ITEMP.EQ.0) GO TO 20
C
C          TEST FOR NEGATIVE NUMBERS
C
           IF (ITEMP.LT.0) THEN
C
C            SIGN BIT ON
C
             ISIGN = SIGN
C
C            TURN SIGN BIT OFF
C
             ITEMP = IAND(ITEMP,MASKSN)
C
           END IF
C
C          GET IEEE EXPONENT
C
           IEEEXP = ISHFT(ITEMP,-23_4)
C
C          TEST FOR INDIFINITE OR NAN NUMBER
C
           IF (IEEEXP.EQ.255) GO TO 10
C
C          TEST FOR ZERO EXPONENT AND FRACTION (UNDERFLOW)
C          CONVERT IEEE EXPONENT (BASE 2) TO IBM EXPONENT
C          (BASE 16)
C
           IF (IEEEXP.EQ.0)   GO TO 20
           IBMEXP = IEEEXP + 133
           IBX7   = IAND(3_4,IBMEXP)
           IBMEXP = IEOR(IBMEXP,IBX7)
           IBX7   = IEOR(3_4,IBX7)
           ITEMP  = IOR(ITEMP,IBIT8)
           JTEMP  = IOR(ISHFT(IBMEXP,22_4),ISHFT(IAND(ITEMP,MASKFR),
     &     -IBX7))
           B(1,I) = IOR(JTEMP,ISIGN)
           B(2,I) = 0
           IF (IBX7.GT.0) B(2,I) = ISHFT(ITEMP,32_4-IBX7)
           GO TO  30
C
 10      CONTINUE
C          ADD 1 TO ISTAT FOR INDEFINITE OR NAN NUMBER
C
           ISTAT = ISTAT + 1
C
 20      CONTINUE
           B(1,I) = 0
           B(2,I) = 0
C
 30      CONTINUE
C
         RETURN
       END
