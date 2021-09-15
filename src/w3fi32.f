C> @file
C> @brief Pack id's into office note 84 format.
C> @author Alan Nierow @date 1986-02-07

C> Converts an array of the 27 data field identifiers into
C> an array of the first 8 identification words of the format de-
C> scribed in NMC office note 84 (89-06-15, page-35). On a cray
C> they will fit into four 64 bit integer words.
C>
C> Program history log:
C> - Alan Nierow 1986-02-07
C> - Ralph Jones 1989-10-24 Convert to cray cft77 fortran.
C> - Ralph Jones 1991-03-19 Changes for big records.
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive.
C> - Stephen Gilbert 1999-03-15 Specified 8-byte integer array explicitly.
C>
C> @param[in] LARRAY Integer array containing 27 data field
C> identifiers (see o.n. 84)
C> @param[out] KIDNT Integer array of 6 words, 12 office note 84 32 bit
C> words, first 4 words are made by w3fi32(), if you are
C> using packer w3ai00(), it will compute word 5 and 6.
C> (office note 84 words 9,10, 11 and 12). If J the
C> word count in word 27 of LARRAY is greater than
C> 32743 then bits 15-0 of the 4th ID word are set to
C> zero, J is stored in bits 31-0 of the 6th ID word.
C> ID word 5 is set zero, bit 63-32 of the 6th ID
C> word are set zero.
C> @note bis are number left to right on the cray as 63-0.
C>
C> @note Exit states printed messages:
C> If any number n in (LARRAY(i),i=1,27) is erroneously large:
C> 'value in LARRAY(i)=n is too large to pack'
C> if any number n in (LARRAY(i),i=1,27) is erroneously negative:
C> 'value in LARRAY(i)=n should not be negative'
C> in either of the above situations, that portion of the packed
C> word corresponding to LARRAY(i) will be set to binary ones.
C>
C> @author Alan Nierow @date 1986-02-07
      SUBROUTINE W3FI32(LARRAY,KIDNT)
C
       INTEGER(8)        LARRAY(27)
       INTEGER(8)        ITABLE(27)
       INTEGER(8)        KIDNT(*)
       INTEGER(8)        KX,MASK,MASK16,ISC,ITEMP8
C
       SAVE
C
       DATA  ITABLE/Z'0000000000340C01',Z'0000000000280C01',
     &              Z'0000000000200801',Z'00000000001C0401',
     &              Z'0000000001081401',Z'0000000001000801',
     &              Z'00000000003C0402',Z'0000000000340802',
     &              Z'0000000000280C02',Z'0000000000200802',
     &              Z'00000000001C0402',Z'0000000001081402',
     &              Z'0000000001000802',Z'0000000000380803',
     &              Z'0000000000300803',Z'0000000000280803',
     &              Z'0000000000200803',Z'00000000001C0403',
     &              Z'0000000000100C03',Z'0000000000001003',
     &              Z'0000000000380804',Z'0000000000300804',
     &              Z'0000000000280804',Z'0000000000200804',
     &              Z'0000000000180804',Z'0000000000100804',
     &              Z'0000000000001004'/
       DATA  KX    /Z'00000000FFFFFFFF'/
       DATA  MASK  /Z'00000000000000FF'/
       DATA  MASK16/Z'FFFFFFFFFFFF0000'/
C
C     MAKE KIDNT = 0
C
      DO 10 I = 1,4
        KIDNT(I) = 0
 10   CONTINUE
C
      ISIGN = 0
C
      DO 90 I = 1,27
        ISC = ITABLE(I)
        I1  = IAND(ISC,MASK)
        I2  = IAND(ISHFT(ISC,-8_8), MASK)
        I3  = IAND(ISHFT(ISC,-16_8),MASK)
        I4  = IAND(ISHFT(ISC,-24_8),MASK)
C
C     SIGN TEST
C
       IV = LARRAY(I)
       IF (IV.GE.0) GO TO 50
       IF (I4.NE.0) GO TO 30
       WRITE (6,20) I, IV
 20    FORMAT(/,1X,' W3FI32 - VALUE IN LARRAY(',I2,') =',I11,
     & ' SHOULD NOT BE NEGATIVE',/)
       GO TO 70
C
 30   CONTINUE
        IV    = IABS(IV)
        MSIGN = 1
        ISIGN = MSIGN
        K     = I2 / 4
C
      DO 40 M = 1,K
        ISIGN = ISHFT(ISIGN,4)
 40   CONTINUE
C
        ISIGN = ISHFT(ISIGN,-1)
        IV    = IOR(IV,ISIGN)
C
 50   CONTINUE
C
C     MAG TEST
C
        IF (ISHFT(IV,-I2).EQ.0) GO TO 80
        IF (LARRAY(27).GT.32743) GO TO 70
        PRINT 60, I , IV
 60     FORMAT(/,1X,' W3FI32 - VALUE IN LARRAY(',I2,') =',I11,
     &  ' IS TOO LARGE TO PACK',/)
C
 70   CONTINUE
        IV = KX
        IA = 32 - I2
        IV = ISHFT(IV,-IA)
C
C     SHIFT
C
 80   CONTINUE
        ITEMP=ISHFT(IV,I3)
        ITEMP8=ITEMP
        KIDNT(I1) = IOR(KIDNT(I1),ITEMP8)
C
 90   CONTINUE
C
C     TEST FOR BIG RECORDS, STORE J THE WORD COUNT IN THE 6TH
C     ID WORD IF GREATER THAN 32743.
C
      IF (LARRAY(27).EQ.0) THEN
        PRINT *,' W3FI32 - ERROR, WORD COUNT J = 0'
      ELSE IF (LARRAY(27).GT.32743) THEN
        KIDNT(4) = IAND(KIDNT(4),MASK16)
        KIDNT(5) = 0
        KIDNT(6) = LARRAY(27)
      END IF
C
      RETURN
      END
