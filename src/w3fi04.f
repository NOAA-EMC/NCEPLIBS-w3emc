C> @file
C> @brief Find word size, endian, character set.
C> @author Ralph Jones @date 1994-10-07

C> Subroutine computes word size, the type of character
C> set, ASCII or EBCDIC, and if the computer is big-endian, or
C> little-endian.
C>
C> Program history log:
C> - Relph Jones 1994-10-07
C> - Stephen Gilbert 1998-07-08 Removed the Fortran SAVE Statement.
C> The SAVE statement is not needed for this outine, and may have been
C> causing errors using the f90 compiler under the 2.0 Programming Environment.
C> - Boi Vuong 2002-10-15 Replaced Function ICHAR with mova2i
C>
C> @param[out] IENDN Integer for big-endian or little-endian
C> - =0 big-endian
C> - =1 little-endian
C> - =2 cannot compute
C> @param[out] ITYPEC Integer for type of character set
C> - =0 ASCII character set
C> - =1 EBCDIC character set
C> - =2 not ASCII or EBCDIC
C> @param[out] LW Integer for words size of computer in bytes
C> - =4 for 32 bit computers
C> - =8 for 64 bit computers
C>
C> @author Ralph Jones @date 1994-10-07
      SUBROUTINE W3FI04(IENDN,ITYPEC,LW)
C
      INTEGER       ITEST1
      INTEGER       ITEST2
      INTEGER       ITEST3
      INTEGER       IENDN
      INTEGER       ITYPEC
      INTEGER       LW
C
      CHARACTER * 8 CTEST1
      CHARACTER * 8 CTEST2
      CHARACTER * 1 CTEST3(8)
      CHARACTER * 1 BLANK
C
      EQUIVALENCE   (CTEST1,ITEST1),(CTEST2,ITEST2)
C
      EQUIVALENCE   (ITEST3,CTEST3(1))
C
      DATA  CTEST1/'12345678'/
      DATA  ITEST3/Z'01020304'/
      DATA  BLANK /' '/
C
C     SAVE
C
C     TEST FOR TYPE OF CHARACTER SET
C     BLANK IS 32 (20 HEX) IN ASCII, 64 (40 HEX) IN EBCDEC
C
      IF (MOVA2I(BLANK).EQ.32) THEN
        ITYPEC = 0
      ELSE IF (MOVA2I(BLANK).EQ.64) THEN
C
C     COMPUTER IS PROBABLY AN IBM360, 370, OR 390 WITH
C     A 32 BIT WORD SIZE, AND BIG-ENDIAN.
C
        ITYPEC = 1
      ELSE
        ITYPEC = 2
      END IF
C
C     TEST FOR WORD SIZE, SET LW TO 4 FOR 32 BIT COMPUTER,
C     8 FOR FOR 64 BIT COMPUTERS
C
      ITEST2 = ITEST1
      IF (CTEST1 .EQ. CTEST2) THEN
C
C     COMPUTER MAY BE A CRAY, OR COULD BE DEC VAX ALPHA
C     OR SGI WITH R4000, R4400, R8800 AFTER THEY CHANGE
C     FORTRAN COMPILERS FOR 64 BIT INTEGER.
C
        LW = 8
      ELSE
        LW = 4
      ENDIF
C
C     USING ITEST3 WITH Z'01020304' EQUIVALNCED TO CTEST3
C     ON A 32 BIT BIG-ENDIAN COMPUTER 03 IS IN THE 3RD
C     BYTE OF A 4 BYTE WORD. ON A 32 BIT LITTLE-ENDIAN
C     COMPUTER IT IS IN 2ND BYTE.
C     ON A 64 BIT COMPUTER Z'01020304' IS RIGHT ADJUSTED IN
C     A 64 BIT WORD, 03 IS IN THE 7TH BYTE.  ON A LITTLE-
C     ENDIAN 64 BIT COMPUTER IT IS IN THE 2ND BYTE.
C
      IF (LW.EQ.4) THEN
        IF (MOVA2I(CTEST3(3)).EQ.3) THEN
          IENDN = 0
        ELSE IF (MOVA2I(CTEST3(3)).EQ.2) THEN
          IENDN = 1
        ELSE
          IENDN = 2
        END IF
      ELSE IF (LW.EQ.8) THEN
        IF (MOVA2I(CTEST3(7)).EQ.3) THEN
          IENDN = 0
        ELSE IF (MOVA2I(CTEST3(2)).EQ.3) THEN
          IENDN = 1
        ELSE
          IENDN = 2
        END IF
      ELSE
        IENDN = 2
      END IF
C
      RETURN
      END
