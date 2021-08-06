C> @file
C                .      .    .                                       .
C> SUBPROGRAM:    W3AI39      TRANSLATE 'ASCII' FIELD TO 'EBCDIC'
C>   PRGMMR: DESMARAIS        ORG: W342       DATE: 93-10-06
C>
C> ABSTRACT: TRANSLATE AN 'ASCII' FIELD TO 'EBCDIC', ALL ALPHANUMERICS,
C>   SPECIAL CHARCATERS, FILL SCATTER, BROCKEN< CLEAR, OVERCAST, BELL,
C>   HT AND VT (FOR AFOS). SPACE, '6D' TO '5E' CONVERSION (HDROLOGY),
C>   CHANGERS WERE MADE TO W3AI38 TO GIVE REVERSE TABLE TRANSLATION
C>
C> PROGRAM HISTORY LOG:
C>   93-10-06  R.E.JONES   CONVERT IBM370 ASSEBLER VERSION TO FORTRAN
C>   94-04-28  R.E.JONES   CHANGES FOR CRAY
C>   98-12-21  Gilbert    Replaced Function ICHAR with mova2i.
C>
C> USAGE:    CALL W3AI39 (NFLD,N)
C>   INPUT ARGUMENT LIST:
C>     NFLD    - CHARACTER*1 ARRAY OF ASCII DATA
C>     N       - INTEGER,  CONTAINS CHARACTER COUNT TO CONVERT....
C>
C>   OUTPUT ARGUMENT LIST:
C>     NFLD     - CHARACTER*1 ARRAY OF EBCDIC DATA
C>
C> REMARKS: SOFTWARE VERSION OF IBM370 TRANSLATE INSTRUCTION, BY
C>   CHANGING THE TABLE WE COULD DO A  64, 96,  ASCII
C>   CHARACTER SET, CHANGE LOWER CASE TO UPPER, ETC.
C>   TR CONVERT DATA AT A RATE OF 5.4 MILLION CHARACTERS PER SEC.
C>   TR IS IN LIBRARY  /USR/LIB/LIBCOS.A   ADD TO SEGLDR CARD.
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY Y-MP8/864
C>
      SUBROUTINE W3AI39 (NFLD, N)
C
      INTEGER(8)      IEBCDC(32)
C
      CHARACTER*1  NFLD(*)
      CHARACTER*1  EBCDIC(0:255)
C
      SAVE
C
      EQUIVALENCE  (IEBCDC(1),EBCDIC(0))
C
C***  EBCDIC CONTAINS HEX. REPRESENTATION OF EBCDIC CHARACTERS
C
C      DATA  IEBCDC/
C    & X'00010203372D2E2F',X'1605250B0C0D0E0F',
C    & X'101112003C3D3226',X'18193F2722003500',
C    & X'405A7F7B5B6C507D',X'4D5D5C4E6B604B61',
C    & X'F0F1F2F3F4F5F6F7',X'F8F97A5E4C7E6E6F',
C    & X'7CC1C2C3C4C5C6C7',X'C8C9D1D2D3D4D5D6',
C    & X'D7D8D9E2E3E4E5E6',X'E7E8E9ADE0BD5F6D',
C    & X'7981828384858687',X'8889919293949596',
C    & X'979899A2A3A4A5A6',X'A7A8A9C06AD0A107',
C    & 16*X'4040404040404040'/
C
C      THIS TABLE IS THE SAME AS HDS ASSEMBLER VERSION
C
       DATA  IEBCDC/
     & Z'007D006C000000E0',Z'00657C66004C0000',
     & Z'0000000000000000',Z'0000000000005B00',
     & Z'40D07F7B5000506E',Z'4D5D5C4F6B604B61',
     & Z'F0F1F2F3F4F5F6F7',Z'F8F90000007E00C0',
     & Z'64C1C2C3C4C5C6C7',Z'C8C9D1D2D3D4D5D6',
     & Z'D7D8D9E2E3E4E5E6',Z'E7E8E90062636D00',
     & Z'0000000000000000',Z'0000000000000000',
     & Z'0000000000000000',Z'000000000000005F',
     & 16 * Z'0000000000000000'/
C
      IF (N .LE. 0)   RETURN
C
C***  CONVERT STRING ...  ASCII TO EBCDIC,   N CHARACTERS
C
        DO  20  J = 1, N
          NFLD(J) = EBCDIC(mova2i(NFLD(J)))
 20     CONTINUE
C
      RETURN
      END
