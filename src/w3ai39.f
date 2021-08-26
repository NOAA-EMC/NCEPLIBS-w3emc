C> @file
C> @brief Translate 'ASCII' field to 'EBCDIC'.
C> @author Armand Desmarais @date 1993-10-06

C> translate an 'ASCII' field to 'EBCDIC', all alphanumerics,
C> special charcaters, fill scatter, brocken< clear, overcast, bell,
C> ht and vt (for AFOS). space, '6D' to '5E' conversion (hdrology),
C> changers were made to W3AI38 to give reverse table translation
C>
C> Program history log:
C> - Ralph Jones 1993-10-06 Convert ibm370 assebler version to fortran.
C> - Ralph Jones 1994-04-28 Changes for cray.
C> - Stephen Gilbert 1998-12-21 Replaced Function ICHAR with mova2i.
C>
C> @param[inout] NFLD Character*1 array of (in) ASCII data (out) EBCDIC data.
C> @param[in] N Integer, contains character count to convert.
C>
C> @note Software version of IBM370 translate instruction, by
C> changing the table we could do a 64, 96, ASCII
C> character set, change lower case to upper, etc.
C> tr convert data at a rate of 5.4 million characters per sec.
C> tr is in library /usr/lib/libcos.a add to segldr card.
C>
C> @author Armand Desmarais @date 1993-10-06
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
