C> @file
C> @brief EBCDIC to ASCII
C> @author Armand Desmarais @date 1982-11-29

C> Convert EBCDIC to ASCII by character.
C> This subroutine can be replaced by cray utility subroutine
C> uscctc. See manual sr-2079 page 3-15. cray utility tr
C> can also be used for ASCII, EBCDIC conversion. See manual sr-2079
C> page 9-35.
C>
C> Program history log:
C> - Armand Desmarais 1982-11-29
C> - Ralph Jones 1988-03-31 Change logic so it works like a
C> ibm370 translate instruction.
C> - Ralph Jones 1988-08-22 Changes for microsoft fortran 4.10.
C> - Ralph Jones 1988-09-04 Change tables to 128 character set.
C> - Ralph Jones 1990-01-31 Convert to cray cft77 fortran
C> cray does not allow char*1 to be set to hex.
C> - Stephen Gilbert 98-12-21 Replaced Function ICHAR with mova2i.
C>
C> @param[inout] IE (in) Character*1 array of EBCDIC data (out) ASCII data
C> @param[in] NC Integer, contains character count to convert.
C>
C> @note Software version of ibm370 translate instruction, by
C> changing the two tables we could do a 64, 96, 128 ASCII
C> character set, change lower case to upper, etc.
C> aea converts data at a rate of 1.5 million characters per sec.
C> cray utility usccti convert ASCII to IBM EBCDIC
C> cray utility uscctc convert IBM EBCDIC to ASCII
C> they convert data at a rate of 2.1 million characters per sec.
C> cray utility tr will also do a ASCII, EBCDIC conversion.
C> tr convert data at a rate of 5.4 million characters per sec.
C> tr is in library /usr/lib/libcos.a add to segldr card.
C>
C> @author Armand Desmarais @date 1982-11-29
      SUBROUTINE W3AI38 (IE, NC )
C
      INTEGER(8)      IASCII(32)
C
      CHARACTER*1  IE(*)
      CHARACTER*1  ASCII(0:255)
C
      EQUIVALENCE  (IASCII(1),ASCII(0))
C
C***   ASCII  CONTAINS ASCII CHARACTERS, AS PUNCHED ON IBM029
C
       DATA  IASCII/
     & Z'000102030009007F',Z'0000000B0C0D0E0F',
     & Z'1011120000000000',Z'1819000000000000',
     & Z'00001C000A001700',Z'0000000000050607',
     & Z'00001600001E0004',Z'000000001415001A',
     & Z'2000600000000000',Z'0000602E3C282B00',
     & Z'2600000000000000',Z'000021242A293B5E',
     & Z'2D2F000000000000',Z'00007C2C255F3E3F',
     & Z'0000000000000000',Z'00603A2340273D22',
     & Z'2061626364656667',Z'6869202020202020',
     & Z'206A6B6C6D6E6F70',Z'7172202020202020',
     & Z'207E737475767778',Z'797A2020205B2020',
     & Z'0000000000000000',Z'00000000005D0000',
     & Z'7B41424344454647',Z'4849202020202020',
     & Z'7D4A4B4C4D4E4F50',Z'5152202020202020',
     & Z'5C20535455565758',Z'595A202020202020',
     & Z'3031323334353637',Z'3839202020202020'/
C
      IF (NC .LE. 0)   RETURN
C
C***  CONVERT STRING ...  EBCDIC TO ASCII,   NC CHARACTERS
C
        DO  20  J = 1, NC
          IE(J) = ASCII(mova2i(IE(J)))
 20     CONTINUE
C
      RETURN
      END
