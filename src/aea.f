C> @file
C> @brief This subroutine converts ascii to ebcdic, or ebcdic to ascii
C> @author desmarais @date 11-29-1982

C>   Program history log:
C>   - 11-29-1982  Desmarais
C>   - 03-31-1988  R. E. Jones
C>      - change logic so it works like a ibm370 translate instruction.
C>   - 08-22-1988  R. E. Jones
C>      - changes for microsoft fortran 4.10
C>   - 09-04-1988  R. E. Jones
C>      - change tables to 128 character set
C>   - 01-31-1990  R. E. Jones
C>      - convert to cray cft77 fortran cray does not allow char*1 to be set to hex
C>   - 12-21-1998  Stephen Gilbert
C>      - replaced function ichar with mova2i.
C>
C>   @param[in, out] IA character*1 array of ascii data  if nc < 0
C>   @param[in, out] IE character*1 array of ebcdic data if nc > 0
C>   @param[in] NC integer, contains character count to convert.
C>     - if nc .lt. 0,  convert ascii to ebcdic
C>     - if nc .gt. 0,  convert ebcdic to ascii
C>
C>   @note This subroutine can be replaced by cray utility subroutines
C>   uscctc and uscctt. See manual sr-2079 page 3-15. Cray utility tr
C>   can also be used for ascii, ebcdic conversion. See manual sr-2079
C>   page 9-35.
C>   @note Software version of ibm370 translate instruction, by
C>   changing the two tables we could do a 64, 96, 128 ascii
C>   character set, change lower case to upper, etc.
C>   - aea() converts data at a rate of 1.5 million characters per sec.
C>   - cray utility usccti convert ibm ebcdic to ascii
C>   - cray utility uscctc convert ascii to ibm ebcdic
C>   - they convert data at a rate of 2.1 million characters per sec.
C>   - cray utility tr will also do a ascii, ebcdic conversion.
C>     tr convert data at a rate of 5.4 million characters per sec.
C>     tr is in library  /usr/lib/libcos.a   add to segldr card.
C>
C>   @author desmarais @date 11-29-1982
        SUBROUTINE AEA (IA, IE, NC )
C***   ASCII  CONTAINS ASCII CHARACTERS, AS PUNCHED ON IBM029
C
      INTEGER(8)      IASCII(32)
      INTEGER(8)      IEBCDC(32)
C
      CHARACTER*1  IA(*)
      CHARACTER*1  IE(*)
      CHARACTER*1  ASCII(0:255)
      CHARACTER*1  EBCDIC(0:255)
C
      EQUIVALENCE  (IASCII(1),ASCII(0))
      EQUIVALENCE  (IEBCDC(1),EBCDIC(0))
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
C***  EBCDIC CONTAINS HEX. REPRESENTATION OF EBCDIC CHARACTERS
C
       DATA  IEBCDC/
     & Z'00010203372D2E2F',Z'1605250B0C0D0E0F',
     & Z'101112003C3D3226',Z'18193F2722003500',
     & Z'405A7F7B5B6C507D',Z'4D5D5C4E6B604B61',
     & Z'F0F1F2F3F4F5F6F7',Z'F8F97A5E4C7E6E6F',
     & Z'7CC1C2C3C4C5C6C7',Z'C8C9D1D2D3D4D5D6',
     & Z'D7D8D9E2E3E4E5E6',Z'E7E8E9ADE0BD5F6D',
     & Z'7981828384858687',Z'8889919293949596',
     & Z'979899A2A3A4A5A6',Z'A7A8A9C06AD0A107',
     & 16*Z'4040404040404040'/
C
      NUM = IABS(NC)
C
      IF (NC .EQ. 0)   RETURN
C
      IF (NC .GT. 0)   THEN
C
C***  CONVERT STRING ...  EBCDIC TO ASCII,   NUM CHARACTERS
C
        DO  10  J = 1, NUM
          IA(J) = ASCII(mova2i(IE(J)))
 10     CONTINUE
C
      ELSE
C
C***  CONVERT STRING ...  ASCII TO EBCDIC,   NUM CHARACTERS
C
        DO  20  J = 1, NUM
          IE(J) = EBCDIC(mova2i(IA(J)))
 20     CONTINUE
      END IF
C
      RETURN
      END
