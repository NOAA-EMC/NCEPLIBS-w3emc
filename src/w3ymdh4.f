C> @file
C> @brief 4-byte date word unpacker and packer.
C> @author K. F. Brill @date 1998-07-29

C> Obtains the components of the nmc date word (ncep y2k
C> compliant form), or given its components, forms an nmc type date
C> word. The packing is done using base 32.
C>
C> If the first byte of IDATE is less than 101, then the old
C> Office Note 84 packing is assumed. A four-digit year is
C> always returned. To pack the "old" way, pass in a 2-digit
C> year.
C>
C> This program will work for the years ranging from A.D. 101
C> through 79359.
C>
C> On unpacking, years less than or equal to 100 are returned
C> as follows:
C>
C> - 0-50   2000--2050
C> - 51-100 1951--2000
C>
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1998-07-29 | K. F. Brill | Initial.
C> 1999-03-15 | Gilbert | Removed Call to W3FS11() and put its processing inline.
C> W3FS11 was deleted from the W3LIB.
C>
C> @param[inout] IDATE Left 4 bytes of integer 64 bit word, or can be
C> IDATE(4) or CHARACTER*4 IDATE.
C> @param[inout] IYEAR Year (4 digits or 2 digits for on84)
C> @param[inout] MONTH Month
C> @param[inout] IDAY Day
C> @param[inout] IHOUR Hour
C> @param[in] NN Code:
C>  - .eq. 0 pack iyear, month, iday, ihour into idate
C>  - .ne. 0 unpack idate into iyear, month, iday, ihour
C>
C> @author K. F. Brill @date 1998-07-29
      SUBROUTINE W3YMDH4 (IDATE,IYEAR,MONTH,IDAY,IHOUR,NN)
C
      CHARACTER IDATE(4)
C
      IF (NN.NE.0) THEN
C
	ITEMP = MOVA2I(IDATE(1))
	IF ( ITEMP .lt. 101 ) THEN
            IYEAR  = MOVA2I(IDATE(1))
            MONTH  = MOVA2I(IDATE(2))
            IDAY   = MOVA2I(IDATE(3))
            IHOUR  = MOVA2I(IDATE(4))
	    IF(IYEAR.LE.100) IYEAR=2050-MOD(2050-IYEAR,100)
	    RETURN
	END IF
	ITEMP = ITEMP - 101
	ITEMP = ITEMP * 256 + MOVA2I(IDATE(2))
	ITEMP = ITEMP * 256 + MOVA2I(IDATE(3))
	ITEMP = ITEMP * 256 + MOVA2I(IDATE(4))
	IHOUR = MOD ( ITEMP, 32 )
	ITEMP = ITEMP / 32
	IDAY  = MOD ( ITEMP, 32 )
	ITEMP = ITEMP / 32
	MONTH = MOD ( ITEMP, 32 )
	IYEAR = ITEMP / 32
C
      ELSE
C
	ITEMP = IYEAR
	IF ( ITEMP .lt. 101 ) THEN
            IDATE(1) = CHAR(IYEAR)
            IDATE(2) = CHAR(MONTH)
            IDATE(3) = CHAR(IDAY)
            IDATE(4) = CHAR(IHOUR)
	    RETURN
	END IF
	ITEMP = ITEMP * 32 + MONTH
	ITEMP = ITEMP * 32 + IDAY
	ITEMP = ITEMP * 32 + IHOUR
C*
	IDATE(4)=CHAR(MOD(ITEMP,256))
	ITEMP = ITEMP / 256
	IDATE(3)=CHAR(MOD(ITEMP,256))
	ITEMP = ITEMP / 256
	IDATE(2)=CHAR(MOD(ITEMP,256))
	ITEMP = ITEMP / 256
	ITEMP = ITEMP + 101
	IDATE(1)=CHAR(ITEMP)
C
      ENDIF
C
      RETURN
      END
