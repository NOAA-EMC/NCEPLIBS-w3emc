C> @file
C> @brief Constant size binary string unpacker.
C> @author Robert Allard @date 1980-04-01

C> Unpack consecutive binary strings of the same size from
C> one user supplied array and store them in the same order right
C> aligned in another array. W3AI41() is the reverse of W3AI40().
C>
C> Program history log:
C> - Robert Allard 1980-04-01  R.ALLARD (ORIGINAL AUTHOR)  ASMEMBLER LANGUAGE VERSION.
C> - Ralph Jones 1984-07-05 Recompiled for NAS-9050
C> - Ralph Jones 1988-07-05 Wrote fortran version of w3ai41 to unpack
C> variable size binary strings, added code to reverse orfer of bytes.
C> - Ralph Jones 1989-11-04 Convert to craf CFT77 FORTRAN
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive.
C>
C> @param[in] KFLD Integer array contining binary string(s).
C> @param[in] KLEN Integer number of bits per string (0 < klen < 65).
C> @param[in] KNUM Integer number of strings to unpack. this value must
C> not exceed the dimension of 'kout'.
C> @param[in] KOFF Integer number specifying the bit offset of the
C> first string 'kfld'. the offset value is reset to
C> include the low order bit of the last string unpacked
C> ('koff' > 0 ).
C> @param[out] KOUT Integer*4 array holding unpacked string(s).
C>
C> Exit states:
C> error - 'koff' < 0 if 'klen' has an illegal value or 'knum' < 1
C> then 'kout' has no strings stored.
C>
C> @note This subroutine should be written in assembler language.
C> The fortran version runs two or three times slower than the asembler
C> version. The fortran version can be converted to run on other
C> computers with a few changes. The bit manipulation functions are the
C> same in IBM370 vs fortran 4.1, microsoft fortran 4.10, vax fortran.
C> Most modern fortran compiler have and, or, shift functions. If you
C> are running on a pc, vax and your input was made on a IBM370, apollo
C> sun, h.p.. etc. you may have to add more code to reverse the order o
C> bytes in an integer word. NCAR gbytes() can be used instead of this
C> subroutine.
C>
C> @author Robert Allard @date 1980-04-01
      SUBROUTINE W3AI41(KFLD,KOUT,KLEN,KNUM,KOFF)
C
      INTEGER  KFLD(*)
      INTEGER  KOUT(*)
      INTEGER  BITSET
      INTEGER  OFFSET
      INTEGER  WRDSET
C
      OFFSET = KOFF
      IF (OFFSET.LT.0) RETURN
      IF (KLEN.GT.64.OR.KLEN.LT.1) THEN
        KOFF = -1
        RETURN
      ENDIF
C
      IF (KNUM.LT.1) THEN
        KOFF = -1
        RETURN
      ENDIF
C
        JCOUNT  = KLEN - 64
        LENGTH  = KLEN
C
      DO 100  I = 1,KNUM
        WRDSET  = ISHFT(OFFSET,-6)
        BITSET  = MOD(OFFSET,64)
        ITEMP   = KFLD(WRDSET+1)
        NTEMP   = KFLD(WRDSET+2)
        ITEMP   = ISHFT(ITEMP,BITSET)
        NTEMP   = ISHFT(NTEMP,BITSET-64)
        KOUT(I) = ISHFT(IOR(ITEMP,NTEMP),JCOUNT)
        OFFSET  = OFFSET + LENGTH
 100  CONTINUE
        KOFF = OFFSET
      RETURN
      END
