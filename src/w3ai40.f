C> @file
C> @brief Constant size binary string packer.
C> @author Robert Allard @date 1980-04-01

C> Packs constant size binary strings into an array. This
C> packing replaces bits in the part of the output array indicated
C> by the offset value. W3AI40 is the reverse of W3AI41. (see W3AI32
C> to pack variable size binary strings.)
C>
C> Program history log:
C> - Robert Allard 1980-04-01 Asmembler language version.
C> - Ralph Jones 1984-07-05 Recompiled for nas-9050.
C> - Ralph Jones 1989-11-04 Wrote fortran version of w3ai40 to pack
C> constant size binary strings.
C> - Ralph Jones 1989-11-05 Convert to cray cft77 fortran.
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive.
C>
C> @param[in] KFLD Integer input array of right adjusted strings.
C> @param[in] KLEN Integer number of bits per string (0 < klen < 33).
C> @param[in] KNUM Integer number of strings in 'kfld' to pack.
C> @param[in] KOFF Integer number specifying the bit offset of the
C> first output string. the offset value is reset to
C> include the low order bit of the last packed string.
C> @param[out] KOUT Integer output array to hold packed string(s).
C>
C> exit states:
C> error  - koff < 0 if klen has an illegal value or knum < 1
C> then kout has no strings stored.
C>
C> @note This subroutine should be written in assembler language.
C> The fortran version runs two or three times slower than the asembler
C> version. The fortran version can be converted to run on other
C> computers with a few changes. The bit manipulation functions are the
C> same in IBM370 vs fortran 4.1, microsoft fortran 4.10, vax fortran.
C> Most modern fortran compiler have and, or, shift functions. If you
C> are running on a pc, vax and your input was made on a IBM370, apollo
C> sun, h.p.. etc. you may have to add more code to reverse the order of
C> bytes in an integer word. NCAR sbytes() can be used instead of this
C> subroutine. Please use NCAR sbytes() subroutine instead of this
C> subroutine.
C>
C> @author Robert Allard @date 1980-04-01
      SUBROUTINE W3AI40(KFLD,KOUT,KLEN,KNUM,KOFF)
C
      INTEGER  KFLD(*)
      INTEGER  KOUT(*)
      INTEGER  BIT
      INTEGER  OFFSET
      INTEGER  WRD
C
      DATA  MASK  /-1/
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
        JCOUNT  = 64 - KLEN
        LENGTH  = KLEN
        MASKWD  = ISHFT(MASK,JCOUNT)
C
      DO 100  I = 1,KNUM
        WRD       = ISHFT(OFFSET,-6) + 1
        BIT       = MOD(OFFSET,64)
        MASK8     = NOT(ISHFT(MASKWD,-BIT))
        OFFSET    = OFFSET + LENGTH
        JTEMP     = IAND(KOUT(WRD),MASK8)
        NCOUNT    = 64 - BIT
        IF (NCOUNT.LT.LENGTH) THEN
          MASK9   = NOT(ISHFT(MASKWD,NCOUNT))
          NTEMP   = IAND(KOUT(WRD+1),MASK9)
        ENDIF
        ITEMP     = ISHFT(ISHFT(KFLD(I),JCOUNT),-BIT)
        KOUT(WRD) = IOR(ITEMP,JTEMP)
        IF (NCOUNT.LT.LENGTH) THEN
          ITEMP       = ISHFT(KFLD(I),(JCOUNT+NCOUNT))
          KOUT(WRD+1) = IOR(ITEMP,NTEMP)
        ENDIF
 100  CONTINUE
        KOFF = OFFSET
      RETURN
      END
