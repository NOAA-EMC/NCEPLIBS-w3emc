C> @file
C> @brief GMT time packer.
C> @author B. Struble @date 1983-12-12

C>
C> @note Convert 32 or 64 bit binary time (GMT) into a 16 bit
C> string and store these 4 packed decimal numbers into bytes
C> 39 and 40 of the output array.
C>
C> Program history log:
C> - B. Struble 1983-12-12
C> - Ralph Jones 1984-07-06 Change to ibm assembler v 02.
C> - Ralph Jones 1995-10-16 Change to fortran for cray and 32 bit workstations.
C>
C> @param[in] ITIME Integer word containing time in binary.
C> @param[out] QDESCR Array containing transmission queue descriptor
C> Time will be placed in 39 and 40th byte of this array as 4 (4 bit) BCD.
C>
C>
C> @note The user can obtain the current time in GMT by invocking
C> the W3 library routine w3fq02 which fills an eight word array
C> with the current date and time. The 5th word from this array
C> contains the time which can be passed to w3aq15 as the
C> input parameter-itime.
C>
C> @author B. Struble @date 1983-12-12
      SUBROUTINE W3AQ15(ITIME, QDESCR)
      INTEGER        ITIME
C
      CHARACTER * 80 QDESCR
C
C BYTES 39-40                  HR/MIN TIME OF BULLETIN CREATION
C                                    TWO BYTES AS 4 BIT BCD
C
C
C     CONVERT INTO 4 BIT BCD
C
      KA        = ITIME / 1000
      KB        = MOD(ITIME,1000) / 100
      KC        = MOD(ITIME,100) / 10
      KD        = MOD(ITIME,10)
C
      QDESCR(39:39) = CHAR(KA * 16 + KB)
      QDESCR(40:40) = CHAR(kC * 16 + KD)
C
      RETURN
      END
