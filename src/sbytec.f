C> @file
C> @brief Wrapper for sbytesc()
C> @author Unknown

C> This is a wrapper for sbytesc()
C> @param[in] OUT = packed array output
C> @param[in] IN = unpacked array input
C> @param[in] ISKIP = initial number of bits to skip
C> @param[in] NBYTE = number of bits to pack
C>
C> @author Unknown

      SUBROUTINE SBYTEC(OUT,IN,ISKIP,NBYTE)
      character*1 out(*)
      integer in(*)
      CALL SBYTESC(OUT,IN,ISKIP,NBYTE,0,1)
      RETURN
      END
