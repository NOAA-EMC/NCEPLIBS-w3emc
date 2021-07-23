C> @file
C> @brief Wrapper for gbytesc() limiting NSKIP and N to 0 and 1
C> @author Unknown

C> Wrapper for gbytesc() limiting NSKIP and N to 0 and 1
C>
C> @param[in] IN Character*1 array input.
C> @param[out] IOUT Unpacked array output.
C> @param[in] ISKIP Initial number of bits to skip.
C> @param[in] NBYTE Number of bits to take.
C>

      SUBROUTINE GBYTEC(IN,IOUT,ISKIP,NBYTE)
      character*1 in(*)
      integer iout(*)
      CALL GBYTESC(IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END
