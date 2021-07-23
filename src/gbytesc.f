C> @file
C> @brief Get bytes - unpack bits
C> @author Unknown

C> Extract arbitrary size values from a packed bit string,
C> right justifying each value in the unpacked array. v1.1
C>
C> @param[in] IN Character*1 array input.
C> @param[out] IOUT Unpacked array output.
C> @param[in] ISKIP Initial number of bits to skip.
C> @param[in] NBYTE Number of bits to take.
C> @param[in] NSKIP Additional number of bits to skip on each iteration.
C> @param[in] N Number of iterations.
C>
      SUBROUTINE GBYTESC(IN,IOUT,ISKIP,NBYTE,NSKIP,N)
      character*1 in(*)
      integer iout(*)
      integer ones(8), tbit, bitcnt
      save ones
      data ones/1,3,7,15,31,63,127,255/

c     nbit is the start position of the field in bits
      nbit = iskip
      do i = 1, n
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        first byte
         tbit = min(bitcnt,8-ibit)
         itmp = iand(mova2i(in(index)),ones(8-ibit))
         if (tbit.ne.8-ibit) itmp = ishft(itmp,tbit-8+ibit)
         index = index + 1
         bitcnt = bitcnt - tbit

c        now transfer whole bytes
         do while (bitcnt.ge.8)
             itmp = ior(ishft(itmp,8),mova2i(in(index)))
             bitcnt = bitcnt - 8
             index = index + 1
         enddo

c        get data from last byte
         if (bitcnt.gt.0) then
             itmp = ior(ishft(itmp,bitcnt),iand(ishft(mova2i(in(index)),
     1          -(8-bitcnt)),ones(bitcnt)))
         endif

         iout(i) = itmp
      enddo

      RETURN
      END
