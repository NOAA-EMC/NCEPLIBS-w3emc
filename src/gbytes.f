C> @file
C> @brief This is the fortran version of gbytes.
C> @author Dr. Robert C. Gammill @date 1972-05

C> Program history log:
C> - Russell E. Jones 1991-03
C> Changes for SiliconGraphics IRIS-4D/25
C> SiliconGraphics 3.3 FORTRAN 77
C>
C> To unpack a series of bytes into a target
C> array. Each unpacked byte is right-justified
C> in its target word, and the remainder of the
C> word is zero-filled.
C>
C> @param[in] IPACKD The word or array containing the packed bytes.
C> @param[out] IUNPKD The array which will contain the unpacked bytes.
C> @param[in] NOFF The initial number of bits to skip, left to right,
C> in 'IPACKD' in order to locate the first byte to unpack.
C> @param[in] NBITS Number of bits in the byte to be unpacked.
C> Maximum of 64 bits on 64 bit machine, 32 bits on 32 bit machine.
C> @param[in] ISKIP The number of bits to skip between each byte in
C> 'IPACKD' in order to locate the next byte to be unpacked.
C> @param[in] ITER The number of bytes to be unpacked.
C>
      SUBROUTINE GBYTES(IPACKD,IUNPKD,NOFF,NBITS,ISKIP,ITER)

      INTEGER    IPACKD(*)

      INTEGER    IUNPKD(*)
      INTEGER    MASKS(64)
C
      SAVE
C
      DATA IFIRST/1/
      IF(IFIRST.EQ.1) THEN
         CALL W3FI01(LW)
         NBITSW = 8 * LW
         JSHIFT = -1 * NINT(ALOG(FLOAT(NBITSW)) / ALOG(2.0))
         MASKS(1) = 1
         DO I=2,NBITSW-1
            MASKS(I) = 2 * MASKS(I-1) + 1
         ENDDO
         MASKS(NBITSW) = -1
         IFIRST = 0
      ENDIF
C
C NBITS MUST BE LESS THAN OR EQUAL TO NBITSW
C
      ICON   = NBITSW - NBITS
      IF (ICON.LT.0) RETURN
      MASK   = MASKS(NBITS)
C
C INDEX TELLS HOW MANY WORDS INTO THE ARRAY 'IPACKD' THE NEXT BYTE
C APPEARS.
C
      INDEX  = ISHFT(NOFF,JSHIFT)
C
C II TELLS HOW MANY BITS THE BYTE IS FROM THE LEFT SIDE OF THE WORD.
C
      II     = MOD(NOFF,NBITSW)
C
C ISTEP IS THE DISTANCE IN BITS FROM THE START OF ONE BYTE TO THE NEXT.
C
      ISTEP  = NBITS + ISKIP
C
C IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.
C
      IWORDS = ISTEP / NBITSW
C
C IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.
C
      IBITS  = MOD(ISTEP,NBITSW)
C
      DO 10 I = 1,ITER
C
C MOVER SPECIFIES HOW FAR TO THE RIGHT A BYTE MUST BE MOVED IN ORDER
C
C    TO BE RIGHT ADJUSTED.
C
      MOVER = ICON - II
C
C THE BYTE IS SPLIT ACROSS A WORD BREAK.
C
      IF (MOVER.LT.0) THEN
        MOVEL   = - MOVER
        MOVER   = NBITSW - MOVEL
        IUNPKD(I) = IAND(IOR(ISHFT(IPACKD(INDEX+1),MOVEL),
     &            ISHFT(IPACKD(INDEX+2),-MOVER)),MASK)
C
C RIGHT ADJUST THE BYTE.
C
      ELSE IF (MOVER.GT.0) THEN
        IUNPKD(I) = IAND(ISHFT(IPACKD(INDEX+1),-MOVER),MASK)
C
C THE BYTE IS ALREADY RIGHT ADJUSTED.
C
      ELSE
        IUNPKD(I) = IAND(IPACKD(INDEX+1),MASK)
      ENDIF
C
C INCREMENT II AND INDEX.
C
        II    = II + IBITS
        INDEX = INDEX + IWORDS
        IF (II.GE.NBITSW) THEN
          II    = II - NBITSW
          INDEX = INDEX + 1
        ENDIF
C
   10 CONTINUE
        RETURN
      END
