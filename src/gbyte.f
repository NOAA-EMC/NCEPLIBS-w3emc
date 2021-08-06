C> @file
C> @brief This is the fortran version of gbyte.
C> @author Dr. Robert C. Gammill @date 1972-05-DD

C> This is the fortran version of gbyte
C>
C> Program history log:
C> - Russell E. Jones 1991-03-DD
C> Changes for SiliconGraphics IRIS-4D/25
C> SiliconGraphics 3.3 FORTRAN 77.
C>
C> To unpack a byte into a target word. The unpacked byte is right-justified
C> in the target word, and the remainder of the word is zero-filled.
C>
C> @param[in] IPACKD The word or array containing the byte to be unpacked.
C>
C> @param[out] IUNPKD The word which will contain the unpacked byte.
C>
C> @param[in] NOFF The number of bits to skip, left to right, in IPACKD
C> in order to locate the byte to be unpacked.
C>
C> @param[in] NBITS Number of bits in the byte to be unpacked. Maximum of
C> 64 bits on 64 bit machine, 32 bits on 32 bit machine.
C>
C> @author Dr. Robert C. Gammill @date 1972-05-DD
      SUBROUTINE GBYTE(IPACKD,IUNPKD,NOFF,NBITS)
      INTEGER    IPACKD(*)
      INTEGER    IUNPKD
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
C MOVER SPECIFIES HOW FAR TO THE RIGHT NBITS MUST BE MOVED IN ORDER
C
C    TO BE RIGHT ADJUSTED.
C
      MOVER = ICON - II
C
      IF (MOVER.GT.0) THEN
        IUNPKD  = IAND(ISHFT(IPACKD(INDEX+1),-MOVER),MASK)
C
C THE BYTE IS SPLIT ACROSS A WORD BREAK.
C
      ELSE IF (MOVER.LT.0) THEN
        MOVEL = - MOVER
        MOVER = NBITSW - MOVEL
        IUNPKD  = IAND(IOR(ISHFT(IPACKD(INDEX+1),MOVEL),
     &          ISHFT(IPACKD(INDEX+2),-MOVER)),MASK)
C
C THE BYTE IS ALREADY RIGHT ADJUSTED.
C
      ELSE
        IUNPKD  = IAND(IPACKD(INDEX+1),MASK)
      ENDIF
C
      RETURN
      END
