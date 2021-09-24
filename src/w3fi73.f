C> @file
C> @brief Construct grib bit map section (BMS).
C> @author M. Farley @date 1992-07-01

C> This subroutine constructs a grib bit map section.
C>
C> Program history log:
C> - M. Farley 1992-07-01
C> - Bill Cavanaugh 1994-02-14 Recoded
C> - Ebisuzaki 1998-06-30 Linux port
C>
C> @param[in] IBFLAG
C> - 0, if bit map supplied by user
C> - #, Number of predefined center bit map
C> @param[in] IBMAP Integer array containing user bit map
C> @param[in] IBLEN Length of bit map
C> @param[out] BMS Completed grib bit map section
C> @param[out] LENBMS Length of bit map section
C> @param[out] IER 0 normal exit, 8 = ibmap values are all zero
C>
C> @author M. Farley @date 1992-07-01
      SUBROUTINE W3FI73 (IBFLAG,IBMAP,IBLEN,BMS,LENBMS,IER)
C
      INTEGER       IBMAP(*)
      INTEGER       LENBMS
      INTEGER       IBLEN
      INTEGER       IBFLAG
C
      CHARACTER*1   BMS(*)
C
      IER   = 0
C
      IZ  = 0
      DO 20 I = 1, IBLEN
          IF (IBMAP(I).EQ.0) IZ  = IZ + 1
   20 CONTINUE
      IF (IZ.EQ.IBLEN) THEN
C
C     AT THIS POINT ALL BIT MAP POSITIONS ARE ZERO
C
        IER = 8
        RETURN
      END IF
C
C     BIT MAP IS A COMBINATION OF ONES AND ZEROS
C     OR      BIT MAP ALL ONES
C
C     CONSTRUCT BIT MAP FIELD OF BIT MAP SECTION
C
      CALL SBYTESC(BMS,IBMAP,48,1,0,IBLEN)
C
      IF (MOD(IBLEN,16).NE.0) THEN
          NLEFT  = 16 - MOD(IBLEN,16)
      ELSE
          NLEFT  = 0
      END IF
C
      NUM  = 6 + (IBLEN+NLEFT) / 8
C
C     CONSTRUCT BMS FROM COLLECTED DATA
C
C     SIZE INTO FIRST THREE BYTES
C
      CALL SBYTEC(BMS,NUM,0,24)
C                          NUMBER OF FILL BITS INTO BYTE 4
      CALL SBYTEC(BMS,NLEFT,24,8)
C                          OCTET 5-6 TO CONTAIN INFO FROM IBFLAG
      CALL SBYTEC(BMS,IBFLAG,32,16)
C
C     BIT MAP MAY BE ALL ONES OR A COMBINATION
C     OF ONES AND ZEROS
C
C     ACTUAL BITS OF BIT MAP PLACED ALL READY
C
C     INSTALL FILL POSITIONS IF NEEDED
      IF (NLEFT.NE.0) THEN
          NLEFT  = 16 - NLEFT
C                          ZERO FILL POSITIONS
          CALL SBYTEC(BMS,0,IBLEN+48,NLEFT)
      END IF
C
C     STORE NUM IN LENBMS  (LENGTH OF BMS SECTION)
C
      LENBMS = NUM
C     PRINT *,'W3FI73 - BMS LEN =',NUM,LENBMS
C
      RETURN
      END
