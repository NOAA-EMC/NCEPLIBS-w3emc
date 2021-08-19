C> @file
C> @brief Unpack record into IEEE F.P.
C> @author Ralph Jones @date 1989-10-17

C> Unpacks a record in office note 84 format and convert the
C> packed data to ieee real floating point numbers. The
C> office note 84 data is bit for bit the same on the nas-9050 and
C> the cray.
C>
C> Program history log:
C> - Ralph Jones 1989-10-20
C> - Ralph Jones 1990-02-02 Change to cray function for integer*2, f.p.
C> - Ralph Jones 1990-10-11 Special version of w3ai01 to unpack records
c> packed by big version of w3ai00. Will do old and new version.
C> - Ralph Jones 1991-03-19 Make special version of w3ai01 to unpack
c> big records the operational version.
C> - Ralph Jones 1993-06-10 Increace array size to 262144 words.
C> - Boi Vuong 1998-03-10 Remove the cdir$ integer=64 directive.
C> - Stephen Gilbert 1998-11-17 Changed to unpack into IEEE reals for the IBM SP.
C>
C> @param[in] PACK Integer array with data in office note 84 format to be unpacked.
C> @param[out] REAL8 Real array of n words. where n is given in word 6 of pack.
c> Word 6 of pack must contain center and scaling values.
C> @param[out] LABEL Six word integer label copied from pack,
c> 12 office note 84 32 bit id's that are stored into six 64-bit words.
C>
C> @note Label and pack may be equivalenced.
C>
C> @author Ralph Jones @date 1989-10-17

      SUBROUTINE W3AI01(PACK,REAL8,LABEL)
C
       REAL     REAL8(*)
C
       INTEGER(2)  ITEMP(262144)
       INTEGER(8)  LABEL(6)
       INTEGER(8)  PACK(*)
       INTEGER(8)  MASK16
       INTEGER(8)  MASK32
       integer(2) i2(4)
       real(4)  rtemp(2)
       integer(8)  ktemp,jtemp(65536)
       equivalence (ktemp,rtemp(1),i2(1))
       equivalence (itemp(1),jtemp(1))
C
       SAVE
C
       DATA  MASK16/Z'000000000000FFFF'/
       DATA  MASK32/Z'00000000FFFFFFFF'/
C
C      MOVE OFFICE NOTE 84 12 32 BIT ID'S INTO LABEL
C
       DO 10 I = 1,6
         LABEL(I) = PACK(I)
 10    CONTINUE
C
C GET WORD COUNT, AVERAGE VALUE, SCALING FACTOR, J, A , N.
C
         J   = IAND(LABEL(4),MASK16)
         IF (J.EQ.0) THEN
           J   = IAND(LABEL(6),MASK32)
           IF (J.EQ.0) THEN
             PRINT *,' W3AI01 ERROR, NUMBER OF WORDS IN GRID IS 0'
             RETURN
           ENDIF
           IF (J.GT.262144) THEN
             PRINT *,' W3AI01 ERROR, NUMBER OF WORDS IN GRID IS ',J
             PRINT *,' THERE IS A LIMIT OF 262144'
             RETURN
           ENDIF
         ENDIF
C
C        CONVERT IBM 32 BIT MEAN VALUE TO IEEE F.P. NUMBER
C
C         CALL USSCTC(LABEL(5),5,A,1)
         ktemp=LABEL(5)
         call q9ie32(rtemp(2),rtemp(1),1,istat)
         A=rtemp(1)
C
C        GET SCALING VALUE N, CAN BE NEGATIVE (INTEGER*2 TWO'S COMPL.)
C
C         CALL USICTC(LABEL(6),3,N,1,2)
         ktemp=LABEL(6)
         n=i2(2)
C
         TWON = 2.0 ** (N - 15)
C
C UNPACK, CONVERT TO REAL 64 BIT FLOATING POINT DATA
C
C         CALL USICTC(PACK(7),1,ITEMP,J,2)
         jtemp(1:65536)=pack(7:65542)
C
         DO 20 I = 1,J
           REAL8(I) = FLOAT(ITEMP(I)) * TWON + A
  20     CONTINUE
C
         RETURN
       END
