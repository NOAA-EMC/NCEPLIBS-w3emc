C> @file
C> @brief Blocker Subroutine.
C> @author Robert Allard @date 1997-04-15

C> Fills a record block with logical records or lines of information.
C>
C> Program history log:
C> - Robeert Allard 1974-02-01
C> - Ralph Jones 1990-09-15 Convert from ibm370 assembler to microsoft
C> fortran 5.0.
C> - Ralph Jones 1990-10-07 Convert to sun fortran 1.3.
C> - Ralph Jones 1991-07-20 Convert to silicongraphics 3.3 fortran 77.
C> - Ralph Jones 1993-03-29 Add save statement.
C> - Ralph Jones 1994-04-22 Add xmovex and xstore to move and
C> store character data faster on the cray.
C> - Bob Hollern 1997-04-15 Corrected the problem of iniializing nblk
C> to @'s instead of blanks.
C>
C> @param[in] LINE Array address of logical record to be blocked.
C> @param[in] L Number of characters in line to be blocked.
C> @param[in] N Maximum character size of nblk.
C> @param[inout] NEXT (in) flag, initialized to 0. (out) character count, error indicator.
C> @param[out] NBLK Block filled with logical records.
C>
C> Exit states:
C> - NEXT = -1  Line will not fit into remainder of block;
C>                otherwise, next is set to (next + l).
C> - NEXT = -2  N is zero or less.
C> - NEXT = -3  L is zero or less.
C>
C> @author Robert Allard @date 1997-04-15
      SUBROUTINE W3AI19(LINE, L, NBLK, N, NEXT)
C
C METHOD:
C
C     THE USER MUST SET NEXT = 0 EACH TIME NBLK IS TO BE FILLED WITH
C LOGICAL RECORDS.
C
C     W3AI19 WILL THEN MOVE THE LINE OF INFORMATION INTO NBLK, STORE
C BLANK CHARACTERS IN THE REMAINDER OF THE BLOCK, AND SET NEXT = NEXT
C + L.
C
C     EACH TIME W3AI19 IS ENTERED, ONE LINE IS BLOCKED AND NEXT INCRE-
C MENTED UNTIL A LINE WILL NOT FIT THE REMAINDER OF THE BLOCK.  THEN
C W3AI19 WILL SET NEXT = -1 AS A FLAG FOR THE USER TO DISPOSE OF THE
C BLOCK.  THE USER SHOULD BE AWARE THAT THE LAST LOGICAL RECORD WAS NOT
C BLOCKED.
C
         INTEGER       L
         INTEGER       N
         INTEGER       NEXT
         INTEGER(8)       WBLANK
C
         CHARACTER * 1 LINE(*)
         CHARACTER * 1 NBLK(*)
         CHARACTER * 1 BLANK
C
         SAVE
C
         DATA  WBLANK/Z'2020202020202020'/
C
C TEST VALUE OF NEXT.
C
         IF (NEXT.LT.0) THEN
           RETURN
C
C TEST N FOR ZERO OR LESS
C
         ELSE IF (N.LE.0) THEN
           NEXT = -2
           RETURN
C
C TEST L FOR ZERO OR LESS
C
         ELSE IF (L.LE.0) THEN
           NEXT = -3
           RETURN
C
C TEST TO SEE IF LINE WILL FIT IN BLOCK.
C
         ELSE IF ((L + NEXT).GT.N) THEN
           NEXT = -1
           RETURN
C
C FILL BLOCK WITH BLANK CHARACTERS IF NEXT EQUAL ZERO.
C BLANK IS EBCDIC BLANK, 40 HEX, OR 64 DECIMAL
C
         ELSE IF (NEXT.EQ.0) THEN
           CALL W3FI01(LW)
           IWORDS = N / LW
           CALL XSTORE(NBLK,WBLANK,IWORDS)
           IF (MOD(N,LW).NE.0) THEN
             NWORDS = IWORDS * LW
             IBYTES = N - NWORDS
             DO I = 1,IBYTES
               NBLK(NWORDS+I) = CHAR(32)
             END DO
           END IF
         END IF
C
C MOVE LINE INTO BLOCK.
C
C        DO 20 I = 1,L
C          NBLK(I + NEXT) = LINE(I)
C20      CONTINUE
         CALL XMOVEX(NBLK(NEXT+1),LINE,L)
C
C ADJUST VALUE OF NEXT.
C
        NEXT = NEXT + L
C
        RETURN
C
        END
