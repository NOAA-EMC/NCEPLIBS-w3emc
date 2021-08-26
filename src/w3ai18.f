C> @file
C> @brief Line builder subroutine.
C> @author Robert Allard @date 1974-02-01

C> Build a line of information composed of user specified
C> character strings.
C>
C> Program history log:
C> - Robert Allard 1974-02-02
C> - Ralph Jones 1984-07-05 Recompile
C> - Ralph Jones 1996-08-06 Convert from ibm370 assembler to fortran
C> for the cray, workstations, and pc's.
C>
C> @param[in] ITEM Character string to be added to line array.
C> @param[in] I1 Number of character strings to be added to line array.
C> @param[in] I2 Number of characters per string to add to line.
C> @param[in] L Character length of line to be built (2.le.l.le.256).
C> @param[in] K Number of blkank characters to precede a character
C> string (0.le.k.le.256).
C> @param[inout] N (in) Pointer set equal to 0 when beginning a line.
C> (out) Character count, error indicator.
C> @param[out] LINE Array in which character string are placed while
C> building aline; must be of type integer.
C>
C> Exit states:
C> - N = -1 Character string will not fit in the line array;
C> otherwise, each time a chacter string is added
C> to the line, n is incremented by (i2 + k).
C>
C> @note Each character string included in the item array must
C> start on a full word boundary and be equal in length.
C> Each successive string must start on the nest fullword
C> boundary following the end of the previous string.
C> On a cray this is 8.
C>
C> @note The dimensions of the item array should be at least the
C> value of (i1*(i2+j))/4, where the integer j is in the
C> range 0.le.j.le.3 and the sum (i2+j) is 4 or a multiple
C> of 4. On a cray this is 8 or a multiple of 8. On a cray
C> (i1*(i2+j))/8, range is 0.le.j.le.7
C>
C> @note The maximum dimension of line is 64 word or 256 bytes.
C> On a cray it is 32 words or 256 bytes.
C>
C> @note The user should set n = 0 each time a line is stated to
C> tell w3ai18 to fill the line array with blank characters.
C> Each time a character string is added to the line, the
C> variable (n) is incremented by (i2 + k). If a character
C> string will not fit in the line array, w3ai18 sets n = -1
C> and returns to the user. The user will not be able to
C> program a recovery procedure for the line being full if
C> more than one character string is in the item array.
C>
C> @author Robert Allard @date 1974-02-01
      SUBROUTINE W3AI18(ITEM,I1,I2,LINE,L,K,N)
C
      CHARACTER * (*) LINE
      CHARACTER * (*) ITEM
C
      SAVE
C
C     TEST WORD LENGTH, LW WILL BE 4 OR 8 BYTES
C
      CALL W3FI01(LW)
C
C     BAIL OUT IF NEGATIVE
C
      IF (N.LT.0) RETURN
C
C     FILL LINE WITH BLANK CHAACTERS
C
      IF (N.EQ.0) THEN
        DO I = 1,L
          LINE(I:I) = ' '
        END DO
      END IF
      IF (I1.EQ.1) THEN
        J = 0
        IF ((I2+K+N).GT.L) GO TO 200
          LINE(K+N+1:K+N+I2) = ITEM(1:I2)
          N = I2+K+N
          RETURN
      ELSE
        JJ = MOD(I2, LW)
        IF (JJ.EQ.0) THEN
          J = 0
        ELSE
          J = LW - JJ
        END IF
        IF ((I2+K+N).GT.L) GO TO 200
          LINE(K+N+1:K+N+I2) = ITEM(1:I2)
          N = I2+K+N
          DO I = 1,I1-1
            IF ((I2+K+N).GT.L) GO TO 200
            LINE(K+N+1:K+N+I2) = ITEM((I2+J)*I+1:(I2+J)*I+I2)
            N = I2+K+N
          END DO
          RETURN
      END IF
 200  CONTINUE
        N = -1
        RETURN
      END
