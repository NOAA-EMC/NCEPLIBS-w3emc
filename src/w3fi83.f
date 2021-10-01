C> @file
C> @brief Restore delta packed data to original.
C> @author Bill Cavanaugh @date 1993-08-18

C> Restore delta packed data to original values
C> restore from boustrephedonic alignment.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-07-14
C> - John Satckpole 1993-07-22 Additions to fix scaling.
C> - Bill Cavanaugh 1994-01-27 Added reversal of even numbered rows
C> (boustrophedonic processing) to restore
C> data to original sequence.
C> - Bill Cavanaugh 1994-03-02 Corrected reversal of even numbered rows.
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C>
C> @param[inout] DATA
C> - [in] Second order differences.
C> - [out] Expanded original data values.
C> @param[in] NPTS Number of points in array.
C> @param[in] FVAL1 Original first entry in array.
C> @param[in] FDIFF1 Original first first-difference.
C> @param[in] ISCAL2 Power-of-two exponent for unscaling.
C> @param[in] ISC10 Power-of-ten exponent for unscaling.
C> @param[in] KPDS Array of information for pds.
C> @param[in] KGDS Array of information for gds.
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Bill Cavanaugh @date 1993-08-18
      SUBROUTINE W3FI83 (DATA,NPTS,FVAL1,FDIFF1,ISCAL2,
     *                                ISC10,KPDS,KGDS)
C
      REAL          FVAL1,FDIFF1
      REAL          DATA(*),BOUST(200)
      INTEGER       NPTS,NROW,NCOL,KPDS(*),KGDS(*),ISC10
C  ---------------------------------------
C
C     REMOVE DECIMAL UN-SCALING INTRODUCED DURING UNPACKING
C
      DSCAL = 10.0 ** ISC10
      IF (DSCAL.EQ.0.0) THEN
          DO 50 I=1,NPTS
              DATA(I) = 1.0
   50     CONTINUE
      ELSE IF (DSCAL.EQ.1.0) THEN
      ELSE
          DO 51 I=1,NPTS
              DATA(I) = DATA(I) * DSCAL
   51     CONTINUE
      END IF
C
      DATA(1)  = FVAL1
      DATA(2)  = FDIFF1
      DO 200 J = 3,2,-1
          DO 100 K = J, NPTS
              DATA(K)  = DATA(K) + DATA(K-1)
  100     CONTINUE
  200 CONTINUE
C
C     NOW REMOVE THE BINARY SCALING FROM THE RECONSTRUCTED FIELD
C     AND THE DECIMAL SCALING TOO
C
      IF (DSCAL.EQ.0) THEN
          SCALE  = 0.0
      ELSE
          SCALE =(2.0**ISCAL2)/DSCAL
      END IF
      DO 300 I=1,NPTS
        DATA(I) = DATA(I) * SCALE
  300 CONTINUE
C  ==========================================================
      IF (IAND(KPDS(4),128).NE.0) THEN
          NROW  = KGDS(3)
          NCOL  = KGDS(2)
C
C      DATA LAID OUT BOUSTROPHEDONIC STYLE
C
C
C         PRINT*, '  REVERSE BOUSTROPHEDON'
          DO 210 I = 2, NROW, 2
C
C          REVERSE THE EVEN NUMBERED ROWS
C
              DO 201 J = 1, NCOL
                  NPOS  = I * NCOL - J + 1
                  BOUST(J) = DATA(NPOS)
  201         CONTINUE
              DO 202 J = 1, NCOL
                  NPOS  = NCOL * (I-1) + J
                  DATA(NPOS)  = BOUST(J)
  202         CONTINUE
  210     CONTINUE
C
C
      END IF
C  =================================================================
      RETURN
      END
