C> @file
C> @brief Convert to second diff array
C> @author Bill Cavanaugh @date 1993-07-14

C> Accept an input array, convert to array of second
C> differences.  return the original first value and the first
C> first-difference as separate values. align data in
C> boustrephedonic style, (alternate row reversal).
C>
C> Program history log:
C> - Bill Cavanaugh 1993-07-14
C> - Bill Cavanaugh 1994-01-27 Added reversal of even numbered rows
C> (boustrophedonic processing)
C> - Bill Cavanaugh 1994-03-02 Corrected improper ordering of even
C> numbered rows
C> - Ebisuzaki 1999-12-06 Linux port
C>
C> @param[inout] IFLD
C> - [in] Integer input array
C> - [out] Second differenced field
C> @param[in] NPTS Number of points in array
C> @param[in] IGDS
C> - (5) Number of rows in array
C> - (4) Number of columns in array
C> @param[in] PDS (8) Flag indicating presence of gds section
C> @param[out] FVAL1 Floating point original first value
C> @param[out] FDIFF1 Floating point first first-difference
C>
C> @author Bill Cavanaugh @date 1993-07-14
      SUBROUTINE W3FI82 (IFLD,FVAL1,FDIFF1,NPTS,PDS,IGDS)
C
      REAL        FVAL1,FDIFF1
C
      INTEGER     IFLD(*),NPTS,NBOUST(300),IGDS(*)
C
      CHARACTER*1 PDS(*)
C
C  ---------------------------------------------
C                  TEST FOR PRESENCE OF GDS
C
c looks like an error      CALL GBYTE(PDS,IQQ,56,8)
      call gbytec(PDS,IQQ,56,1)
      IF (IQQ.NE.0) THEN
          NROW  = IGDS(5)
          NCOL  = IGDS(4)
C
C      LAY OUT DATA BOUSTROPHEDONIC STYLE
C
C         PRINT*, '  DATA SET UP BOUSTROPHEDON'
C
          DO 210 I = 2, NROW, 2
C
C          REVERSE THE EVEN NUMBERED ROWS
C
              DO 200 J = 1, NCOL
                  NPOS  = I * NCOL - J + 1
                  NBOUST(J) = IFLD(NPOS)
  200         CONTINUE
              DO 201 J = 1, NCOL
                  NPOS  = NCOL * (I-1) + J
                  IFLD(NPOS)  = NBOUST(J)
  201         CONTINUE
  210     CONTINUE
C
C
      END IF
C  =================================================================
          DO 4000 I = NPTS, 2, -1
              IFLD(I)  = IFLD(I) - IFLD(I-1)
 4000     CONTINUE
          DO 5000 I = NPTS, 3, -1
              IFLD(I)  = IFLD(I) - IFLD(I-1)
 5000     CONTINUE
C
C                      SPECIAL FOR GRIB
C                         FLOAT OUTPUT OF FIRST POINTS TO ANTICIPATE
C                         GRIB FLOATING POINT OUTPUT
C
          FVAL1    = IFLD(1)
          FDIFF1   = IFLD(2)
C
C       SET FIRST TWO POINTS TO SECOND DIFF VALUE FOR BETTER PACKING
C
          IFLD(1)  = IFLD(3)
          IFLD(2)  = IFLD(3)
C  -----------------------------------------------------------
      RETURN
      END
