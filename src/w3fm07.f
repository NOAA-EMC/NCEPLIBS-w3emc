C> @file
C> @brief Nine-point smoother for rectangular grids.
C> @author P. Chase @date 1975-04-01

C> Smooths data on a rectangular grid using a nine-point
C> smoothing operator.
C>
C> Program history log:
C> P. Chase 1975-04-01
C> Ralph Jones 1984-07-01 Change to ibm vs fortran
C> Ralph Jones 1991-04-24 Change to cray cft77 fortran
C>
C> @param[in] FIN - Real size(ncol*nrow) array of data to be smoothed
C> @param[in] CWORK - Real size(2*ncol*(nrow+2)) work array
C> @param[in] GAMMA - Complex smoothing parameter. The imaginary part must
C> be positive.
C> @param[in] NCOL - Integer number of columns in the grid
C> @param[in] NROW - Integer number of rows in the grid
C> @param[out] FOUT - Real size(ncol*nrow) array of smoothed data. May
C> be the same array as 'fin' or overlap it in any fashion.
C>
C> @author P. Chase @date 1975-04-01
      SUBROUTINE W3FM07(FIN,FOUT,CWORK,GAMMA,NCOL,NROW)
C
      REAL    FIN(NCOL,NROW)
      REAL    FOUT(NCOL,NROW)
C
      COMPLEX CWORK(NCOL,*),GAMMA,GAMMX,GAMA,GAMB,GAMC
C
      GAMMX  = GAMMA
      XSWTCH = AIMAG(GAMMX)
      NCOLM  = NCOL-1
      NROWM  = NROW-1
C
C INITIALIZE WORK ARRAY.  WORK ARRAY STARTS UP TWO ROWS SO IT CAN SMOOTH
C DOWNWARD WITHOUT OVERLAP OF SMOOTHED AND UNSMOOTHED DATA
C
      DO 10 J = 1,NROW
      DO 10 I = 1,NCOL
        CWORK(I,J+2) = CMPLX(FIN(I,J),0.)
 10   CONTINUE
      IF (XSWTCH .EQ. 0.) GO TO 30
      DO 20 J = 1,NROW,NROWM
        JJ = J+ISIGN(1,NROWM-J)
      DO 20 I = 1,NCOL,NCOLM
        II = I+ISIGN(1,NCOLM-I)
        CWORK(I,J+2) = CMPLX(FIN(I,JJ)+FIN(II,J)-FIN(II,JJ),0.)
 20   CONTINUE
C
C SET SMOOTHING OPERATORS
C
 30   GAMA = 0.50 * GAMMX * (1.0 - GAMMX)
      GAMB = 0.25 * GAMMX * GAMMX
      GAMC = 0.50 * GAMMX
C
C SMOOTH WORK ARRAY, PUTTING SMOOTHED POINTS DOWN TWO ROWS
C
      CWORK(1,1) = CWORK(1,3)
      CWORK(NCOL,1) = CWORK(NCOL,3)
      DO 40 I = 2,NCOLM
      CWORK(I,1) = CWORK(I,3)+GAMC*(CWORK(I-1,3)-2.*CWORK(I,3)+
     & CWORK(I+1,3))
 40   CONTINUE
      DO 60 J = 2,NROWM
      DO 50 I = 1,NCOL,NCOLM
      CWORK(I,J) = CWORK(I,J+2)+GAMC*(CWORK(I,J+1)-2.*CWORK(I,J+2)+
     & CWORK(I,J+3))
 50   CONTINUE
      DO 60 I = 2,NCOLM
      CWORK(I,J) = CWORK(I,J+2)+GAMA*(CWORK(I+1,J+2)+CWORK(I-1,J+2)+
     & CWORK(I,J+1)+CWORK(I,J+3)-4.*CWORK(I,J+2))+GAMB*(CWORK(I-1,J+1)+
     & CWORK(I+1,J+1)+CWORK(I-1,J+3)+CWORK(I+1,J+3)-4.*CWORK(I,J+2))
 60   CONTINUE
      CWORK(1,NROW) = CWORK(1,NROW+2)
      CWORK(NCOL,NROW) = CWORK(NCOL,NROW+2)
      DO 70 I = 2,NCOLM
      CWORK(I,NROW) = CWORK(I,NROW+2)+GAMC*(CWORK(I-1,NROW+2)-2.*
     & CWORK(I,NROW+2)+CWORK(I+1,NROW+2))
 70   CONTINUE
C
C IF IMAGINARY PART OF SMOOTHING PARAMETER IS NOT POSITIVE, DONE
C
      IF (XSWTCH .LE. 0.) GO TO 90
C
C OTHERWISE MOVE WORK ARRAY BACK UP TWO ROWS
C
      DO 80 JJ=1,NROW
      J = NROW+1-JJ
      DO 80 I=1,NCOL
      CWORK(I,J+2) = CWORK(I,J)
 80   CONTINUE
C
C SET SMOOTHING PARAMETER FOR CONJUGATE PASS AND GO DO IT
C
      GAMMX  = CONJG(GAMMX)
      XSWTCH = AIMAG(GAMMX)
      GO TO 30
C
C DONE.  OUTPUT SMOOTH ARRAY
C
 90   DO 100 J = 1,NROW
      DO 100 I = 1,NCOL
        FOUT(I,J) = REAL(CWORK(I,J))
 100  CONTINUE
      RETURN
      END
