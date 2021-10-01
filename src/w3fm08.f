C> @file
C> @brief Nine point smoother/desmoother.
C> @author J. Howcroft @date 1971-02-01

C> Nine point smoother/desmoother. Smoother pass uses an
C> equivalent linear smoother with stencil (.25 .5 .25) and the
C> desmoother uses stencil (-.25 1.5 -.25). Two grid interval waves
C> are annihilated, four grid interval waves have a .75 response.
C>
C> Program history log:
C> - J. Howcroft 1971-02-01
C> - Ralph Jones 1984-07-01 Change to ibm vs fortran.
C> - Ralph Jones 1994-07-27 Change to cray cft77 fortran.
C>
C> @param[inout] A
C> - [in] Real size (li,lj) array to hold field to be smoothed.
C> - [out] Array holding smoothed field.
C> @param[in] Z - Real size (li,lj) work area.
C> @param[in] LI - Integer number of columns.
C> @param[in] LJ - Integer number of rows.
C>
C> @author J. Howcroft @date 1971-02-01
      SUBROUTINE W3FM08 (A,Z,LI,LJ)
C
      REAL     A(LI,LJ)
      REAL     Z(LI,LJ)
C
      SAVE
C
      LI1 = LI - 1
      LJ1 = LJ - 1
      DO 1  J=2,LJ1
      DO 1  I=2,LI1
      CRUX = A(I-1,J-1) + A(I+1,J-1) + A(I+1,J+1) + A(I-1,J+1)
      PLUS = A(I,J-1) + A(I,J+1) + A(I-1,J) + A(I+1,J)
      Z(I,J) = 0.25 * A(I,J)  + .125 * PLUS + .0625 * CRUX
    1 CONTINUE
      DO 2  I=1,LI
      Z(I,1) = A(I,1)
      Z(I,LJ) = A(I,LJ)
    2 CONTINUE
      DO 3  J=1,LJ
      Z(1,J) = A(1,J)
      Z(LI,J) = A(LI,J)
    3 CONTINUE
      DO 4  J=2,LJ1
      DO 4  I=2,LI1
      CRUX = Z(I-1,J-1) + Z(I+1,J-1) + Z(I+1,J+1) + Z(I-1,J+1)
      PLUS = Z(I,J-1) + Z(I,J+1) + Z(I-1,J) + Z(I+1,J)
      A(I,J) = 2.25 * Z(I,J)  - .375 * PLUS + .0625 * CRUX
    4 CONTINUE
      RETURN
      END
