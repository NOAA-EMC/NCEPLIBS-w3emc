C> @file
C> @brief A point interpolater.
C> @author James Howcroft @date 1979-02-15

C> Do either bilinear or biquadratic interpolation for a
C> point within a two-dimensional data array.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1979-02-15 | James Howcroft | Initial.
C> 1989-01-25 | Ralph Jones | Change to microsoft fortran 4.10.
C> 1990-06-12 | Ralph Jones | Change to sun fortran 1.3.
C> 1991-03-30 | Ralph Jones | Convert to silicongraphics fortran.
C> 1993-03-29 | Ralph Jones | Add save statement.
C> 1996-07-01 | Ralph Jones | Compile on cray.
C>
C> @param[in] FL Real*4 two-dimensional cartesian array of data.
C> @param[in] MAXI Integer*4 i-dimension of fl.
C> @param[in] MAXJ Integer*4 j-dimension of fl.
C> @param[in] STI Real*4 i-coordinate to which a value is to be
C> interpolated.
C> @param[in] STJ Real*4 j-coordinate to which a value is to be
C> interpolated.
C> @param ITYPE
C> @param[out] HI Real*4 interpolated output value.
C>
C> @remark No error checks are made. it is left for the user to
C> determine that the point for which interpolation is desired
C> lies within the grid.
C>
C> @author James Howcroft @date 1979-02-15
      SUBROUTINE W3FT03(FL,HI,STI,STJ,MAXI,MAXJ,ITYPE)
C
       REAL            FL(MAXI,MAXJ)
       REAL            E (4)
C
       SAVE
C
       I  = STI
       J  = STJ
       DI = I
       DJ = J
       DI = STI - DI
       DJ = STJ - DJ
C
       HI = 0.
C                      TEST FOR POINT OFF GRID.
       IF (I.LT.1 .OR. I.GT.MAXI) GO TO 300
       IF (J.LT.1 .OR. J.GT.MAXJ) GO TO 300
       IF (ITYPE .NE. 2) GO TO 100
C                      DO BILINEAR IF POINT IS BETWEEN ULTIMATE AND
C                      PENULTIMATE ROWS, WHERE BIQUAD NOT POSSIBLE.
       IF (I.LT.2 .OR. I.GT.(MAXI-1)) GO TO 100
       IF (J.LT.2 .OR. J.GT.(MAXJ-1)) GO TO 100
       GO TO 200
C
C        BILINEAR.
  100  CONTINUE
         HI = FL(I  ,J  )*(1.-DI)*(1.-DJ)  + FL(I+1,J  )*DI*(1.-DJ)
     &      + FL(I  ,J+1)*(1.-DI)*DJ       + FL(I+1,J+1)*DI*DJ
         GO TO 300
C
  200  CONTINUE
C        BIQUADRATIC.
       DI2 = DI*(DI-1.)*.25
       DJ2 = DJ*(DJ-1.)*.25
       J1  = J - 1
       DO 250  K=1,4
         E(K) = FL(I  ,J1)*(1.-DI-DI2) + FL(I+1,J1)*(DI-DI2)
     &        + (FL(I-1,J1) + FL(I+2,J1))*DI2
         J1 = J1 + 1
  250  CONTINUE
       HI = E(2)*(1.-DJ-DJ2) + E(3)*(DJ-DJ2) + (E(1) + E(4))*DJ2
C
  300  CONTINUE
       RETURN
       END
