C> @file
C> @brief NMC octagon boundary finding subroutine.
C> @author James Howcroft @date 1973-10-15

C> Relates the I,J coordinate point in a 65x65 grid-point
C> array as being either inside, outside, or on the boundary of the
C> NMC octagon centered in the 65x65 array.
C>
C> Program history log:
C> - James Howcroft 1973-10-15
C> - Ralph Jones 1984-07-02 Convert to fortran 77.
C> - Ralph Jones 1989-02-02 Convert to microsoft fortran 4.10.
C> - Ralph Jones 1990-06-12 Convert to sun fortran 1.3.
C> - Ralph Jones 1991-03-16 Convert to silicongraphics 3.3 fortran 77.
C> - Ralph Jones 1993-03-29 Add save statement.
C>
C> @param[in] I Coordinate identification of a point in the 65x65 array.
C> @param[in] J Coordinate identification of a point in the 65x65 array.
C> @param[out] NW Integer return code.
C>
C> Exit states:
C> - NW = -1 Point is outside the octagon.
C> - NW =  0 Point is on the octagon boundary.
C> - NW = +1 Point is inside the octagon.
C>
C> @author James Howcroft @date 1973-10-15
      SUBROUTINE W3FI18(I,J,NW)
C
      SAVE
C
      K = I + J
      M = I - J
      IF (I.LT.10.OR.I.GT.56.OR.J.LT.8.OR.J.GT.58)    GO TO 10
      IF (K.LT.32.OR.K.GT.100.OR.M.LT.-34.OR.M.GT.34) GO TO 10
      IF (I.EQ.10.OR.I.EQ.56.OR.J.EQ.8.OR.J.EQ.58)    GO TO 20
      IF (K.EQ.32.OR.K.EQ.100.OR.M.EQ.-34.OR.M.EQ.34) GO TO 20
      NW = 1
      RETURN
C
   10 CONTINUE
      NW = -1
      RETURN
C
   20 CONTINUE
      NW = 0
      RETURN
      END
