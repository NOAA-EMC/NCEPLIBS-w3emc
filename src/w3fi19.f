C> @file
C> @brief NMC Rectangle boundary finding subroutine.
C> @author James Howcroft @date 1973-10-15

C> Relates the I,J coordinate point in a 65x65 grid-point
C> array as being either inside, outside, or on the boundary of the
C> 53x57 NMC rectangle centered in the 65x65 array.
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
C> - NW = -1 Point is outside the rectangle.
C> - NW =  0 Point is on the rectangle boundary.
C> - NW = +1 Point is inside the rectangle.
C>
C> @author James Howcroft @date 1973-10-15
      SUBROUTINE W3FI19(I,J,NW)
C
      SAVE
C
      IF (I.LT.7.OR.I.GT.59.OR.J.LT.5.OR.J.GT.61)  GO TO 10
      IF (I.EQ.7.OR.I.EQ.59.OR.J.EQ.5.OR.J.EQ.61)  GO TO 20
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
