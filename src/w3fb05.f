C> @file
C> @brief Grid coordinates to latitude, longitude.
C> @author Ralph Jones @date 1986-07-17

C> Converts the coordinates of a location from the grid(i,j)
C> coordinate system overlaid on the polar stereographic map projec-
C> tion true at 60 degrees n or s latitude to the natural coordinate
C> system of latitude/longitude on the earth. w3fb05() is the reverse
C> of w3fb04().
C>
C> Program history log:
C> - Ralph Jones 1986-07-17
C> - Ralph Jones 1989-11-01 Change to cray cft77 fortran.
C>
C> @param[in] XI I of the point relative to the north or s. pole
C> @param[in] XJ J of the point relative to the north or s. pole
C> @param[in] XMESHL Mesh length of grid in km at 60 degrees(<0 if sh)
C> (190.5 lfm grid, 381.0 nh pe grid,-381.0 sh pe grid)
C> @param[in] ORIENT Orientation west longitude of the grid
C> (105.0 lfm grid, 80.0 nh pe grid, 260.0 sh pe grid)
C> @param[out] ALAT Latitude in degrees  (<0 if sh)
C> @param[out] ALONG West longitude in degrees
C>
C> @note All parameters in the calling statement must be
C> real. the range of allowable latitudes is from a pole to
C> 30 degrees into the opposite hemisphere.
C> the grid used in this subroutine has its origin (i=0,j=0)
C> at the pole, so if the user's grid has its origin at a point
C> other than a pole, a translation is required to get i and j for
C> input into w3fb05(). the subroutine grid is oriented so that
C> gridlines of i=constant are parallel to a west longitude sup-
C> plied by the user. the earth's radius is taken to be 6371.2 km.
C>
C> @note This code will not vectorize, it is normaly used in a
C> double do loop with w3ft01(), w3ft00(), etc. to vectorize it,
C> put it in line, put w3ft01(), w3ft00(), etc. in line.
C>
C> @author Ralph Jones @date 1986-07-17
      SUBROUTINE W3FB05(XI,XJ,XMESHL,ORIENT,ALAT,ALONG)
C
      DATA  DEGPRD/57.2957795/
      DATA  EARTHR/6371.2/
C
      GI2   = ((1.86603 * EARTHR) / (XMESHL))**2
      R2    = XI * XI + XJ * XJ
C
      IF (R2.EQ.0.0) THEN
        ALONG = 0.0
        ALAT  = 90.0
        IF (XMESHL.LT.0.0) ALAT = -ALAT
        RETURN
      ELSE
        ALAT  = ASIN((GI2 - R2) / (GI2 + R2)) * DEGPRD
        ANGLE = DEGPRD * ATAN2(XJ,XI)
        IF (ANGLE.LT.0.0) ANGLE = ANGLE + 360.0
      ENDIF
C
      IF (XMESHL.GE.0.0) THEN
        ALONG = 270.0 + ORIENT - ANGLE
C
      ELSE
C
        ALONG = ANGLE + ORIENT - 270.0
        ALAT  = -(ALAT)
      ENDIF
C
      IF (ALONG.LT.0.0)   ALONG = ALONG + 360.0
      IF (ALONG.GE.360.0) ALONG = ALONG - 360.0
C
      RETURN
C
      END
