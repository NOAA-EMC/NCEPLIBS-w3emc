C> @file
C> @brief Latitude, longitude to grid coordinates.
C> @author James McDonell @date 1986-07-17

C> Converts the coordinates of a location on earth from the
C> natural coordinate system of latitude/longitude to the grid (i,j)
C> coordinate system overlaid on a polar stereographic map pro-
C> jection true at 60 degrees n or s latitude. w3fb04() is the reverse
C> of w3fb05().
C>
C> Program history log:
C> - James McDonell 1986-07-17
C> - Ralph Jones 1988-06-07 Clean up code, take out goto, use then, else.
C> - Ralph Jones 1989-11-02 Change to cray cft77 fortran.
C>
C> @param[in] ALAT Latitude in degrees (<0 if sh).
C> @param[in] ALONG West longitude in degrees.
C> @param[in] XMESHL Mesh length of grid in km at 60 deg lat(<0 if sh)
C> (190.5 lfm grid, 381.0 nh pe grid,-381.0 sh pe grid).
C> @param[in] ORIENT Orientation west longitude of the grid
C> (105.0 lfm grid, 80.0 nh pe grid, 260.0 sh pe grid).
C> @param[out] XI I of the point relative to north or south pole.
C> @param[out] XJ J of the point relative to north or south pole.
C>
C> @note All parameters in the calling statement must be
c> real. the range of allowable latitudes is from a pole to
c> 30 degrees into the opposite hemisphere.
c> The grid used in this subroutine has its origin (i=0,j=0)
c> at the pole in either hemisphere, so if the user's grid has its
c> origin at a point other than the pole, a translation is needed
c> to get i and j. The gridlines of i=constant are parallel to a
c> longitude designated by the user. the earth's radius is taken
c> to be 6371.2 km.
C>
C> @note This code is not vectorized. To vectorize take it and the
C> subroutine it calls and put them in line.
C>
C> @author James McDonell @date 1986-07-17
      SUBROUTINE W3FB04(ALAT,ALONG,XMESHL,ORIENT,XI,XJ)
C
      DATA  RADPD /.01745329/
      DATA  EARTHR/6371.2/
C
      RE    = (EARTHR * 1.86603) / XMESHL
      XLAT  = ALAT * RADPD
C
      IF (XMESHL.GE.0.) THEN
        WLONG = (ALONG + 180.0 - ORIENT) * RADPD
        R     = (RE * COS(XLAT)) / (1.0 + SIN(XLAT))
        XI    =   R * SIN(WLONG)
        XJ    =   R * COS(WLONG)
      ELSE
        RE    = -RE
        XLAT  = -XLAT
        WLONG = (ALONG - ORIENT) * RADPD
        R     = (RE * COS(XLAT)) / (1.0 + SIN(XLAT))
        XI    =   R * SIN(WLONG)
        XJ    =  -R * COS(WLONG)
      ENDIF
C
      RETURN
      END
