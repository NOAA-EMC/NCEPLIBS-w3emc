C> @file
C> @brief I,J TO LATITUDE, LONGITUDE
C> @author A. Heermann @date 1969-08-01

C> Converts the coordinates of a location from the grid(i,j)
C> coordinate system overlaid on the polar stereographic map pro-
C> jection true at 60 n to the natural coordinate system of latitude
C> /longitude on the Earth. A preferable more flexible subroutine to
C> use is w3fb05(). w3fb01() is the reverse of w3fb00().
C>
C> PROGRAM HISTORY LOG:
C> - A. Heermann 1969-08-01  A. HEERMANN
C> - Ralph Jones 1990-08-31 Change to cray cft77 fortran.
C>
C> @param[in] XI I of the point relative to north pole.
C> @param[in] XJ J of the point relative to north pole.
C> @param[in] XMESHL Mesh length of grid in kilometers at 60n.
C> @param[out] ALAT Latitude in deg. (-20.0(s. hemis) < alat < 90.0).
C> @param[out] ALONG West longitude in degrees.
C>
C> @note The grid used in this subroutine has its origin (i=0,j=0)
C> at the north pole, so if the user's grid has its origin at a
C> point other than the north pole, a translation is required to
C> get i and j for input into w3fb01(). The subroutine grid is
C> oriented so that longitude 80w is parallel to gridlines of
C> i=constant. The Earth's radius is taken to be 6371.2 km.
C> All parameters in the call statement must be real.
C>
C> @author A. Heermann @date 1969-08-01
      SUBROUTINE W3FB01(XI,XJ,XMESHL,ALAT,ALONG)
C
      DATA  DEGPRD/57.2957795/
      DATA  EARTHR/6371.2/
C
      GI2 = (1.86603 * EARTHR) / XMESHL
      GI2 = GI2 * GI2
      R2  = XI * XI + XJ * XJ
      IF (R2.NE.0.0)  GO TO 100
      ALONG = 0.0
      ALAT = 90.0
      RETURN
C
100   CONTINUE
      ALAT = ASIN((GI2-R2) / (GI2+R2)) * DEGPRD
      XLONG = DEGPRD * ATAN2(XJ,XI)
      IF (XLONG) 200,300,300
C
200   CONTINUE
      ALONG = -10.0 - XLONG
      IF (ALONG.LT.0.0)  ALONG = ALONG + 360.0
      GO TO 400
C
300   CONTINUE
      ALONG = 350.0 - XLONG
C
400   CONTINUE
      RETURN
        END
