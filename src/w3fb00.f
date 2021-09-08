C> @file
C> @brief Convert latitude, longitude to i,j
C> @author A. Heermann @date 1969-08-01

C> Converts the coordinates of a location on earth from the
C> natural coordinate system of latitude/longitude to the grid (i,j)
C> coordinate system overlaid on the polar stereographic map pro-
C> jection true at 60 n. a preferable, more flexible subroutine to
C> use is w3fb04(). w3fb00() is the reverse of w3fb01().
C>
C> Program history log:
C> - A. Heermann 1969-08-01
C> - Ralph Jones 1990-08-31 Convert to cray cft77 fortran
C>
C> @param[in] ALAT Latitude in deg. (-20.0 (s. hemis)) alat) 90.0).
C> @param[in] ALONG West longitude in degrees.
C> @param[in] XMESHL Mesh length of grid in kilometers at 60n.
C> @param[out] XI I of the point relative to north pole.
C> @param[out] XJ J of the point relative to north pole.
C>
C> @note The grid used in this subroutine has its origin (i=0,j=0)
C> at the north pole, so if the user's grid has its origin at a
C> point other than the north pole, a translation is required to
C> get i and j. The subroutine grid is oriented so that longitude
C> 80w is parallel to the gridlines of i=constant. The radius of
C> the earth is taken to be 6371.2 km. All parameters in the call statement
C> must be real this code will not vectorize on a cray. You will have put
C> it line to vectorize it.
C>
C> @author A. Heermann @date 1969-08-01
      SUBROUTINE W3FB00(ALAT,ALONG,XMESHL,XI,XJ)
C
      DATA  RADPD /.01745329/
      DATA  EARTHR/6371.2/
C
      RE    = (EARTHR * 1.86603) / XMESHL
      XLAT  = ALAT * RADPD
      SINL  = SIN(XLAT)
      WLONG = (ALONG + 100.0) * RADPD
      R     = (RE * COS(XLAT)) / (1. + SINL)
      XI    = R * SIN(WLONG)
      XJ    = R * COS(WLONG)
      RETURN
      END
