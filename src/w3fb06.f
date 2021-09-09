C> @file
C> @brief Lat/lon to pola (i,j) for grib.
C> @author John Stackpole @date 1988-01-01

C> Converts the coordinates of a location on earth given in
C> the natural coordinate system of latitude/longitude to a grid
C> coordinate system overlaid on a polar stereographic map pro-
C> jection true at 60 degrees n or s latitude. w3fb06() is the reverse
C> of w3fb07(). uses grib specification of the location of the grid
C>
C> Program history log:
C> - John Stackpole 1988-01-01
C> - Ralph Jones 1990-04-12 Convert to cray cft77 fortran.
C>
C> @param[in] ALAT Latitude in degrees (negative in southern hemis)
C> @param[in] ALON East longitude in degrees, real*4
C> @param[in] ALAT1 Latitude of lower left point of grid (point (1,1))
C> @param[in] ALON1 Longitude of lower left point of grid (point (1,1))
C> all real*4
C> @param[in] DX Mesh length of grid in meters at 60 deg lat
C> must be set negative if using
C> southern hemisphere projection.
C> 190500.0 lfm grid,
C> 381000.0 nh pe grid, -381000.0 sh pe grid, etc.
C> @param[in] ALONV The orientation of the grid.  i.e.,
C> the east longitude value of the vertical meridian
C> which is parallel to the y-axis (or columns of
C> of the grid)along which latitude increases as
C> the y-coordinate increases.  real*4
C> for example:
C> 255.0 for lfm grid,
C> 280.0 nh pe grid, 100.0 sh pe grid, etc.
C> @param[out] XI I Coordinate of the point specified by alat, alon.
C> @param[out] XJ J Coordinate of the point; both real*4.
C>
C> @note Formulae and notation loosely based on hoke, hayes,
C> and renninger's "map projections and grid systems...", march 1981
C> afgwc/tn-79/003
C>
C> @author John Stackpole @date 1988-01-01
      SUBROUTINE W3FB06(ALAT,ALON,ALAT1,ALON1,DX,ALONV,XI,XJ)
C
         DATA  RERTH /6.3712E+6/, PI/3.1416/
         DATA  SS60  /1.86603/
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
C        H = 1 FOR NORTHERN HEMISPHERE; = -1 FOR SOUTHERN
C
C        REFLON IS LONGITUDE UPON WHICH THE POSITIVE X-COORDINATE
C        DRAWN THROUGH THE POLE AND TO THE RIGHT LIES
C        ROTATED AROUND FROM ORIENTATION (Y-COORDINATE) LONGITUDE
C        DIFFERENTLY IN EACH HEMISPHERE
C
         IF (DX.LT.0) THEN
           H      = -1.0
           DXL    = -DX
           REFLON = ALONV - 90.0
         ELSE
           H      = 1.0
           DXL    = DX
           REFLON = ALONV - 270.0
         ENDIF
C
         RADPD  = PI / 180.0
         REBYDX = RERTH/DXL
C
C        RADIUS TO LOWER LEFT HAND (LL) CORNER
C
         ALA1 =  ALAT1 * RADPD
         RMLL = REBYDX * COS(ALA1) * SS60/(1. + H * SIN(ALA1))
C
C        USE LL POINT INFO TO LOCATE POLE POINT
C
         ALO1  = (ALON1 - REFLON) * RADPD
         POLEI = 1. - RMLL * COS(ALO1)
         POLEJ = 1. - H * RMLL * SIN(ALO1)
C
C        RADIUS TO DESIRED POINT AND THE I J TOO
C
         ALA = ALAT   * RADPD
         RM  = REBYDX * COS(ALA) * SS60/(1. + H * SIN(ALA))
C
         ALO = (ALON - REFLON) * RADPD
         XI  = POLEI + RM * COS(ALO)
         XJ  = POLEJ + H * RM * SIN(ALO)
C
      RETURN
      END
