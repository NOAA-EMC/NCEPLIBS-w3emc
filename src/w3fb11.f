C> @file
C> @brief Lat/lon to lambert(i,j) for grib.
C> @author John Stackpole @date 1988-11-25

C> Converts the coordinates of a location on Earth given in
C> the natural coordinate system of latitude/longitude to a grid
C> coordinate system overlaid on a lambert conformal tangent cone
C> projection true at a given n or s latitude. w3fb11() is the reverse
C> of w3fb12(). uses grib specification of the location of the grid
C>
C> Program history log:
C> - John Stackpole 1988-11-25
C> - Ralph Jones 1990-04-12 Convert to cft77 fortran.
C> - Ralph Jones 1994-04-28 Add save statement.
C>
C> @param[in] ALAT Latitude in degrees (negative in southern hemis).
C> @param[in] ELON East longitude in degrees, real*4.
C> @param[in] ALAT1 Latitude of lower left point of grid (point (1,1)).
C> @param[in] ELON1 Longitude of lower left point of grid (point (1,1))
C> all real*4.
C> @param[in] DX Mesh length of grid in meters at tangent latitude.
C> @param[in] ELONV The orientation of the grid. i.e.,
C> the east longitude value of the vertical meridian
C> which is parallel to the y-axis (or columns of
C> of the grid) along which latitude increases as
C> the y-coordinate increases. real*4
C> this is also the meridian (on the back side of the
C> tangent cone) along which the cut is made to lay
C> the cone flat.
C> @param[in] ALATAN The latitude at which the lambert cone is tangent to
C> (touching) the spherical Earth. Set negative to indicate a
C> southern hemisphere projection.
C> @param[out] XI I coordinate of the point specified by alat, elon
C> @param[out] XJ J coordinate of the point; both real*4
C>
C> @note Formulae and notation loosely based on hoke, hayes,
C> and renninger's "map projections and grid systems...", march 1981
C> afgwc/tn-79/003.
C>
C> @author John Stackpole @date 1988-11-25
      SUBROUTINE W3FB11(ALAT,ELON,ALAT1,ELON1,DX,ELONV,ALATAN,XI,XJ)
C
         SAVE
C
         DATA  RERTH /6.3712E+6/, PI/3.14159/
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
C        H = 1 FOR NORTHERN HEMISPHERE; = -1 FOR SOUTHERN
C
         IF (ALATAN.GT.0) THEN
           H = 1.
         ELSE
           H = -1.
         ENDIF
C
         RADPD  = PI    / 180.0
         REBYDX = RERTH / DX
         ALATN1 = ALATAN * RADPD
         AN     = H * SIN(ALATN1)
         COSLTN = COS(ALATN1)
C
C        MAKE SURE THAT INPUT LONGITUDES DO NOT PASS THROUGH
C        THE CUT ZONE (FORBIDDEN TERRITORY) OF THE FLAT MAP
C        AS MEASURED FROM THE VERTICAL (REFERENCE) LONGITUDE.
C
         ELON1L = ELON1
         IF ((ELON1 - ELONV).GT.180.)
     &     ELON1L = ELON1 - 360.
         IF ((ELON1 - ELONV).LT.(-180.))
     &     ELON1L = ELON1 + 360.
C
         ELONL = ELON
         IF ((ELON  - ELONV).GT.180.)
     &     ELONL  = ELON  - 360.
         IF ((ELON - ELONV).LT.(-180.))
     &     ELONL = ELON + 360.
C
         ELONVR = ELONV * RADPD
C
C        RADIUS TO LOWER LEFT HAND (LL) CORNER
C
         ALA1 =  ALAT1 * RADPD
         RMLL = REBYDX * (((COSLTN)**(1.-AN))*(1.+AN)**AN) *
     &           (((COS(ALA1))/(1.+H*SIN(ALA1)))**AN)/AN
C
C        USE LL POINT INFO TO LOCATE POLE POINT
C
         ELO1 = ELON1L * RADPD
         ARG = AN * (ELO1-ELONVR)
         POLEI = 1. - H * RMLL * SIN(ARG)
         POLEJ = 1. + RMLL * COS(ARG)
C
C        RADIUS TO DESIRED POINT AND THE I J TOO
C
         ALA =  ALAT * RADPD
         RM = REBYDX * ((COSLTN**(1.-AN))*(1.+AN)**AN) *
     &           (((COS(ALA))/(1.+H*SIN(ALA)))**AN)/AN
C
         ELO = ELONL * RADPD
         ARG = AN*(ELO-ELONVR)
         XI = POLEI + H * RM * SIN(ARG)
         XJ = POLEJ - RM * COS(ARG)
C
C        IF COORDINATE LESS THAN 1
C        COMPENSATE FOR ORIGIN AT (1,1)
C
         IF (XI.LT.1.)  XI = XI - 1.
         IF (XJ.LT.1.)  XJ = XJ - 1.
C
      RETURN
      END
