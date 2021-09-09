C> @file
C> @brief Lambert(i,j) to lat/lon for grib.
C> @author John Stackpole @date 1988-11-25

C> Converts the coordinates of a location on Earth given in a
C> grid coordinate system overlaid on a lambert conformal tangent
C> cone projection true at a given N or S latitude to the
C> natural coordinate system of latitude/longitude
C> w3fb12() is the reverse of w3fb11().
C> Uses grib specification of the location of the grid
C>
C> PROGRAM HISTORY LOG:
C> - John Stackpole 1988-11-25
C> - Ralph Jones 1990-04-12 Convert to cft77 fortran.
C> - Ralph Jones 1994-04-28 Add save statement.
C>
C> @param[in] XI I coordinate of the point  real*4
C> @param[in] XJ J coordinate of the point  real*4
C> @param[in] ALAT1 Latitude of lower left point of grid (point 1,1)
C> latitude <0 for southern hemisphere; real*4
C> @param[in] ELON1 Longitude of lower left point of grid (point 1,1)
C> east longitude used throughout; real*4
C> @param[in] DX Mesh length of grid in meters at tangent latitude
C> @param[in] ELONV The orientation of the grid.  i.e.,
C> the east longitude value of the vertical meridian
C> which is parallel to the y-axis (or columns of
C> the grid) along which latitude increases as
C> the y-coordinate increases.  real*4
C> this is also the meridian (on the other side of the
C> tangent cone) along which the cut is made to lay
C> the cone flat.
C> @param[in] ALATAN The latitude at which the lambert cone is tangent to
C> (touches or osculates) the spherical earth.
C> set negative to indicate a
C> southern hemisphere projection; real*4
C>
C> @param[out] ALAT Latitude in degrees (negative in southern hemi.)
C> @param[out] ELON East longitude in degrees, real*4
C> @param[out] IERR
C> - .eq. 0   if no problem
C> - .ge. 1   if the requested xi,xj point is in the
C> forbidden zone, i.e. off the lambert map
C> in the open space where the cone is cut.
C> - if ierr.ge.1 then alat=999. and elon=999.
C>
C> @note Formulae and notation loosely based on hoke, hayes,
C> and renninger's "map projections and grid systems...", march 1981
C> afgwc/tn-79/003
C>
C> @author John Stackpole @date 1988-11-25
      SUBROUTINE W3FB12(XI,XJ,ALAT1,ELON1,DX,ELONV,ALATAN,ALAT,ELON,
     &                               IERR)
C
         LOGICAL NEWMAP
C
         SAVE
C
         DATA  RERTH /6.3712E+6/, PI/3.14159/, OLDRML/99999./
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
         PIBY2  = PI     / 2.0
         RADPD  = PI     / 180.0
         DEGPRD = 1.0    / RADPD
         REBYDX = RERTH  / DX
         ALATN1 = ALATAN * RADPD
         AN     = H * SIN(ALATN1)
         COSLTN = COS(ALATN1)
C
C        MAKE SURE THAT INPUT LONGITUDE DOES NOT PASS THROUGH
C        THE CUT ZONE (FORBIDDEN TERRITORY) OF THE FLAT MAP
C        AS MEASURED FROM THE VERTICAL (REFERENCE) LONGITUDE
C
         ELON1L = ELON1
         IF ((ELON1-ELONV).GT.180.)
     &     ELON1L = ELON1 - 360.
         IF ((ELON1-ELONV).LT.(-180.))
     &     ELON1L = ELON1 + 360.
C
         ELONVR = ELONV * RADPD
C
C        RADIUS TO LOWER LEFT HAND (LL) CORNER
C
         ALA1 =  ALAT1 * RADPD
         RMLL = REBYDX * ((COSLTN**(1.-AN))*(1.+AN)**AN) *
     &           (((COS(ALA1))/(1.+H*SIN(ALA1)))**AN)/AN
C
C        USE RMLL TO TEST IF MAP AND GRID UNCHANGED FROM PREVIOUS
C        CALL TO THIS CODE.  THUS AVOID UNNEEDED RECOMPUTATIONS.
C
         IF (RMLL.EQ.OLDRML) THEN
           NEWMAP = .FALSE.
         ELSE
           NEWMAP = .TRUE.
           OLDRML = RMLL
C
C          USE LL POINT INFO TO LOCATE POLE POINT
C
           ELO1 = ELON1L * RADPD
           ARG = AN * (ELO1-ELONVR)
           POLEI = 1. - H * RMLL * SIN(ARG)
           POLEJ = 1. + RMLL * COS(ARG)
         ENDIF
C
C        RADIUS TO THE I,J POINT (IN GRID UNITS)
C              YY REVERSED SO POSITIVE IS DOWN
C
         XX = XI - POLEI
         YY = POLEJ - XJ
         R2 = XX**2 + YY**2
C
C        CHECK THAT THE REQUESTED I,J IS NOT IN THE FORBIDDEN ZONE
C           YY MUST BE POSITIVE UP FOR THIS TEST
C
         THETA = PI*(1.-AN)
         BETA = ABS(ATAN2(XX,-YY))
         IERR = 0
         IF (BETA.LE.THETA) THEN
           IERR = 1
           ALAT = 999.
           ELON = 999.
           IF (.NOT.NEWMAP)  RETURN
         ENDIF
C
C        NOW THE MAGIC FORMULAE
C
         IF (R2.EQ.0) THEN
           ALAT = H * 90.0
           ELON = ELONV
         ELSE
C
C          FIRST THE LONGITUDE
C
           ELON = ELONV + DEGPRD * ATAN2(H*XX,YY)/AN
           ELON = AMOD(ELON+360., 360.)
C
C          NOW THE LATITUDE
C          RECALCULATE THE THING ONLY IF MAP IS NEW SINCE LAST TIME
C
           IF (NEWMAP) THEN
             ANINV = 1./AN
             ANINV2 = ANINV/2.
             THING = ((AN/REBYDX) ** ANINV)/
     &         ((COSLTN**((1.-AN)*ANINV))*(1.+ AN))
           ENDIF
           ALAT = H*(PIBY2 - 2.*ATAN(THING*(R2**ANINV2)))*DEGPRD
         ENDIF
C
C        FOLLOWING TO ASSURE ERROR VALUES IF FIRST TIME THRU
C         IS OFF THE MAP
C
         IF (IERR.NE.0) THEN
           ALAT = 999.
           ELON = 999.
           IERR = 2
         ENDIF
         RETURN
         END
