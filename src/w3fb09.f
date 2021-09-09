C> @file
C> @brief Merc (i,j) to lat/lon for grib
C> @author John Stackpole @date 1988-03-01

C> Converts a location on Earth given in
C> an i,j coordinate system overlaid on a mercator map projection
C> to the coordinate system of latitude/longitude
C> w3fb09() is the reverse of w3fb08()
C> uses grib specification of the location of the grid.
C>
C> Program history log:
C> - John Stackpole 1988-03-01
C> - Ralph Jones 1990-04-12 Convert to cray cft77 fortran.
C>
C> @param[in] XI I coordinate of the point.
C> @param[in] XJ J coordinate of the point; both real*4.
C> @param[in] ALAT1 Latitude  of lower left corner of grid (point (1,1)).
C> @param[in] ALON1 Longitude of lower left corner of grid (point (1,1))
C> all real*4.
C> @param[in] ALATIN The latitude at which the mercator cylinder
C> intersects the Earth.
C> @param[in] DX Mesh length of grid in meters at alatin.
C> @param[out] ALAT Latitude in degrees (negative in southern hemis).
C> @param[out] ALON East longitude in degrees, real*4
C> of the point specified by (i,j).
C>
C> @note Formulae and notation loosely based on hoke, hayes,
C> and renninger's "map projections and grid systems...", march 1981
C> afgwc/tn-79/003
C>
C> @author John Stackpole @date 1988-03-01
      SUBROUTINE W3FB09(XI,XJ,ALAT1,ALON1,ALATIN,DX,ALAT,ALON)
C
         DATA  RERTH /6.3712E+6/, PI/3.1416/
C
C        PRELIMINARY VARIABLES AND REDIFINITIONS
C
         RADPD  = PI    / 180.0
         DEGPR  = 180.0 / PI
         CLAIN  = COS(RADPD*ALATIN)
         DELLON = DX   / (RERTH*CLAIN)
C
C        GET DISTANCE FROM EQUATOR TO ORIGIN ALAT1
C
         DJEO = 0.
         IF (ALAT1.NE.0.)
     &     DJEO = (ALOG(TAN(0.5*((ALAT1+90.0)*RADPD))))/DELLON
C
C        NOW THE LAT AND LON
C
         ALAT = 2.0*ATAN(EXP(DELLON*(DJEO + XJ-1.)))*DEGPR - 90.0
         ALON = (XI-1.) * DELLON * DEGPR + ALON1
C
      RETURN
      END
