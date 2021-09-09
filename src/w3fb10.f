C> @file
C> @brief Lat/long pair to compass bearing, gcd.
C> @author Peter Chase @date 1988-08-29

C> Given a pair of points (1) and (2) given by latitude and
C> longitude, w3fb10() computes the bearing and great circle distance
C> from point (1) to point (2) assuming a spherical Earth. The
C> north and south poles are special cases. If latitude of point
C> (1) is within 1e-10 degrees of the north pole, bearing is the
C> negative longitude of point (2) by convention. If latitude of
C> point (1) is within 1e-10 degrees of the south pole, bearing is
C> the longitude of point (2) by convention. If point (2) is within
C> 1e-6 radians of the antipode of point (1), the bearing will be
C> set to zero. If point (1) and point (2) are within 1e-10 radians
C> of each other, both bearing and distance will be set to zero.
C>
C> Program history log:
C> - Peter Chase 1988-08-29
C> - Peter Chase 1988-09-23 Fix dumb south pole error.
C> - Peter Chase 1988-10-05 Fix bearing ambiguity.
C> - Ralph Jones 1990-04-12 Convert to cft77 fortran.
C>
C> @param[in] DLAT1 REAL Latitude of point (1) in degrees north.
C> @param[in] DLON1 REAL Longitude of point (1) in degrees east.
C> @param[in] DLAT2 REAL Latitude of point (2) in degrees north.
C> @param[in] DLON2 REAL Longitude of point (2) in degrees east.
C> @param[out] BEARD REAL Bearing of point (2) from point (1) in
C> compass degrees with north = 0.0, values from
C> -180.0 to +180.0 degrees.
C> @param[out] GCDKM REAL Great circle distance from point (1) to
C> point (2) in kilometers.
C>
C> @note According to the nmc handbook, the Earth's radius is
C> 6371.2 kilometers. This is what we use, even though the value
C> recommended by the smithsonian meteorological handbook is
C> 6371.221 km. (I wouldn't want you to think that I didn't know
C> what the correct value was.)
C>
C> @note Method: The poles are special cases, and handled separately.
C> otherwise, from spherical trigonometry, the law of cosines is used
C> to calculate the third side of the spherical triangle having
C> sides from the pole to points (1) and (2) (the colatitudes).
C> then the law of sines is used to calculate the angle at point
C> (1). A test is applied to see whether the arcsine result may be
C> be used as such, giving an acute angle as the bearing, or whether
C> the arcsine result should be subtracted from pi, giving an obtuse
C> angle as the bearing. This test is derived by constructing a
C> right spherical triangle using the pole, point (2), and the
C> meridian through point(1). The latitude of the right-angled
C> vertex then provides a test--if latitude (1) is greater than this
C> latitude, the bearing angle must be obtuse, otherwise acute.
C> If the two points are within 1e-6 radians of each other
C> a flat Earth is assumed, and the four-quadrant arctangent
C> function is used to find the bearing. The y-displacement is
C> the difference in latitude and the x-displacement is the
C> difference in longitude times cosine latitude, both in radians.
C> distance is then the diagonal.
C>
C> @note Fundamental trigonometric identities are used freely, such
C> as that cos(x) = sin(pi/2 - x), etc.  See almost any mathematical
C> handbook, such as the c.r.c. standard math tables under 'relations
C> in any spherical triangle', or the national bureau of standards
C> 'handbook of mathematical functions' under section 4.3.149,
C> formulas for solution of spherical triangles.
C>
C> @note Double precision is used internally because of the wide
C> range of geographic values that may be used.
C>
C> @author Peter Chase @date 1988-08-29
      SUBROUTINE W3FB10(DLAT1, DLON1, DLAT2, DLON2, BEARD, GCDKM)
C
C *** IMPLICIT TYPE DEFAULTS.....
C
      IMPLICIT REAL  (A-H,O-Z)
C
C *** CONSTANTS......
C
      REAL   PI
      REAL   HALFPI
      REAL   DR
      REAL   RD
      REAL   TDEG, TRAD, TPOD, TFLT
      REAL   EARTHR
      REAL   WHOLCD, HALFCD, QUARCD
C
C *** VARIABLES......
C
      REAL   RLAT1,  RLAT2,  COSLA1, COSLA2, SINLA1, SINLA2
      REAL   DLOND,  RLOND,  COSLO,  SINLO,  SANGG,  ABEAR
      REAL   YDISP,  XDISP,  DDLAT1, DDLAT2, DBANG
      REAL   DLAT1,  DLAT2,  DLON1,  DLON2,  BEARD,  GCDKM
C
C *** CONVERT LATITUDES AND LONGITUDE DIFFERENCE TO RADIANS.
C
      DATA   PI    /3.141592653589793238462643/
      DATA   HALFPI/1.570796326794896619231322/
      DATA   DR    /0.017453292519943295769237/
      DATA   RD    /57.295779513082320876798155/
      DATA   TDEG  /1E-10/, TRAD/1E-10/, TPOD/1E-6/, TFLT/1E-6/
      DATA   EARTHR/6371.2/
      DATA   WHOLCD/360.0/, HALFCD/180.0/, QUARCD/90.0/

      DDLAT1 = DLAT1
      DDLAT2 = DLAT2
      RLAT1  = DR    * DDLAT1
      RLAT2  = DR    * DDLAT2
      DLOND  = DLON2 - DLON1
      IF (DLOND .GT. HALFCD)  DLOND = DLOND - WHOLCD
      IF (DLOND .LT. -HALFCD) DLOND = DLOND + WHOLCD
      RLOND = DR * DLOND
C
C *** FIRST WE ATTACK THE CASES WHERE POINT 1 IS VERY CLOSE TO THE
C *** NORTH OR SOUTH POLES.
C *** HERE WE USE CONVENTIONAL VALUE FOR BEARING.. - LONG (2) AT THE
C *** NORTH POLE, AND + LONG (2) AT THE SOUTH POLE.
C
      IF (ABS(DDLAT1-QUARCD) .LT. TDEG) THEN
        IF (ABS(DDLAT2-QUARCD) .LT. TDEG) THEN
          DBANG = 0.0
          SANGG = 0.0
        ELSE IF (ABS(DDLAT2+QUARCD) .LT. TDEG) THEN
          DBANG = 0.0
          SANGG = PI
        ELSE
          DBANG = -DLON2
          SANGG = HALFPI - RLAT2
        ENDIF
      ELSE IF (ABS(DDLAT1+QUARCD) .LT. TDEG) THEN
        IF (ABS(DDLAT2-QUARCD) .LT. TDEG) THEN
          DBANG = 0.0
          SANGG = PI
        ELSE IF (ABS(DDLAT2+QUARCD) .LT. TDEG) THEN
          DBANG = 0.0
          SANGG = 0.0
        ELSE
          DBANG = +DLON2
          SANGG = HALFPI + RLAT2
        ENDIF
C
C *** NEXT WE ATTACK THE CASES WHERE POINT 2 IS VERY CLOSE TO THE
C *** NORTH OR SOUTH POLES.
C *** HERE BEARING IS SIMPLY 0 OR 180 DEGREES.
C
      ELSE IF (ABS(DDLAT2-QUARCD) .LT. TDEG) THEN
        DBANG = 0.0
        SANGG = HALFPI - RLAT1
      ELSE IF (ABS(DDLAT2+QUARCD) .LT. TDEG) THEN
        DBANG = HALFCD
        SANGG = HALFPI + RLAT1
C
C *** THE CASE REMAINS THAT NEITHER POINT IS AT EITHER POLE.
C *** FIND COSINE AND SINE OF LATITUDES AND LONGITUDE DIFFERENCE
C *** SINCE THEY ARE USED IN MORE THAN ONE FORMULA.
C
      ELSE
        COSLA1 = COS(RLAT1)
        SINLA1 = SIN(RLAT1)
        COSLA2 = COS(RLAT2)
        SINLA2 = SIN(RLAT2)
        COSLO  = COS(RLOND)
        SINLO  = SIN(RLOND)
C
C *** FOLLOWING IS FORMULA FOR GREAT CIRCLE SUBTENDED ANGLE BETWEEN
C *** POINTS IN RADIAN MEASURE.
C
        SANGG = ACOS(SINLA1*SINLA2 + COSLA1*COSLA2*COSLO)
C
C *** IF THE GREAT CIRCLE SUBTENDED ANGLE IS VERY SMALL, FORCE BOTH
C *** BEARING AND DISTANCE TO BE ZERO.
C
        IF (ABS(SANGG) .LT. TRAD) THEN
          DBANG = 0.0
          SANGG = 0.0
C
C *** IF THE GREAT CIRCLE SUBTENDED ANGLE IS JUST SMALL, ASSUME A
C *** FLAT EARTH AND CALCULATE Y- AND X-DISPLACEMENTS.  THEN FIND
C *** BEARING USING THE ARCTANGENT FUNCTION AND DISTANCE USING THE
C *** SQUARE ROOT.
C
        ELSE IF (ABS(SANGG) .LT. TFLT) THEN
          YDISP = RLAT2-RLAT1
          XDISP = RLOND*COSLA2
          ABEAR = ATAN2(XDISP, YDISP)
          DBANG = RD*ABEAR
          SANGG = SQRT(YDISP**2 + XDISP**2)
C
C *** IF THE ANGLE IS RATHER CLOSE TO PI RADIANS, FORCE BEARING TO
C *** BE ZERO AND DISTANCE TO BE PI.
C *** THE TEST FOR 'CLOSE TO PI' IS MORE RELAXED THAN THE TEST FOR
C *** 'CLOSE TO ZERO' TO ALLOW FOR GREATER RELATIVE ERROR.
C
        ELSE IF (ABS(SANGG-PI) .LT. TPOD) THEN
          DBANG = 0.0
          SANGG = PI
C
C *** OTHERWISE COMPUTE THE PRINCIPAL VALUE OF THE BEARING ANGLE
C *** USING THE LAW OF SINES.  THE DIVISION BY THE SINE FORCES US TO
C *** LIMIT THE DOMAIN OF THE ARCSINE TO (-1,1).
C
        ELSE
          ABEAR = ASIN(AMAX1(-1.0,AMIN1(+1.0,COSLA2*SINLO/
     &            SIN(SANGG))))
C
C *** IF THE LONGITUDE DIFFERENCE IS LESS THAN PI/2 IT IS NECESSARY
C *** TO CHECK WHETHER THE BEARING ANGLE IS ACUTE OR OBTUSE BY
C *** COMPARING LATITUDE (1) WITH THE LATITUDE OF THE GREAT CIRCLE
C *** THROUGH POINT (2) NORMAL TO MERIDIAN OF LONGITUDE (1).  IF
C *** LATITUDE (1) IS GREATER, BEARING IS OBTUSE AND THE ACTUAL
C *** BEARING ANGLE IS THE SUPPLEMENT OF THE ANGLE CALCULATED ABOVE.
C
          IF (0.0 .LE. COSLA1*SINLA2 .AND. COSLA1*SINLA2 .LE.
     &    COSLA2*SINLA1*COSLO .OR. COSLA1*SINLA2 .LE. 0.0 .AND.
     &    COSLA2*SINLA1*COSLO .GE. COSLA1*SINLA2) ABEAR =
     &    SIGN(PI,ABEAR) - ABEAR
          DBANG = RD * ABEAR
        ENDIF
      ENDIF
C
C *** THIS FINISHES THE CASE WHERE POINTS ARE NOT AT THE POLES.
C *** NOW CONVERT BEARING TO DEGREES IN RANGE -180 TO +180 AND FIND
C *** GREAT CIRCLE DISTANCE IN KILOMETERS.
C
      IF (DBANG .LE. -HALFCD) DBANG = DBANG + WHOLCD
      IF (DBANG .GT. HALFCD)  DBANG = DBANG - WHOLCD
      GCDKM = EARTHR * SANGG
      BEARD = DBANG
      RETURN
      END
