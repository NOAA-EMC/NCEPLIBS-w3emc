C> @file
C> @brief COnvert s. hemisphere lat/lon to i and j.
C> @author Ralph Jones @date 1985-09-13

C> Computes i and j coordinates for a latitude/longitude
C> point on the southern hemisphere polar stereographic map
C> projection.
C>
C> Program history log:
C> - Ralph Jones 1985-09-13 Convert to fortran 77.
C> - Ralph Jones 1990-08-31 Convert to cray cft77 fortran.
C>
C> @param[in] ALAT Real*4 latitude (s.h. latitudes are negative)
C> @param[in] ALONG Real*4 west longitude.
C> @param[in] XMESHL Real*4 grid interval in km.
C> @param[out] XI Real*4 i coordinate.
C> @param[out] XJ Real*4 j coordinate.
C>
C> @author Ralph Jones @date 1985-09-13
      SUBROUTINE W3FB02(ALAT, ALONG, XMESHL, XI, XJ)
C
C     ...GIVEN ... ALAT   SRN HEMI LATS ARE NEGATIVE VALUED
C                  ALONG   IN DEGREES  WEST LONGITUDE
C                  XMESHL= GRID INTERVAL IN KM,  E.G., 381.0 KM
C     ...TO COMPUTE XI,XJ FOR A PT ON THE SRN HEMI POLAR STEREOGRAPHIC
C     ...   PROJECTION, WITH 80W LONGITUDE VERTICAL AT THE TOP OF  MAP,
C     ...   AND 100E LONGITUDE VERTICAL AT THE BOTTOM OF THE MAP.
C     ...THE RESULTING XI AND XJ ARE RELATIVE TO  (0,0) AT SOUTH POLE.
C
       DATA  ADDLNG/80.0/
C
C     ...WHICH IS DIFFERENCE BETWEEN 180 DEGREES AND VERTICAL MERIDIAN.
C     ...   THE VERTICAL BEING 100 WEST AFTER CHANGING THE SENSE
C
       DATA  TINY  /0.00001/
       DATA  EARTHR/6371.2/
       DATA  CONVT /0.017453293/
C
C     ...WHICH CONVERTS DEGREES TO RADIANS
C
      RE   = (EARTHR * 1.86603) / XMESHL
C
C     ...WHICH IS DISTANCE IN GRID INTERVALS FROM POLE TO  EQUATOR
C
      XLAT = -ALAT * CONVT
C
C     ...WHERE NEGATIVE ALATS WERE GIVEN FOR SRN HEMI
C
      WLONG = 360.0 - ALONG
      WLONG = (WLONG + ADDLNG) * CONVT
      R     = (RE * COS(XLAT))/(1.0 + SIN(XLAT))
      XI    = R * SIN(WLONG)
      IF (ABS(XI) .LT. TINY) XI = 0.0
      XJ    = R * COS (WLONG)
      IF (ABS(XJ) .LT. TINY) XJ = 0.0
      RETURN
      END
