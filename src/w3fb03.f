C> @file
C> @brief Convert i,j grid coordinates to lat/lon.
C> @author Ralph Jones @date 1986-07-17

C> Converts i,j grid coordinates to the corresponding
C> latitude/longitude on a southern hemisphere polar stereographic
C> map projection.
C>
C> Program history log.
C> - Ralph Jones 1986-07-17 Convert to fortran 77.
C> - Ralph Jones 1990-08-31 Convert to cray cft77 fortran.
C>
C> @param[in] XI Real i coordinate.
C> @param[in] XJ Real j coordinate.
C> @param[in] XMESHL Real grid interval in km.
C> @param[out] TLAT Real s.h. latitude.
C> @param[out] TLONG Real longitude.
C>
C> @author Ralph Jones @date 1986-07-17
      SUBROUTINE W3FB03(XI, XJ, XMESHL, TLAT, TLONG)
C
C     ...GIVEN ... XI/XJ GRID COORDINATES OF A POINT RELATIVE
C     ...        TO (0,0) AT SOUTH POLE
C     ...TO COMPUTE TLAT,TLONG ON THE SRN HEMI POLAR STEREO PROJECTION
C     ...WITH 80W VERTICAL AT TOP OF THE MAP
C
       DATA  DEGPRD/57.2957795/
       DATA  EARTHR/6371.2/
C
      RE  = (EARTHR * 1.86603) / XMESHL
      GI2 = RE * RE
C
C     ...WHERE GI2 IS THE SQUARE OF DISTANCE IN GRID INTERVALS
C     ...   FROM POLE TO EQUATOR...
C
      R2 = XI * XI + XJ * XJ
      IF (R2 .NE. 0.0) THEN
C
      XLONG = DEGPRD * ATAN2(XJ,XI)
      TLONG = XLONG - 10.0
      IF (TLONG .LT. 0.0) TLONG = TLONG + 360.0
      TLAT  = ASIN((GI2 - R2)/(GI2 + R2)) * DEGPRD
      TLAT  = -TLAT
C
      ELSE
      TLAT = -90.0
C
C     ...FOR SOUTH POLE...
C
      TLONG = 0.0
      ENDIF
C
      RETURN
      END
