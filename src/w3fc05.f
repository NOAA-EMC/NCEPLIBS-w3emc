C> @file
C> @brief Earth U,V wind components to dir and spd.
C> @author John Stackpole @date 1981-12-30

C> Given the true (Earth oriented) wind components
C> compute the wind direction and speed.
C> Input winds at the pole are assumed to follow the WMO
C> conventions, with the output direction computed in accordance
C> with WMO standards for reporting winds at the pole.
C> (see office note 241 for WMO definition.)
C>
C> Program history log:
C> - John Stackpole 1981-12-30
C> - P. Chase 1988-10-19 Allow output values to overlay input
C> - Ralph Jones 1991-03-05 Changes for cray cft77 fortran
C> - Dennis Keyser 1992-10-21 Added 1.e-3 to direction to allow truncation
C> to nearest whole degree to be correct (keeps agreement between cray & nas versions)
C>
C> @param[in] U REAL Earth-oriented U-component.
C> @param[in] V REAL Earth-oriented V-component.
C> @param[out] DIR REAL Wind direction, degrees. Values will
C> be from 0 to 360 inclusive.
C> @param[out] SPD REAL Wind speed in same units as input.
C>
C> @note If speed is less than 1e-10 then direction will be set to zero.
C>
C> @author John Stackpole @date 1981-12-30
      SUBROUTINE W3FC05(U, V, DIR, SPD)                                                                                                    11700000
C
C     VARIABLES.....
C
      REAL   U, V, DIR, SPD, XSPD
C
C     CONSTANTS.....
C
      DATA  SPDTST/1.0E-10/
      DATA  RTOD  /57.2957795/
      DATA  DCHALF/180.0/
C
      XSPD = SQRT(U * U + V * V)
      IF (XSPD .LT. SPDTST) THEN
         DIR = 0.0
      ELSE
         DIR = ATAN2(U,V) * RTOD + DCHALF + 1.E-3
      ENDIF
      SPD = XSPD
      RETURN
      END
