C> @file
C> @brief Wind dir and spd to Earth U,V components.
C> @author John Stackpole @date 1981-12-30

C> Given the wind direction and speed,
C> compute Earth-oriented (true) wind components.
C> Input direction at the pole point
C> must be consistent with WMO conventions, and output components
C> will follow those conventions.
C> (See office note 241 for WMO definition.)
C>
C> Program history log:
C> - John Stackpole 1981-12-30
C> - Ralph Jones 1991-03-06 Change to cray cft77 fortran.
C>
C> @param[in] DIR REAL*4 Wind direction, degrees
C> @param[in] SPD REAL*4 Wind speed, any units
C> @param[out] U REAL*4 Earth-oriented U-component.
C> @param[out] V REAL*4 Earth-oriented V-component.
C>
C> @note This code will not vectorize on cray, you could
C> put the four lines in your code with a couple of
C> do loops.
C>
C> @author John Stackpole @date 1981-12-30
      SUBROUTINE W3FC06(DIR,SPD,U,V)
C
         XSPD = -SPD
         DIRL = 0.0174533 * DIR
         U    = XSPD * SIN(DIRL)
         V    = XSPD * COS(DIRL)
C
      RETURN
      END
