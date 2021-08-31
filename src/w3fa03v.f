C> @file
C> @brief Compute standard height, temp, and pot temp.
C> @author James McDonell @date 1974-06-01
C>
C> Computes the standard height, temperature, and potential
C> temperature given the pressure in millibars (>8.68 mb). For
C> height and temperature the results duplicate the values in the
C> U.S. standard atmosphere (l962), which is the icao standard
C> atmosphere to 54.7487 mb (20 km) and the proposed extension to
C> 8.68 mb (32 km). For potential temperature a value of 2/7 is
C> used for rd/cp.
C>
C> Program history log:
C> - James McDonell 1974-06-01
C> - Ralph Jones 1984-06-01 Change to ibm vs fortran.
C> - Dennis Keyser 1992-06-29 Convert to cray cft77 fortran.
C> - Ralph Jones 1994-09-13 Vectorized version to do array instead of one word.
C>
C> @param[in] PRESS Pressure array in millibars.
C> @param[out] HEIGHT Height array in meters.
C> @param[out] TEMP Temperature array in degrees kelvin.
C> @param[out] THETA Potential temperature array in degrees kelvin.
C> @param[out] N Number of points in array press.
C>
C> @note Not valid for pressures less than 8.68 millibars, declare
C> all parameters as type real.
C>
C> @note Height, temp, theta are now all arrays, you must
C> have arrays of size n or you will wipe out memory.
C>
C> @author James McDonell @date 1974-06-01
      SUBROUTINE W3FA03V(PRESS,HEIGHT,TEMP,THETA,N)
C
      REAL  M0
      REAL  HEIGHT(*)
      REAL  PRESS(*)
      REAL  TEMP(*)
      REAL  THETA(*)
C
      SAVE
C
      DATA  G/9.80665/,RSTAR/8314.32/,M0/28.9644/,PISO/54.7487/,
     $ ZISO/20000./,SALP/-.0010/,PZERO/1013.25/,T0/288.15/,ALP/.0065/,
     $ PTROP/226.321/,TSTR/216.65/
C
      ROVCP = 2.0/7.0
      R     = RSTAR/M0
      ROVG  = R/G
      FKT   = ROVG * TSTR
      AR    = ALP  * ROVG
      PP0   = PZERO**AR
      AR1   = SALP * ROVG
      PP01  = PISO**AR1
C
      DO J = 1,N
      IF (PRESS(J).LT.PISO) THEN
C
C     COMPUTE LAPSE RATE = -.0010 CASES
C
        HEIGHT(J) = ((TSTR/(PP01 * SALP )) * (PP01-(PRESS(J) ** AR1)))
     &              + ZISO
        TEMP(J)   = TSTR - ((HEIGHT(J) - ZISO) * SALP)
C
      ELSE IF (PRESS(J).GT.PTROP) THEN
C
        HEIGHT(J) = (T0/(PP0 * ALP)) * (PP0 - (PRESS(J) ** AR))
        TEMP(J)   = T0 - (HEIGHT(J) * ALP)
C
      ELSE
C
C     COMPUTE ISOTHERMAL CASES
C
        HEIGHT(J) = 11000.0 + (FKT * ALOG(PTROP/PRESS(J)))
        TEMP(J)   = TSTR
C
      END IF
        THETA(J)  = TEMP(J) * ((1000./PRESS(J))**ROVCP)
      END DO
C
      RETURN
      END
