C> @file
C> @brief Compute standard height, temp, and pot temp.
C> @author James McDonell @date 1974-06-01

C> Computes the standard height, temperature, and potential
C> temperature given the pressure in millibars ( > 8.68 mb ). For
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
C>
C> @param[in] PRESS Pressure in millibars.
C> @param[out] HEIGHT Height in meters.
C> @param[out] TEMP Temperature in degrees kelvin.
C> @param[out] THETA Potential temperature in degrees kelvin.
C>
C> @note Not valid for pressures less than 8.68 millibars, declare
C> all parameters as type real.
C>
C> @author James McDonell @date 1974-06-01
      SUBROUTINE W3FA03(PRESS,HEIGHT,TEMP,THETA)
C
      REAL  M0
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
      IF(PRESS.LT.PISO)   GO TO 100
      IF(PRESS.GT.PTROP)  GO TO 200
C
C     COMPUTE ISOTHERMAL CASES
C
      HEIGHT = 11000.0 + (FKT * ALOG(PTROP/PRESS))
      TEMP = TSTR
      GO TO 300
C
C     COMPUTE LAPSE RATE = -.0010 CASES
C
 100  CONTINUE
      AR     = SALP * ROVG
      PP0    = PISO**AR
      HEIGHT = ((TSTR/(PP0 * SALP )) * (PP0-(PRESS ** AR))) + ZISO
      TEMP   = TSTR - ((HEIGHT - ZISO) * SALP)
      GO TO 300
C
 200  CONTINUE
      HEIGHT = (T0/(PP0 * ALP)) * (PP0 - (PRESS ** AR))
      TEMP   = T0 - (HEIGHT * ALP)
C
 300  CONTINUE
      THETA  = TEMP * ((1000./PRESS)**ROVCP)
      RETURN
      END
