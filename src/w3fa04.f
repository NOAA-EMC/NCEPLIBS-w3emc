C> @file
C> @brief Compute standard pressure, temp, pot temp.
C> @author James McDonell @date 1974-06-01

C> Computes the standard pressure, temperature, and poten-
C> tial temperature given the height in meters (<32 km). For
C> the pressure and temperature the results duplicate the values in
C> the U.S. standard atmosphere (1962), which is the icao standard
C> atmosphere to 54.7487 mb (20 km) and the proposed extension to
C> 8.68 mb (32 km). For potential temperature a value of 2/7 is
C> used for rd/cp.
C>
C> Program history log:
C> - James McDonell 1974-06-01
C> - Ralph Jones 1984-07-05 Change to ibm vs fortran.
C> - Ralph Jones 1990-04-27 Change to cray cft77 fortran.
C>
C> @param[in] HEIGHT Height in meters.
C> @param[out] PRESS Standard pressure in millibars.
C> @param[out] TEMP Temperature in degrees kelvin.
C> @param[out] THETA Potential temperature in degrees kelvin.
C>
C> @note Not valid for heights greater than 32 km. declare all parameters
C> as type real*4.
C>
C> @author James McDonell @date 1974-06-01

      SUBROUTINE W3FA04(HEIGHT,PRESS,TEMP,THETA)
C
      REAL  M0
C
      DATA
     *G     /9.80665/,
     *RSTAR /8314.32/,
     *M0    /28.9644/,
     *PISO  /54.7487/,
     *ZISO  /20000./,
     *SALP  /-.0010/,
     *TSTR  /216.65/,
     *PTROP /226.321/,
     *ALP   /.0065/,
     *T0    /288.15/,
     *PZERO /1013.25/
C
      ROVCP = 2.0 / 7.0
      R     = RSTAR/M0
      IF (HEIGHT.GT.ZISO)   GO TO 100
      IF (HEIGHT.GT.11000.) GO TO 200
C
C     COMPUTE IN TROPOSPHERE
C
      TEMP = T0 - HEIGHT * ALP
      PRESS = PZERO * ((1.0 - ((ALP/T0) * HEIGHT)) ** (G/(ALP * R)))
      GO TO 300
C
C     COMPUTE LAPSE RATE = -.0010 CASES
C
 100  CONTINUE
      D     = HEIGHT - ZISO
      PRESS = PISO * ((1.-(( SALP /TSTR) * D  )) ** (G/( SALP * R)))
      TEMP  = TSTR - D * SALP
      GO TO 300
C
C     COMPUTE ISOTHERMAL CASES
C
 200  CONTINUE
      D     = EXP((HEIGHT - 11000.0) / ((R / G) * TSTR))
      PRESS = PTROP / D
      TEMP  = TSTR
C
 300  CONTINUE
      THETA = TEMP * ((1000.0 / PRESS) ** ROVCP)
      RETURN
      END
