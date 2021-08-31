C> @file
C> @brief Compute lifting condendsation level.
C> @author James Howcroft @date 1979-07-01

C> Given the pressure, temperature and relative humidity of
C> an air parcel at some point in the atmosphere, calculate the
C> dewpoint temperature and the pressure and temperature of the
C> lifting condensation level.
C>
C> Program history log:
C> - James Howcroft 1979-07-01
C> - Ralph Jones 1989-01-24 Change to microsoft fortran 4.10.
C> - Ralph Jones 1990-06-11 Change to sun fortran 1.3.
C> - Ralph Jones 1991-03-29 Convert to silicongraphics fortran.
C> - Ralph Jones 1993-03-29 Add save statement.
C> - Ralph Jones 1995-09-25 Put in cray w3 library.
C>
C> @param[in] P Parcel pressure in millibars.
C> @param[in] T Parcel temperature in degrees celsius.
C> @param[in] RH Parcel relative humidity in percent.
C> @param[out] TD Dewpoint temperature in degrees celsius.
C> @param[out] PLCL Pressure of LCL in millibars.
C> @param[out] TLCL Temperature at LCL in degrees celsius.
C>
C> @author James Howcroft @date 1979-07-01
      SUBROUTINE W3FA01(P,T,RH,TD,PLCL,TLCL)
C
      SAVE
C
C        DEFINITION OF THE POTENTIAL TEMPERATURE
C
      POTEMP(T,P) = (T+273.16)*((1000./P)**0.2857)
C
C        TETENS FORMULA WITH NATURAL BASE
C
      VAPRES(T)   = 6.11*EXP((17.2694*T)/(T+237.3))
C
C        BEGIN
C
      IF (RH.LT.100) GO TO 10
        PLCL = P
        TLCL = T
        TD   = T
        GO TO 40
C
C       CALCULATE DEW POINT FROM RH AND T
C
   10 CONTINUE
        AR    = ALOG(RH*0.01)/17.269
        TD    = (-237.3*(AR+1.0)*T - AR*237.3**2)/(AR*T+237.3*(AR-1.0))
        E     = VAPRES(TD)
        W     = (0.622*E)/(P-E)
        THETA = POTEMP(T,P)
C
C        DO STACKPOLE'S THING AS IN JOUR APPL MET, VOL 6, PP 464-467.
C
        EPS  = 0.1
        CGES = 0.5
C
C        CONSTANTS   -35.86 = 237.30 - 273.16
C                    2048.7 = 273.16 *   7.50
C
        PGES = (((CGES*(-35.86)+2048.7)/(THETA*(7.5-CGES)))**3.5)*1000.
C
C        START ITERATION.
C
   20 CONTINUE
        CGES = ALOG10((PGES*W)/(6.11*(0.622+W)))
        PLCL = (((CGES*(-35.86)+2048.7)/(THETA*(7.5-CGES)))**3.5)*1000.
        IF (ABS(PLCL-PGES) .LT. EPS)  GO TO 30
        PGES = PLCL
        GO TO 20
C
   30 CONTINUE
        TLCL   = (CGES * 237.3) / (7.5 - CGES)
C
C        FALL THRU WITH  P,T  OF THE LIFTED CONDENSATION LEVEL.
C
   40 CONTINUE
        RETURN
      END
