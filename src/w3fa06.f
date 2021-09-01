C> @file
C> @brief Calculation of the lifted index.
C> @author James Howcroft @date 1978-07-01

C> Given the pressure,temperature and relative humidity of
C> an air parcel at some point in the atmosphere, calculate the
C> lifted index of the parcel. Lifted index is defined as the
C> temperature difference between the observed 500mb temperature and
C> the supposed temperature that the parcel would obtain if it were
C> lifted dry-adiabatically to saturation and then moved moist
C> adiabatically to the 500mb level.
C>
C> Program history log:
C> - James Howcroft 1978-07-01
C> - Ralph Jones 1989-01-24 Change to microsoft fortran 4.10.
C> - Ralph Jones 1990-06-08 Change to sun fortran 1.3.
C> - Ralph Jones 1991-03-29 Convert to silicongraphics fortran.
C> - Ralph Jones 1993-03-29 Add save statement.
C> - Ralph Jones 1995-09-25 Put in w3 library on cray.
C>
C> @param[in] P Parcel pressure in millibars.
C> @param[in] T Parcel temperataure in degrees celsius.
C> @param[in] RH Parcel relative humidity in percent.
C> @param[in] T5 Temperature at the 500mb level in deg. celsius.
C> @param[out] TLI Lifted index in degrees celsius
C> tli = 9.9999  iteration diverges; return to user program.
C>
C> @author James Howcroft @date 1978-07-01
      SUBROUTINE W3FA06 (P,T,RH,T5,TLI)
C
       SAVE
C
       DATA            EPS   /0.5/
       DATA            KOUT  /  6/
C
  300  FORMAT ('  *** ITERATION NOT CONVERGING IN W3FA06 ***')
  350  FORMAT ('      INPUT PARAMS ARE:',4F15.8,/
     1         '      CALCULATIONS ARE',7E15.8)
C
       POTEMP(T,P) = (T+273.16)*((1000./P)**0.2857)
C
       EEP(T,P,ES) = EXP((596.73-0.601*T)*((0.622*ES)/(P-ES))
     1                / (0.24*(T+273.16)))
C
       UNPOT(TE,P) = (((P/1000.)**0.2857)*TE)-273.16
C
       VAPRES(T)   = 6.11*EXP(17.2694*T/(T+237.3))
C
       CALL W3FA01 (P,T,RH,TD,PLCL,TLCL)
       IF (PLCL .GT. 500.)  GO TO 30
       IF (PLCL .LT. 500.)  GO TO 20
         TLI = T5 - TLCL
         GO TO 80
   20  CONTINUE
C        LCL IS ABOVE THE 500MB LVL
         TLI = T5 - UNPOT((POTEMP(TLCL,PLCL)),500.)
         GO TO 80
   30  CONTINUE
C        USE STACKPOLE ALGORITHM (JAM VOL 6/1967 PP 464-7) TO FIND TGES
C        SO THAT (TGES,500) IS ON SAME MOIST ADIABAT AS (TLCL,PLCL).
         ES     = VAPRES(TLCL)
         THD    = POTEMP(TLCL,(PLCL-ES))
         THETA  = THD * EEP(TLCL,PLCL,ES)
C          THETA IS THE PSEUDO-EQUIV POTENTIAL TEMP THRU (PLCL,TLCL).
C      NOW FIND TEMP WHERE THETA INTERSECTS 500MB SFC.
C          INITIALIZE FOR STACKPOLIAN ITERATION
         TGES = T5
         DTT  = 10.
         PIIN = 1./(0.5**0.2857)
         A    = 0.
         ISTP = 0
C          START ITERATION.
   40  CONTINUE
         ISTP = ISTP + 1
         IF (ISTP .GT. 200)  GO TO 50
         SVA = VAPRES(TGES)
         AX  = A
         A   = (TGES+273.16)*PIIN * EEP(TGES,500.,SVA) - THETA
         IF (ABS(A) .LT. EPS)  GO TO 70
         DTT = DTT * 0.5
         IF (A*AX.LT.0.0)  DTT = -DTT
         TP   = TGES + DTT
         SVA  = VAPRES(TP)
         AP   = (TP+273.16)*PIIN * EEP(TP,500.,SVA) - THETA
         IF (ABS(AP) .LT. EPS)  GO TO 60
C          FIND NEXT ESTIMATE, DTT IS ADJUSTMENT FROM OLD TO NEW TGES.
           DTT  = A*DTT/(A-AP)
           IF (ABS(DTT).LT.0.01)  DTT = SIGN(0.01,DTT)
           TGES = TGES + DTT
           IF (TGES .GT. 50)  TGES = 50.
           GO TO 40
C
   50  CONTINUE
C        DISASTER SECTION
         WRITE (KOUT,300)
         WRITE (KOUT,350) P,T,RH,T5,THETA,AX,A,AP,TGES,TP,SVA
           TLI = 9.9999
           GO TO 80
   60  CONTINUE
         TGES = TP
   70  CONTINUE
         TLI  = T5 - TGES
   80  CONTINUE
       RETURN
       END
