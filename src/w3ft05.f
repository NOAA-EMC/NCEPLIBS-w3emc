C> @file
C> @brief Convert (145,37) to (65,65) n. hemi. grid
C> @author Ralph Jones @date 1985-04-08

C> Convert a northern hemisphere 2.5 degree lat.,lon. 145 by
C> 37 grid to a polar stereographic 65 by 65 grid. The polar
C> stereographic map projection is true at 60 deg. n. , The mesh
C> length is 381 km. and the oriention is 80 deg. w.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1985-04-08 | Ralph Jones | Initial.
C> 1991-07-30 | Ralph Jones | convert to cray cft77 fortran.
C> 1992-05-02 | Ralph Jones | add save.
C>
C> @param[in] ALOLA 145*37 grid 2.5 lat,lon grid n. hemi.
C> 5365 point grid is type 29 or 1d hex o.n. 84
C> @param[in] LINEAR 1 linear interpolation , ne.1 biquadratic
C> @param[out] APOLA 65*65 grid of northern hemi.
C> 4225 point grid is type 27 or 1b hex o.n. 84
C> @param[out] W1 65*65 scratch field
C> @param[out] W2 65*65 scratch field
C>
C> @remark
C> - 1. W1 and w2 are used to store sets of constants which are
C> reusable for repeated calls to the subroutine. If they are
C> over written by the user, a warning message will be printed
C> and w1 and w2 will be recomputed.
C> - 2. Wind components are not rotated to the 65*65 grid orientation
C> after interpolation. You may use w3fc08() to do this.
C> - 3. The grid points values on the equator have been extrapolated
C> outward to all the grid points outside the equator on the 65*65
C> grid (about 1100 points).
C> - 4. You should use the cray vectorized version w3ft05v on the cray
C> it has 3 parameters in the call, runs about 10 times faster. Uses
C> more memory.
C>
C> @author Ralph Jones @date 1985-04-08
      SUBROUTINE W3FT05(ALOLA,APOLA,W1,W2,LINEAR)
C
       REAL            ALOLA(145,37)
       REAL            APOLA(4225)
       REAL            ERAS(4)
       REAL            SAVEW1(10)
       REAL            SAVEW2(10)
       REAL            W1(4225)
       REAL            W2(4225)
C
       INTEGER         JY(4)
       INTEGER         OUT
C
       LOGICAL         LIN
C
       SAVE
C
       DATA  DEGPRD/57.2957795/
       DATA  EARTHR/6371.2/
       DATA  ISWT  /0/
       DATA  OUT   /6/
C
 4000  FORMAT ( 52H *** WARNING , W1 OR W2 SCRATCH FILES OVER WRITTEN ,,
     &          43H I WILL RESTORE THEM , BURNING UP CPU TIME,,
     &          14H IN W3FT05 ***)
C
         LIN = .FALSE.
         IF (LINEAR.EQ.1) LIN = .TRUE.
C
         IF  (ISWT.EQ.0)  GO TO  300
C
C        TEST W1 AND W2 TO SEE IF THEY WERE WRITTEN OVER
C
         DO 100  KK=1,10
           IF (SAVEW1(KK).NE.W1(KK)) GO TO  200
           IF (SAVEW2(KK).NE.W2(KK)) GO TO  200
  100    CONTINUE
         GOTO  1000
C
  200    CONTINUE
         WRITE (OUT,4000)
C
  300    CONTINUE
         DEG   = 2.5
         NN    = 0
         XMESH = 381.0
         GI2   = (1.86603*EARTHR) / XMESH
         GI2   = GI2 * GI2
C
C        DO LOOP 800 PUTS SUBROUTINE W3FB01 IN LINE
C
         DO  800  J = 1,65
           XJ  = J - 33
           XJ2 = XJ * XJ
           DO  800  I=1,65
             XI = I - 33
             R2 = XI*XI + XJ2
             IF  (R2.NE.0.0)  GO TO  400
             WLON = 0.0
             XLAT = 90.0
             GO TO  700
 400         CONTINUE
             XLONG = DEGPRD * ATAN2(XJ,XI)
             IF (XLONG.GE.0.0)  GO TO 500
             WLON  = -10.0 - XLONG
             IF (WLON.LT.0.0)  WLON = WLON + 360.0
             GO TO  600
C
 500         CONTINUE
             WLON = 350.0 - XLONG
 600         CONTINUE
             XLAT = ASIN((GI2-R2)/(GI2+R2))*DEGPRD
 700         CONTINUE
             IF  (WLON.GT.360.0)  WLON = WLON - 360.0
             IF  (WLON.LT.0.0)   WLON = WLON + 360.0
             NN     = NN + 1
             W1(NN) = ( 360.0 - WLON ) / DEG + 1.0
             W2(NN) = XLAT / DEG + 1.0
 800       CONTINUE
C
         DO 900  KK = 1,10
           SAVEW1(KK) = W1(KK)
           SAVEW2(KK) = W2(KK)
 900     CONTINUE
C
         ISWT = 1
C
 1000    CONTINUE
C
         DO  2100  KK = 1,4225
           I     = W1(KK)
           J     = W2(KK)
           FI    = I
           FJ    = J
           XDELI = W1(KK) - FI
           XDELJ = W2(KK) - FJ
           IP1   = I + 1
           JY(3) = J + 1
           JY(2) = J
           IF (LIN)  GO TO 1100
           IP2   = I + 2
           IM1   = I - 1
           JY(4) = J + 2
           JY(1) = J - 1
           XI2TM = XDELI * (XDELI-1.) * 0.25
           XJ2TM = XDELJ * (XDELJ-1.) * 0.25
C
 1100    CONTINUE
           IF ((I.LT.2).OR.(J.LT.2))    GO TO  1200
           IF ((I.GT.142).OR.(J.GT.34)) GO TO  1200
C
C     QUADRATIC (LINEAR TOO) OK W/O FURTHER ADO SO GO TO 1700
C
           GO TO  1700
C
 1200    CONTINUE
           IF (I.EQ.1)   GO TO  1300
           IF (I.EQ.144) GO TO 1400
           IP2 = I + 2
           IM1 = I - 1
           GO TO  1500
C
 1300    CONTINUE
           IP2 = 3
           IM1 = 144
           GO TO   1500
C
 1400    CONTINUE
           IP2 = 2
           IM1 = 143
C
 1500   CONTINUE
          IP1 = I + 1
          IF (LIN)  GO TO 1600
          IF ((J.LT.2).OR.(J.GE.36))  XJ2TM=0.
C.....DO NOT ALLOW POINT OFF GRID
          IF (IP2.LT.1)   IP2 = 1
          IF (IM1.LT.1)   IM1 = 1
          IF (IP2.GT.145) IP2 = 145
          IF (IM1.GT.145) IM1 = 145
C
 1600   CONTINUE
C.....DO NOT ALLOW POINT OFF GRID
          IF (I.LT.1)     I   = 1
          IF (IP1.LT.1)   IP1 = 1
          IF (I.GT.145)   I   = 145
          IF (IP1.GT.145) IP1 = 145
C
 1700    CONTINUE
         IF (.NOT.LIN)  GO TO  1900
C
C        LINEAR INTERPLOATION
C
         DO 1800 K = 2,3
           J1 = JY(K)
           IF  (J1.LT.1)  J1 = 1
           IF  (J1.GT.37) J1 = 37
           ERAS(K) = (ALOLA(IP1,J1) - ALOLA(I,J1)) * XDELI + ALOLA(I,J1)
 1800    CONTINUE
C
           APOLA(KK) = ERAS(2) + (ERAS(3) - ERAS(2)) * XDELJ
           GO TO  2100
C
 1900    CONTINUE
C
C        QUADRATIC INTERPOLATION
C
         DO 2000 K = 1,4
           J1 = JY(K)
C.....DO NOT ALLOW POINT OFF GRID
           IF (J1.LT.1) J1 = 1
           IF (J1.GT.37) J1 = 37
           ERAS(K) = (ALOLA(IP1,J1)-ALOLA(I,J1))*XDELI+ALOLA(I,J1)+
     &               (ALOLA(IM1,J1)-ALOLA(I,J1)-ALOLA(IP1,J1)+
     &                ALOLA(IP2,J1))*XI2TM
 2000    CONTINUE
C
         APOLA(KK) = ERAS(2)+(ERAS(3)-ERAS(2))*XDELJ+(ERAS(1)-
     &               ERAS(2)-ERAS(3)+ERAS(4)) * XJ2TM
C
 2100    CONTINUE
C
C        SET POLE POINT , WMO STANDARD FOR U OR V
C
         APOLA(2113) = ALOLA(73,37)
C
         RETURN
       END
