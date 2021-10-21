C> @file
C> @brief Convert (145,37) grid to (65,65) s. hemi. grid.
C> @author Ralph Jones @date 1985-04-10

C> Convert a southern hemisphere 2.5 degree lat.,lon. 145 by
C> 37 grid to a polar stereographic 65 by 65 grid. The polar
C> stereographic map projection is true at 60 deg. s.; The mesh
C> length is 381 km. and the oriention is 260 deg. w.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1985-04-10 | Ralph Jones | Vectorized version of w3ft05.
C> 1989-10-21 | Ralph Jones | Changes to increase speed.
C> 1991-07-24 | Ralph Jones | Change  to cray cft77 fortran.
C> 1993-05-31 | Ralph Jones | Recompile so linear interpolation works.
C>
C> @param[in] ALOLA  - 145*37 gid 2.5 lat,lon grid s. hemishere. 5365 point
C> grid is o.n.84 type 30 or 1e hex.
C> @param[in] INTERP - 1 linear interpolation , ne.1 biquadratic.
C> @param[out] APOLA - 65*65 grid of northern hemi. 4225 point grid is o.n. 84
C> type 28 or 1c hex.
C>
C> @remark
C> - 1. W1 and w2 are used to store sets of constants which are
C> reusable for repeated calls to the subroutine.
C> - 2. Wind components are not rotated to the 65*65 grid orientation
C> after interpolation. You may use w3fc10 to do this.
C> - 3. The grid points values on the equator have been extrapolated
C> outward to all the grid points outside the equator on the 65*65
C> grid (about 1100 points).
C>
C> @author Ralph Jones @date 1985-04-10
      SUBROUTINE W3FT06V(ALOLA,APOLA,INTERP)
C
       REAL        R2(4225),      WLON(4225)
       REAL        XLAT(4225),    XI(65,65),   XJ(65,65)
       REAL        XII(4225),     XJJ(4225),   ANGLE(4225)
       REAL        ALOLA(145,37), APOLA(4225), ERAS(4225,4)
       REAL        W1(4225),      W2(4225)
       REAL        XDELI(4225),   XDELJ(4225)
       REAL        XI2TM(4225),   XJ2TM(4225)
C
       INTEGER     IV(4225),      JV(4225),    JY(4225,4)
       INTEGER     IM1(4225),     IP1(4225),   IP2(4225)
C
       LOGICAL     LIN
C
       SAVE
C
       EQUIVALENCE (XI(1,1),XII(1)),(XJ(1,1),XJJ(1))
C
       DATA  DEGPRD/57.2957795/
       DATA  EARTHR/6371.2/
       DATA  INTRPO/99/
       DATA  ISWT  /0/
C
      LIN = .FALSE.
      IF (INTERP.EQ.1) LIN = .TRUE.
      IF  (ISWT.EQ.1)  GO TO  900
C
        ORIENT = 260.0
        DEG    = 2.5
        XMESH  = 381.0
        GI2    = (1.86603 * EARTHR) / XMESH
        GI2    = GI2 * GI2
C
C     NEXT 32 LINES OF CODE PUTS SUBROUTINE W3FB03 IN LINE
C
      DO 100 J = 1,65
         XJ1 = J - 33
         DO 100 I = 1,65
             XI(I,J) = I - 33
             XJ(I,J) = XJ1
 100     CONTINUE
C
      DO 200 KK = 1,4225
        R2(KK)   = XJJ(KK) * XJJ(KK) + XII(KK) * XII(KK)
        XLAT(KK) = -DEGPRD *
     &      ASIN((GI2 - R2(KK)) / (GI2 + R2(KK)))
 200  CONTINUE
C
      XII(2113) = 1.0
      DO 300 KK = 1,4225
        ANGLE(KK) = DEGPRD * ATAN2(XJJ(KK),XII(KK))
 300  CONTINUE
C
      DO 400 KK = 1,4225
        IF (ANGLE(KK).LT.0.0) ANGLE(KK) = ANGLE(KK) + 360.0
 400  CONTINUE
C
      DO 500 KK = 1,4225
        WLON(KK) = ANGLE(KK) + ORIENT - 270.0
 500  CONTINUE
C
      DO 600 KK = 1,4225
        IF (WLON(KK).GE.360.0) WLON(KK) = WLON(KK) - 360.0
 600  CONTINUE
C
      DO 700 KK = 1,4225
        IF (WLON(KK).LT.0.0)   WLON(KK) = WLON(KK) + 360.0
 700  CONTINUE
C
      XLAT(2113) = -90.0
      WLON(2113) =   0.0
C
      DO 800 KK = 1,4225
        W1(KK)  = (360.0 - WLON(KK)) / DEG + 1.0
        W2(KK)  = (XLAT(KK) + 90.0)  / DEG + 1.0
 800  CONTINUE
C
      ISWT   = 1
      INTRPO = INTERP
      GO TO 1000
C
C     AFTER THE 1ST CALL TO W3FT05 TEST INTERP, IF IT HAS
C     CHANGED RECOMPUTE SOME CONSTANTS
C
  900 CONTINUE
        IF (INTERP.EQ.INTRPO) GO TO 2100
        INTRPO = INTERP
C
 1000 CONTINUE
        DO 1100 K = 1,4225
          IV(K)    = W1(K)
          JV(K)    = W2(K)
          XDELI(K) = W1(K) - IV(K)
          XDELJ(K) = W2(K) - JV(K)
          IP1(K)   = IV(K) + 1
          JY(K,3)  = JV(K) + 1
          JY(K,2)  = JV(K)
 1100   CONTINUE
C
      IF (LIN) GO TO 1400
C
      DO 1200 K = 1,4225
        IP2(K)   = IV(K) + 2
        IM1(K)   = IV(K) - 1
        JY(K,1)  = JV(K) - 1
        JY(K,4)  = JV(K) + 2
        XI2TM(K) = XDELI(K) * (XDELI(K) - 1.0) * .25
        XJ2TM(K) = XDELJ(K) * (XDELJ(K) - 1.0) * .25
 1200 CONTINUE
C
      DO 1300 KK = 1,4225
         IF (IV(KK).EQ.1) THEN
           IP2(KK) = 3
           IM1(KK) = 144
         ELSE IF (IV(KK).EQ.144) THEN
           IP2(KK) = 2
           IM1(KK) = 143
         ENDIF
 1300 CONTINUE
C
 1400 CONTINUE
C
      IF (LIN) GO TO 1700
C
      DO 1500 KK = 1,4225
        IF (JV(KK).LT.2.OR.JV(KK).GT.35) XJ2TM(KK) = 0.0
 1500 CONTINUE
C
      DO 1600 KK = 1,4225
        IF (IP2(KK).LT.1)   IP2(KK) = 1
        IF (IM1(KK).LT.1)   IM1(KK) = 1
        IF (IP2(KK).GT.145) IP2(KK) = 145
        IF (IM1(KK).GT.145) IM1(KK) = 145
 1600 CONTINUE
C
 1700 CONTINUE
      DO 1800 KK = 1,4225
        IF (IV(KK).LT.1)    IV(KK)  = 1
        IF (IP1(KK).LT.1)   IP1(KK) = 1
        IF (IV(KK).GT.145)  IV(KK)  = 145
        IF (IP1(KK).GT.145) IP1(KK) = 145
 1800 CONTINUE
C
C     LINEAR INTERPOLATION
C
      DO 1900 KK = 1,4225
        IF (JY(KK,2).LT.1)  JY(KK,2) = 1
        IF (JY(KK,2).GT.37) JY(KK,2) = 37
        IF (JY(KK,3).LT.1)  JY(KK,3) = 1
        IF (JY(KK,3).GT.37) JY(KK,3) = 37
 1900 CONTINUE
C
      IF (.NOT.LIN) THEN
      DO 2000 KK = 1,4225
        IF (JY(KK,1).LT.1)  JY(KK,1) = 1
        IF (JY(KK,1).GT.37) JY(KK,1) = 37
        IF (JY(KK,4).LT.1)  JY(KK,4) = 1
        IF (JY(KK,4).GT.37) JY(KK,4) = 37
 2000 CONTINUE
      ENDIF
C
 2100 CONTINUE
      IF (LIN) THEN
C
C     LINEAR INTERPOLATION
C
      DO 2200 KK = 1,4225
        ERAS(KK,2) = (ALOLA(IP1(KK),JY(KK,2))-ALOLA(IV(KK),JY(KK,2)))
     &             * XDELI(KK) + ALOLA(IV(KK),JY(KK,2))
        ERAS(KK,3) = (ALOLA(IP1(KK),JY(KK,3))-ALOLA(IV(KK),JY(KK,3)))
     &             * XDELI(KK) + ALOLA(IV(KK),JY(KK,3))
 2200 CONTINUE
C
      DO 2300 KK = 1,4225
        APOLA(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &            * XDELJ(KK)
 2300 CONTINUE
C
      ELSE
C
C     QUADRATIC INTERPOLATION
C
      DO 2400 KK = 1,4225
        ERAS(KK,1)=(ALOLA(IP1(KK),JY(KK,1))-ALOLA(IV(KK),JY(KK,1)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,1)) +
     &            ( ALOLA(IM1(KK),JY(KK,1)) - ALOLA(IV(KK),JY(KK,1))
     &            - ALOLA(IP1(KK),JY(KK,1))+ALOLA(IP2(KK),JY(KK,1)))
     &            * XI2TM(KK)
        ERAS(KK,2)=(ALOLA(IP1(KK),JY(KK,2))-ALOLA(IV(KK),JY(KK,2)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,2)) +
     &            ( ALOLA(IM1(KK),JY(KK,2)) - ALOLA(IV(KK),JY(KK,2))
     &            - ALOLA(IP1(KK),JY(KK,2))+ALOLA(IP2(KK),JY(KK,2)))
     &            * XI2TM(KK)
        ERAS(KK,3)=(ALOLA(IP1(KK),JY(KK,3))-ALOLA(IV(KK),JY(KK,3)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,3)) +
     &            ( ALOLA(IM1(KK),JY(KK,3)) - ALOLA(IV(KK),JY(KK,3))
     &            - ALOLA(IP1(KK),JY(KK,3))+ALOLA(IP2(KK),JY(KK,3)))
     &            * XI2TM(KK)
        ERAS(KK,4)=(ALOLA(IP1(KK),JY(KK,4))-ALOLA(IV(KK),JY(KK,4)))
     &            * XDELI(KK) + ALOLA(IV(KK),JY(KK,4)) +
     &            ( ALOLA(IM1(KK),JY(KK,4)) - ALOLA(IV(KK),JY(KK,4))
     &            - ALOLA(IP1(KK),JY(KK,4))+ALOLA(IP2(KK),JY(KK,4)))
     &            * XI2TM(KK)
 2400      CONTINUE
C
       DO 2500 KK = 1,4225
         APOLA(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &             * XDELJ(KK)  + (ERAS(KK,1) - ERAS(KK,2)
     &             - ERAS(KK,3) + ERAS(KK,4)) * XJ2TM(KK)
 2500  CONTINUE
C
      ENDIF
C
C     SET POLE POINT , WMO STANDARD FOR U OR V
C
      APOLA(2113) = ALOLA(73,1)
C
      RETURN
      END
