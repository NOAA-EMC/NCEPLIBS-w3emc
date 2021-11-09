C> @file
C> @brief Convert (361,181) grid to (93,68) mercator grid.
C> @author Ralph Jones @date 1994-05-18

C> Convert a n. s. hemisphere 1.0 degree lat.,lon. 361 by
C> 181 grid to a national - hawaii (mercator) 93*68 awips 204
C> grid.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1994-05-18 | Ralph Jones | Initial.
C>
C> @param[in] ALOLA 361*181 grid 1.0 deg. lat,lon grid n. hemi.
C> 65341 point grid. 360 * 181 one degree grib grid 3 was flipped, greenwish added
C> to right side.
C> @param[in] INTERP 1 linear interpolation , ne.1 biquadratic
C> @param[out] AMERC 93*68 grid national - hawaii (mercator) 6324 point grid
C> is awips grid type 204
C> @note
C> - 1. W1 and w2 are used to store sets of constants which are
C> reusable for repeated calls to the subroutine. 20 other array
C> are saved and reused on the next call.
C>
C> @author Ralph Jones @date 1994-05-18
      SUBROUTINE W3FT204(ALOLA,AMERC,INTERP)
C
       PARAMETER   (NPTS=6324,II=93,JJ=68)
       PARAMETER   (ALATIN=20.000)
       PARAMETER   (PI=3.1416)
       PARAMETER   (DX=160000.0)
       PARAMETER   (ALAT1=-25.000)
       PARAMETER   (ALON1=110.000)
C
       REAL        WLON(NPTS),    XLAT(NPTS)
       REAL        XI(II,JJ),     XJ(II,JJ)
       REAL        XII(NPTS),     XJJ(NPTS)
       REAL        ALOLA(361,181), AMERC(NPTS), ERAS(NPTS,4)
       REAL        W1(NPTS),      W2(NPTS)
       REAL        XDELI(NPTS),   XDELJ(NPTS)
       REAL        XI2TM(NPTS),   XJ2TM(NPTS)
C
       INTEGER     IV(NPTS),      JV(NPTS),    JY(NPTS,4)
       INTEGER     IM1(NPTS),     IP1(NPTS),   IP2(NPTS)
C
       LOGICAL     LIN
C
       SAVE
C
       EQUIVALENCE (XI(1,1),XII(1)),(XJ(1,1),XJJ(1))
C
C      DATA  DEGPR /57.2957795/
       DATA  RERTH /6.3712E+6/
       DATA  INTRPO/99/
       DATA  ISWT  /0/
C
      RADPD  = PI / 180.0
      DEGPR  = 180.0 / PI
      CLAIN  = COS(RADPD * ALATIN)
      DELLON = DX / (RERTH * CLAIN)
      DJEO   = (ALOG(TAN(0.5*((ALAT1+90.0)*RADPD))))/DELLON
C
      LIN    = .FALSE.
      IF (INTERP.EQ.1) LIN = .TRUE.
C
      IF  (ISWT.EQ.1)  GO TO  900
C
        DEG    = 1.0
C
C     NEXT 32 LINES OF CODE PUTS SUBROUTINE W3FB09 IN LINE
C
      DO 100 J = 1,JJ
         DO 100 I = 1,II
             XI(I,J) = I
             XJ(I,J) = J
 100     CONTINUE
C
      DO 200 KK = 1,NPTS
        XLAT(KK) = 2.0*ATAN(EXP(DELLON*(DJEO + XJJ(KK)-1.)))
     &                * DEGPR - 90.0
 200  CONTINUE
C
      DO 300 KK = 1,NPTS
        WLON(KK) = (XII(KK) -1.0) * DELLON * DEGPR + ALON1
 300  CONTINUE
C
      DO 400 KK = 1,NPTS
        W1(KK)  = WLON(KK) +  1.0
        W2(KK)  = XLAT(KK) + 91.0
 400  CONTINUE
C
      ISWT   = 1
      INTRPO = INTERP
      GO TO 1000
C
C     AFTER THE 1ST CALL TO W3FT204 TEST INTERP, IF IT HAS
C     CHANGED RECOMPUTE SOME CONSTANTS
C
  900 CONTINUE
        IF (INTERP.EQ.INTRPO) GO TO 2100
        INTRPO = INTERP
C
 1000 CONTINUE
        DO 1100 K = 1,NPTS
          IV(K)    = W1(K)
          JV(K)    = W2(K)
          XDELI(K) = W1(K) - IV(K)
          XDELJ(K) = W2(K) - JV(K)
          IP1(K)   = IV(K) + 1
          JY(K,3)  = JV(K) + 1
          JY(K,2)  = JV(K)
 1100   CONTINUE
C
      IF (LIN) GO TO 2100
C
      DO 1200 K = 1,NPTS
        IP2(K)   = IV(K) + 2
        IM1(K)   = IV(K) - 1
        JY(K,1)  = JV(K) - 1
        JY(K,4)  = JV(K) + 2
        XI2TM(K) = XDELI(K) * (XDELI(K) - 1.0) * .25
        XJ2TM(K) = XDELJ(K) * (XDELJ(K) - 1.0) * .25
 1200 CONTINUE
C
 2100 CONTINUE
      IF (LIN) THEN
C
C     LINEAR INTERPOLATION
C
      DO 2200 KK = 1,NPTS
        ERAS(KK,2) = (ALOLA(IP1(KK),JY(KK,2))-ALOLA(IV(KK),JY(KK,2)))
     &             * XDELI(KK) + ALOLA(IV(KK),JY(KK,2))
        ERAS(KK,3) = (ALOLA(IP1(KK),JY(KK,3))-ALOLA(IV(KK),JY(KK,3)))
     &             * XDELI(KK) + ALOLA(IV(KK),JY(KK,3))
 2200 CONTINUE
C
      DO 2300 KK = 1,NPTS
        AMERC(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &            * XDELJ(KK)
 2300 CONTINUE
C
      ELSE
C
C     QUADRATIC INTERPOLATION
C
      DO 2400 KK = 1,NPTS
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
       DO 2500 KK = 1,NPTS
         AMERC(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &             * XDELJ(KK)  + (ERAS(KK,1) - ERAS(KK,2)
     &             - ERAS(KK,3) + ERAS(KK,4)) * XJ2TM(KK)
 2500  CONTINUE
C
      ENDIF
C
      RETURN
      END
