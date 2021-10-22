C> @file
C> @brief Convert (361,91) grid to (185,129) lambert grid
C> @author Ralph Jones @date 1994-05-18

C> Convert a northern hemisphere 1.0 degree lat.,lon. 361 by
C> 91 grid to a lambert conformal 185 by 129 awips grib 212.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1994-05-18 | Ralph Jones | Initial.
C>
C> @param[in] ALOLA 361*91 grid 1.0 deg. lat,lon grid n. hemi.
C> 32851 point grid. 360 * 181 one degree grib grid 3 was flipped, greenwish added
C> to right side and cut to 361 * 91.
C> @param[in] INTERP 1 linear interpolation , ne.1 biquadratic
C> @param[out] ALAMB 185*129 regional - conus double resolution
C> (lambert conformal). 23865 point grid is awips grid type 212
C>
C> @note
C> - 1. W1 and w2 are used to store sets of constants which are
C> reusable for repeated calls to the subroutine. 11 other array
C> are saved and reused on the next call.
C> - 2. Wind components are not rotated to the 185*129 grid orientation
C> after interpolation. You may use w3fc08() to do this.
C>
C> @author Ralph Jones @date 1994-05-18
      SUBROUTINE W3FT212(ALOLA,ALAMB,INTERP)
C
C
       PARAMETER   (NPTS=23865,II=185,JJ=129)
       PARAMETER   (ALATAN=25.000)
       PARAMETER   (PI=3.1416)
       PARAMETER   (DX=40635.250)
       PARAMETER   (ALAT1=12.190)
       PARAMETER   (ELON1=226.541)
       PARAMETER   (ELONV=265.000)
       PARAMETER   (III=361,JJJ=91)
C
       REAL        ALOLA(III,JJJ)
       REAL        ALAMB(NPTS)
       REAL        W1(NPTS),    W2(NPTS),   ERAS(NPTS,4)
       REAL        XDELI(NPTS), XDELJ(NPTS)
       REAL        XI2TM(NPTS), XJ2TM(NPTS)
C
       INTEGER     IV(NPTS),      JV(NPTS),    JY(NPTS,4)
       INTEGER     IM1(NPTS),     IP1(NPTS),   IP2(NPTS)
C
       LOGICAL     LIN
C
       SAVE
C
       DATA  ISWT  /0/
       DATA  INTRPO/99/
C
       LIN = .FALSE.
       IF (INTERP.EQ.1) LIN = .TRUE.
C
       IF (ISWT.EQ.1) GO TO 900
c      print *,'iswt = ',iswt
       N  = 0
       DO J = 1,JJ
         DO I = 1,II
           XJ = J
           XI = I
           CALL W3FB12(XI,XJ,ALAT1,ELON1,DX,ELONV,ALATAN,ALAT,
     &     ELON,IERR)
           N     = N    + 1
           W1(N) = ELON + 1.0
           W2(N) = ALAT + 1.0
         END DO
       END DO
C
       ISWT   = 1
       INTRPO = INTERP
       GO TO 1000
C
C     AFTER THE 1ST CALL TO W3FT212 TEST INTERP, IF IT HAS
C     CHANGED RECOMPUTE SOME CONSTANTS
C
 900   CONTINUE
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
        ALAMB(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
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
         ALAMB(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &             * XDELJ(KK)  + (ERAS(KK,1) - ERAS(KK,2)
     &             - ERAS(KK,3) + ERAS(KK,4)) * XJ2TM(KK)
 2500  CONTINUE
C
      ENDIF
C
      RETURN
      END
