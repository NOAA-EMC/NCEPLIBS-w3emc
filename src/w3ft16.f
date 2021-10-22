C> @file
C> @brief Convert (95,91) grid to (3447) grid
C> @author Ralph Jones @date 1994-05-03

C> Convert a northern hemisphere 1.0 degree lat.,lon. 95 by
C> 91 grid to a wafs 1.25 degree thinned 3447 point grid.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1994-05-03 | Ralph Jones | Initial.
C>
C> @param[in] ALOLA 95 * 91 grid 1.0 deg. lat,lon grid northern hemisphere 8645 point grid.
C> @param[in] INTERP 1 linear interpolation , ne.1 biquadratic
C> @param[out] BTHIN 3447 point thinned grid of n. hemispere 3447 grid is for grib grids 37-40.
C>
C> @note
C> - W1 and w2 are used to store sets of constants which are
C> reusable for repeated calls to the subroutine. 10 other arrays
C> are saved and reused on the next call.
C>
C> @author Ralph Jones @date 1994-05-03
      SUBROUTINE W3FT16(ALOLA,BTHIN,INTERP)
C
       PARAMETER   (NPTS=3447)
C
       REAL        SEP(73)
       REAL        ALOLA(95,91),  BTHIN(NPTS), ERAS(NPTS,4)
       REAL        W1(NPTS),      W2(NPTS)
       REAL        XDELI(NPTS),   XDELJ(NPTS)
       REAL        XI2TM(NPTS),   XJ2TM(NPTS)
C
       INTEGER     NPT(73)
       INTEGER     IV(NPTS),      JV(NPTS),    JY(NPTS,4)
       INTEGER     IM1(NPTS),     IP1(NPTS),   IP2(NPTS)
C
       LOGICAL     LIN
C
       SAVE
C
       DATA  INTRPO/99/
       DATA  ISWT  /0/
C
C   GRID POINT SEPARATION
C
       DATA  SEP   /1.250, 1.250, 1.250, 1.250, 1.250, 1.250,
     &              1.250, 1.250, 1.268, 1.268, 1.268, 1.286,
     &              1.286, 1.286, 1.304, 1.304, 1.324, 1.324,
     &              1.343, 1.364, 1.364, 1.385, 1.406, 1.406,
     &              1.429, 1.452, 1.475, 1.500, 1.525, 1.525,
     &              1.552, 1.579, 1.607, 1.636, 1.667, 1.698,
     &              1.765, 1.800, 1.837, 1.875, 1.915, 1.957,
     &              2.045, 2.093, 2.143, 2.195, 2.308, 2.368,
     &              2.432, 2.571, 2.647, 2.813, 2.903, 3.103,
     &              3.214, 3.333, 3.600, 3.750, 4.091, 4.286,
     &              4.737, 5.000, 5.625, 6.000, 6.923, 8.182,
     &              9.000,11.250,12.857,18.000,22.500,45.000,
     &              90.000/
C
C   NUMBER OF POINTS ALONG LAT CIRCLE FOR ONE OCTANT
C
       DATA  NPT   /   73,    73,    73,    73,    73,    73,
     &                 73,    73,    72,    72,    72,    71,
     &                 71,    71,    70,    70,    69,    69,
     &                 68,    67,    67,    66,    65,    65,
     &                 64,    63,    62,    61,    60,    60,
     &                 59,    58,    57,    56,    55,    54,
     &                 52,    51,    50,    49,    48,    47,
     &                 45,    44,    43,    42,    40,    39,
     &                 38,    36,    35,    33,    32,    30,
     &                 29,    28,    26,    25,    23,    22,
     &                 20,    19,    17,    16,    14,    12,
     &                 11,     9,     8,     6,     5,     3,
     &                  2/
C
      LIN    = .FALSE.
      IF (INTERP.EQ.1) LIN = .TRUE.
C
      IF  (ISWT.EQ.1)  GO TO  900
C
      IJOUT = 0
      DO 200 J = 1,73
         XJOU   = (J-1) * 1.25 + 1.0
         II     = NPT(J)
         RDGLAT = SEP(J)
         DO 100 I = 1,II
           IJOUT     = IJOUT + 1
           W1(IJOUT) = (I-1) * RDGLAT + 3.0
           W2(IJOUT) = XJOU
 100     CONTINUE
 200   CONTINUE
C
       ISWT   = 1
       INTRPO = INTERP
       GO TO 1000
C
C     AFTER THE 1ST CALL TO W3FT16 TEST INTERP, IF IT HAS
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
      IF (LIN) GO TO 1400
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
 1400 CONTINUE
C
      IF (LIN) GO TO 1700
C
      DO 1500 KK = 1,NPTS
        IF (JV(KK).LT.2.OR.JV(KK).GE.90) XJ2TM(KK) = 0.0
 1500 CONTINUE
C
C     LINEAR INTERPOLATION
C
 1700 CONTINUE
      DO 1900 KK = 1,NPTS
        IF (JY(KK,3).GT.91) JY(KK,3) = 91
 1900 CONTINUE
C
      IF (.NOT.LIN) THEN
      DO 2000 KK = 1,NPTS
        IF (JY(KK,1).LT.1)  JY(KK,1) = 1
        IF (JY(KK,4).GT.91) JY(KK,4) = 91
 2000 CONTINUE
      ENDIF
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
        BTHIN(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
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
         BTHIN(KK) = ERAS(KK,2) + (ERAS(KK,3) - ERAS(KK,2))
     &             * XDELJ(KK)  + (ERAS(KK,1) - ERAS(KK,2)
     &             - ERAS(KK,3) + ERAS(KK,4)) * XJ2TM(KK)
 2500  CONTINUE
C
      ENDIF
C
      RETURN
      END
