C> @file
C> @brief Fast fourier for 2.5 degree grid.
C> @author Joe Sela @date 1980-11-21

C> Fast fourier to compute 145 grid values at desired
C> latitude from 31 complex fourier coefficients. This subroutine
C> is special purpose for converting coefficients to a 2.5 degree
C> lat,lon grid.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1980-11-21 | Joe Sela | Initial.
C> 1984-06-21 | Ralph Jones | Change to ibm vs fortran.
C> 1993-04-12 | Ralph Jones | Change to cray cft77 fortran.
C>
C> @param[in] COEF 31 complex fourier coefficients.
C> @param[in] TRIGS 216 trig functions assumed precomputed by w3fa13() before
C> first call to w3ft12().
C> @param[in] WORK 144 real work space
C> @param[out] GRID 145 grid values, grid(1)=grid(145)
C>
C> @author Joe Sela @date 1980-11-21
       SUBROUTINE W3FT12(COEF,WORK,GRID,TRIGS)
       REAL            COEF( 62 )
       REAL            GRID(145)
       REAL            TRIGS(216)
       REAL            WORK(144)
C
       SAVE
C
       DATA  SIN60/0.866025403784437/
C
      GRID(1) = COEF(1)
      GRID(2) = COEF(1)
      K = 147
      J = 143
      DO 100 I=3, 61 ,2
      TEMP = COEF(I)*TRIGS(K+1) - COEF(I+1)*TRIGS(K)
      GRID(I) = COEF(I) - TEMP
      GRID(J) = COEF(I) + TEMP
      TEMP = COEF(I)*TRIGS(K) + COEF(I+1)*TRIGS(K+1)
      GRID(I+1) = TEMP - COEF(I+1)
      GRID(J+1) = TEMP + COEF(I+1)
      K = K + 2
      J = J - 2
100   CONTINUE
      DO 110 I= 63 , 84
      GRID(I) = 0.0
110   CONTINUE
C
      A0 = GRID(1) + GRID(73)
      A2 = GRID(1) - GRID(73)
      B0 = GRID(2) + GRID(74)
      B2 = GRID(2) - GRID(74)
      A1 = GRID(37) + GRID(109)
      A3 = GRID(37) - GRID(109)
      B1 = GRID(38) + GRID(110)
      B3 = GRID(38) - GRID(110)
      WORK(1) = A0 + A1
      WORK(5) = A0 - A1
      WORK(2) = B0 + B1
      WORK(6) = B0 - B1
      WORK(3) = A2 - B3
      WORK(7) = A2 + B3
      WORK(4) = B2 + A3
      WORK(8) = B2 - A3
      KB = 3
      KC = 5
      KD = 7
      J = 75
      K = 39
      L = 111
      M = 9
      DO 300 I=3,35,2
      A0 = GRID(I) + GRID(J)
      A2 = GRID(I) - GRID(J)
      B0 = GRID(I+1) + GRID(J+1)
      B2 = GRID(I+1) - GRID(J+1)
      A1 = GRID(K) + GRID(L)
      A3 = GRID(K) - GRID(L)
      B1 = GRID(K+1) + GRID(L+1)
      B3 = GRID(K+1) - GRID(L+1)
      WORK(M  ) = A0 + A1
      WORK(M+4) = A0 - A1
      WORK(M+1) = B0 + B1
      WORK(M+5) = B0 - B1
      WORK(M+2) = A2 - B3
      WORK(M+6) = A2 + B3
      WORK(M+3) = B2 + A3
      WORK(M+7) = B2 - A3
      TEMP = WORK(M+2)*TRIGS(KB) - WORK(M+3)*TRIGS(KB+1)
      WORK(M+3) = WORK(M+2)*TRIGS(KB+1) + WORK(M+3)*TRIGS(KB)
      WORK(M+2) = TEMP
      TEMP = WORK(M+4)*TRIGS(KC) - WORK(M+5)*TRIGS(KC+1)
      WORK(M+5) = WORK(M+4)*TRIGS(KC+1) + WORK(M+5)*TRIGS(KC)
      WORK(M+4) = TEMP
      TEMP = WORK(M+6)*TRIGS(KD) - WORK(M+7)*TRIGS(KD+1)
      WORK(M+7) = WORK(M+6)*TRIGS(KD+1) + WORK(M+7)*TRIGS(KD)
      WORK(M+6) = TEMP
      J = J + 2
      K = K + 2
      L = L + 2
      KB = KB + 2
      KC = KC + 4
      KD = KD + 6
      M = M + 8
300   CONTINUE
C
      I = 1
      J = 1
      K = 73
      DO 440 L=1,4
      GRID(I) = WORK(J) + WORK(K)
      GRID(I+8) = WORK(J) - WORK(K)
      GRID(I+1) = WORK(J+1) + WORK(K+1)
      GRID(I+9) = WORK(J+1) - WORK(K+1)
      I = I + 2
      J = J + 2
      K = K + 2
440   CONTINUE
      DO 500 KB=9,65,8
      I = I + 8
      DO 460 L=1,4
      GRID(I) = WORK(J) + WORK(K)
      GRID(I+8) = WORK(J) - WORK(K)
      GRID(I+1) = WORK(J+1) + WORK(K+1)
      GRID(I+9) = WORK(J+1) - WORK(K+1)
      TEMP = GRID(I+8)*TRIGS(KB) - GRID(I+9)*TRIGS(KB+1)
      GRID(I+9) = GRID(I+8)*TRIGS(KB+1) + GRID(I+9)*TRIGS(KB)
      GRID(I+8) = TEMP
      I = I + 2
      J = J + 2
      K = K + 2
460   CONTINUE
500   CONTINUE
C
      I = 1
      L = 1
      KC = 1
      J = 49
      K = 97
      M = 17
      N = 33
      DO 660 LL=1,8
      A1 = GRID(J) + GRID(K)
      A3 = SIN60*(GRID(J)-GRID(K))
      B1 = GRID(J+1) + GRID(K+1)
      B3 = SIN60*(GRID(J+1)-GRID(K+1))
      WORK(L) = GRID(I) + A1
      A2 = GRID(I) - 0.5*A1
      WORK(L+1) = GRID(I+1) + B1
      B2 = GRID(I+1) - 0.5*B1
      WORK(N) = A2 + B3
      WORK(M) = A2 - B3
      WORK(M+1) = B2 + A3
      WORK(N+1) = B2 - A3
      I = I + 2
      J = J + 2
      K = K + 2
      L = L + 2
      M = M + 2
      N = N + 2
660   CONTINUE
      DO 700 KB=17,33,16
      L = L + 32
      M = M + 32
      N = N + 32
      KC = KC + 32
      DO 680 LL=1,8
      A1 = GRID(J) + GRID(K)
      A3 = SIN60*(GRID(J)-GRID(K))
      B1 = GRID(J+1) + GRID(K+1)
      B3 = SIN60*(GRID(J+1)-GRID(K+1))
      WORK(L) = GRID(I) + A1
      A2 = GRID(I) - 0.5*A1
      WORK(L+1) = GRID(I+1) + B1
      B2 = GRID(I+1) - 0.5*B1
      WORK(N) = A2 + B3
      WORK(M) = A2 - B3
      WORK(M+1) = B2 + A3
      WORK(N+1) = B2 - A3
      TEMP = WORK(M)*TRIGS(KB) - WORK(M+1)*TRIGS(KB+1)
      WORK(M+1) = WORK(M)*TRIGS(KB+1) + WORK(M+1)*TRIGS(KB)
      WORK(M) = TEMP
      TEMP = WORK(N)*TRIGS(KC) - WORK(N+1)*TRIGS(KC+1)
      WORK(N+1) = WORK(N)*TRIGS(KC+1) + WORK(N+1)*TRIGS(KC)
      WORK(N) = TEMP
      I = I + 2
      J = J + 2
      K = K + 2
      L = L + 2
      M = M + 2
      N = N + 2
680   CONTINUE
700   CONTINUE
C
      J = 49
      K = 97
      L = 144
      M = 96
      N = 48
      DO 900 I=1,47,2
      A1 = WORK(J) + WORK(K)
      A3 = SIN60 * (WORK(J)-WORK(K))
      B3 = SIN60 * (WORK(J+1)-WORK(K+1))
      B1 = WORK(J+1) + WORK(K+1)
      GRID(L+1) = WORK(I) + A1
      A2 = WORK(I) - 0.5*A1
      B2 = WORK(I+1) - 0.5*B1
      GRID(L) = WORK(I+1) + B1
      GRID(N+1) = A2 + B3
      GRID(M+1) = A2 - B3
      GRID(M) = B2 + A3
      GRID(N) = B2 - A3
      J = J + 2
      K = K + 2
      L = L - 2
      M = M - 2
      N = N - 2
900   CONTINUE
      GRID(1) = GRID(145)
C
      RETURN
      END
