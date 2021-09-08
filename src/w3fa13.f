C> @file
C> @brief Computes Trig Functions
C> @author Joe Sela @date 1980-11-21

C> Computes trig functions used in 2.5 by 2.5 lat,lon
C> mapping routines. w3fa13() must be called at least once before
C> calls to w3ft08(), w3ft09(), w3ft10(), w3ft11().
C>
C> Program history log:
C> - Joe Sela 1980-11-21
C> - Ralph Jones 1984-06-01 Change to vs fortran
C>
C> @param[out] TRIGS 216 trig values, used by subroutine w3fa12().
C> @param[out] RCOS 37 colatitudes used by subroutines w3ft09() ,w3ft11()
C>
C> @author Joe Sela @date 1980-11-21
      SUBROUTINE W3FA13(TRIGS,RCOS)
C
       REAL            RCOS(*)
       REAL            TRIGS(*)
C
       SAVE
C
       DATA  PI    /3.14159265358979323846/
C
         N = 144
         MODE = 3
         DRAD = 2.5*PI/180.
C
         DO 100 LAT = 2,37
           ARG = (LAT-1)*DRAD
           RCOS(LAT) = 1./SIN(ARG)
 100     CONTINUE
C
         RCOS(1) = 77777.777
         IMODE = IABS(MODE)
         NN = N
         IF (IMODE.GT.1.AND.IMODE.LT.6) NN = N/2
         ANGLE = 0.0
         DEL = (PI+PI)/FLOAT(NN)
         L = NN+NN
C
         DO 200 I = 1,L,2
           TRIGS(I) = COS(ANGLE)
           TRIGS(I+1) = SIN(ANGLE)
           ANGLE = ANGLE+DEL
 200     CONTINUE
C
         IF (IMODE.EQ.1) RETURN
         IF (IMODE.EQ.8) RETURN
         ANGLE = 0.0
         DEL = 0.5*DEL
         NH = (NN+1)/2
         L = NH+NH
         LA = NN+NN
C
         DO 300 I = 1,L,2
           TRIGS(LA+I) = COS(ANGLE)
           TRIGS(LA+I+1) = SIN(ANGLE)
           ANGLE = ANGLE+DEL
 300     CONTINUE
C
         IF (IMODE.LE.3) RETURN
         DEL = 0.5*DEL
         ANGLE = DEL
         LA = LA+NN
C
         DO 400 I = 2,NN
           TRIGS(LA+I) = 2.0*SIN(ANGLE)
           ANGLE = ANGLE+DEL
 400     CONTINUE
C
         RETURN
       END
