C> @file
C> @brief Interpolate precipitation to specific point.
C> @author Robert Hirano @date 1979-08-05

C> Interpolate, using a fancy non-linear method,
C> gridded quantitative precipitation forecasts to a specific
C> interior point. One point (e.g. an observation station)
C> per call to w3ft02().
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1979-08-05 | Robert Hirano | Initial.
C> 1996-06-23 | Farley | Converted to cray fortran 77
C>
C> @param[in] RAIN Real*4 grid field of (forecast) precipitation.
C> @param[in] IMAX Integer*4 i-dimension of rain field.
C> @param[in] JMAX Integer *4 j-dimension of rain field.
C> @param[in] PI Real*4 i-coordinate of interpolation point.
C> @param[in] PJ Real*4 j-coordinate of interpolation point.
C> @param[out] AMOUNT Real*4 amount of precip interpolated to pi,pj.
C>
C> @author Robert Hirano @date 1979-08-05
      SUBROUTINE W3FT02 (RAIN, IMAX, JMAX, PI, PJ, AMOUNT)
C
C        INTERPOLATE PRECIPITATION FROM RAIN FIELD
C        TO INTERNAL POINT (PI,PJ).  RESULT IN AMOUNT
C
         real RAIN(IMAX,JMAX)
C
C              CHECK FOR INTERPOLATION POINT OUTSIDE GRID
C
         AMOUNT = 0.
         IF(PI.LE.1.OR.PI.GE.IMAX)  GO TO 150
         IF(PJ.LE.1.OR.PJ.GE.JMAX)  GO TO 150
C
C        SET UP RAIN AMMOUNTS AT CORNERS OF BOX SURROUNDING POINT (PI,PJ
C
C             R2          R4
C
C                 (PI,PJ)
C
C             R1          R3
C
         I=PI
         J=PJ
         R1=RAIN(I  ,J  )
         R2=RAIN(I  ,J+1)
         R3=RAIN(I+1,J  )
         R4=RAIN(I+1,J+1)
C
C        CHECK FOR NO RAIN AT ALL
C
         IF(AMAX1(R1,R2,R3,R4).LE.0.)  GO TO 150
C
C        GOT SOME  --  FIND APPROPRIATE SECTOR AND SECTION
C        OF THE GRID BOX IN WHICH THE STATION IS LOCATED
C
         AI = PI-I
         AJ=PJ-J
         X = 0.5
C
C        MEANINOF IC FOR SECTORS (K=1) OR SECTIONS (K=2)
C
C             2    4
C
C             1    3
C
C        ALSO REFERENCED AS
C
C             TOP       DIAGONAL       /    T    D
C                                      /
C             NEAR      RIGHT          /    N    R
C
         DO   1  K=1,2
           IF(AI.GT.X)  GO TO 2
             IF(AJ.GT.X)  GO TO 4
               IC = 1
               GO TO 10
    4        CONTINUE
               IC = 2
               GO TO 10
    2      CONTINUE
             IF(AJ.GT.X)  GO TO 6
               IC = 3
               GO TO 10
    6        CONTINUE
               IC = 4
   10      CONTINUE
           IF(K.NE.1)  GO TO 16
C
C            SET UP SECTORS  THIS BUSINESS IN EFFECT ROTATES THE SECTORS
C            FOR CONVENIENCE IN LATER INTERPOLATIONS
C
             GO TO (11, 12, 13, 14), IC
   11        CONTINUE
               R = R1
               RT = R2
               RR = R3
               RD = R4
               GO TO 15
   12        CONTINUE
               R = R2
               RT = R1
               RR = R4
               RD = R3
               AJ = 1. - AJ
               GO TO 15
   13        CONTINUE
               R = R3
               RT = R4
               RR = R1
               RD = R2
               AI = 1. - AI
               GO TO 15
   14         CONTINUE
               R = R4
               RT = R3
               RR = R2
               RD = R1
               AI = 1. - AI
               AJ = 1. - AJ
   15        CONTINUE
C
C            IF NO RAIN IN CORNER SECTTOR  WHERE STATION IS - QUIT
C
             IF(R.LE.0.)  GO TO 150
             X = 0.5 * X
   16      CONTINUE
    1    CONTINUE
C
C        INTERPOLATE TO STATION IN EASY (NON-CORNER) SECTIONS
C
         GO TO (21, 22, 23, 24), IC
   21    CONTINUE
           AMOUNT = R
           GO TO 150
   22    CONTINUE
           RC = RT
           RX = AJ
           GO TO 120
   23    CONTINUE
           RC = RR
           RX = AI
  120    CONTINUE
         IF(RC.GT. 0.)  GO TO 130
           AMOUNT = R - R*(RX-X)/X
           GO TO  150
  130    CONTINUE
           AMOUNT = R + (0.5*(R+RC)-R)*(RX-X)/X
           GO TO 150
   24    CONTINUE
C
C        CORNER (CENTER OF BOX) SECTION
C
           AA = AMAX1(RR, RT, RD)
           IF(AA.GT.0.)  GO TO 30
             RS = 0.
             RU = 0.
             RD = 0.
             GO TO 37
   30      CONTINUE
             IF(RR.GT.0.)  GO TO 32
               RS = 0.
               RRD = 0.
   33        CONTINUE
               IF(RT.GT.0.)  GO TO 34
                 RU = 0.
                 RTD = 0.
                 GO TO 35
   34          CONTINUE
                 RU = 0.5 * (R+RT)
                 IF(RD.GT.0.)  GO TO 36
                   RTD = 0.
                   GO TO 35
   36            CONTINUE
                   RTD = 0.5 * (RT + RD)
                   GO TO 35
   32        CONTINUE
               RS = 0.5 * (R+RR)
               IF(RD.GT.0.)  GO TO 38
                 RRD = 0.
                 GO TO 33
   38          CONTINUE
                 RRD = 0.5 * (RD + RR)
                 GO TO 33
   35        CONTINUE
               RD = 0.25 * (RS + RU + RTD + RRD)
               IF(RS.LE.0. .AND. RTD.LE.0.) RD = 0.
               IF(RU.LE.0..AND.RRD.LE.0.)  RD=0.
               RU = RU + (RD-RU) * (AI-X)/X
   37      CONTINUE
           R = R + (RS-R) * (AI-X)/X
           AMOUNT = R + (RU-R) * (AJ-X)/X
  150    CONTINUE
         RETURN
C
         END
