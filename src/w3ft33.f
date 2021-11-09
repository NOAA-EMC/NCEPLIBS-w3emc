C> @file
C> @brief Thicken thinned wafs grib grid 37-44
C> @author Ralph Peterson @date 1994

C> Subroutine thickens one thinned wafs grib grid to a
C> real array of 5329 numbers (73,73) 1.25 degree grid.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1994-??-?? | Ralph Peterson
C> 1994-11-07 | Ralph Jones | Add doc block, change call to 3 parameters.
C> Replace cos with table lookup.
C> 1995-06-02 | Ralph Peterson | Changes to correct miss-position
C> between + or - 8.75 n/s.
C> 1995-06-03 | Ralph Jones | Changes so 8 rows with 73 values
C> are not thickened, 10% faster.
C>
C> @param[in] AIN Real 3447 word array with unpacked thinned wafs
C> grib type 37-44.
C> @param[in] NSFLAG Integer = 1 AIN is wafs grib grid 37-40 n. hemi.
C> = -1 AIN is wafs grib grid 41-44 s. hemi.
C> @param[out] OUT Real (73,73) word array with thickened wafs grib grid 37-44.
C>
C> @remark The pole point for u and v wind components will have only
C> one point. If you need the pole row corrected see page 9 section
C> 1 in office note 388. You need both u and v to make the
C> correction.
C>
C> @author Ralph Peterson @date 1994
      SUBROUTINE W3FT33(AIN,OUT,NSFLAG)
C
         PARAMETER (NX=73,NY=73)
         PARAMETER (NIN=3447)
C
         REAL      AIN(*)
         REAL      OUT(NX,NY)
C
         INTEGER   IPOINT(NX)
C
         SAVE
C
      DATA  IPOINT/
     & 73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,
     & 70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,
     & 59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,
     & 42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,
     & 20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
C
         NXM   = NX - 1
         FNXM  = FLOAT(NXM)
C
C        TEST FOR GRIDS (37-40)
C
         IF (NSFLAG.GT.0) THEN
C
C          DO NOT THICKEN 8 ROWS WITH 73 VALUES, MOVE DATA
C          TO OUT ARRAY. GRIDS (37-40) N.
C
           IS = 0
           DO J = 1,8
             DO I = 1,NX
               IS       = IS + 1
               OUT(I,J) = AIN(IS)
             END DO
           END DO
C
           IE = NX * 8
           DO J = 9,NY
             NPOINT   = IPOINT(J)
             IS       = IE + 1
             IE       = IS + NPOINT - 1
             DPTS     = (FLOAT(NPOINT)-1.) / FNXM
             PW       = 1.0
             PE       = PW + DPTS
             OUT(1,J) = AIN(IS)
             VALW     = AIN(IS)
             VALE     = AIN(IS+1)
             DVAL     = (VALE-VALW)
             DO I = 2,NXM
               WGHT     = PE -FLOAT(IFIX(PE))
               OUT(I,J) = VALW + WGHT * DVAL
               PW       = PE
               PE       = PE + DPTS
               IF (IFIX(PW).NE.IFIX(PE)) THEN
                 IS   = IS + 1
                 VALW = VALE
                 VALE = AIN(IS+1)
                 DVAL = (VALE - VALW)
               END IF
             END DO
             OUT(NX,J) = AIN(IE)
           END DO
C
         ELSE
C
C         DO NOT THICKEN 8 ROWS WITH 73 VALUES, MOVE DATA
C         TO OUT ARRAY. GRIDS (41-44) S.
C
           IS = NIN - (8 * NX)
           DO J = 66,NY
             DO I = 1,NX
               IS       = IS + 1
               OUT(I,J) = AIN(IS)
             END DO
           END DO
C
           IE = 0
           DO J = 1,65
             NPOINT   = IPOINT(74-J)
             IS       = IE + 1
             IE       = IS + NPOINT - 1
             DPTS     = (FLOAT(NPOINT)-1.) / FNXM
             PW       = 1.0
             PE       = PW + DPTS
             OUT(1,J) = AIN(IS)
             VALW     = AIN(IS)
             VALE     = AIN(IS+1)
             DVAL     = (VALE-VALW)
             DO I = 2,NXM
               WGHT     = PE -FLOAT(IFIX(PE))
               OUT(I,J) = VALW + WGHT * DVAL
               PW       = PE
               PE       = PE + DPTS
               IF (IFIX(PW).NE.IFIX(PE)) THEN
                 IS   = IS + 1
                 VALW = VALE
                 VALE = AIN(IS+1)
                 DVAL = (VALE - VALW)
               END IF
             END DO
             OUT(NX,J) = AIN(IE)
           END DO
         END IF
C
         RETURN
         END
