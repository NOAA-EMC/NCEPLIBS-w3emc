C> @file
C> @brief Cut a 65 x 65 grid to a nmc 1977 point grid.
C> @author Ralph Jones @date 1984-07-02

C> Extracts the NMC 1977 point octagon grid points out of
C> a 65x65 (4225 point) array.
C>
C> Program history log:
C> - Ralph Jones 1973-06-15
C> - Ralph Jones 1984-07-02 Convert to vs fortran
C> - Ralph Jones 1989-02-02 Convert to microsoft fortran 4.10
C> - Ralph Jones 1990-08-22 Convert to sun fortran 1.3
C> - Ralph Jones 1991-03-29 Convert to silicongraphics fortran
C> - Ralph Jones 1993-03-29 Add save statement
C>
C> @param[in] A REAL*4 (65 x 65 grid, 4225 point) array
C> grid is office note 84 type 27 or 1b hex.
C> @param[out] B 1977 point array (octagon) office note 84 type
C> 0 or hex 0.
C>
C> @note Arrays A and B may be the same array or be equivalenced,
C> in which case the first 1977 words of 'A' are written over.
C>
C> @author Ralph Jones @date 1984-07-02
      SUBROUTINE W3FI20(A,B)
C
      REAL          A(*)
      REAL          B(*)
C
      INTEGER       RB
      INTEGER       LBR(51)
      INTEGER       RBR(51)
C
      SAVE
C
      DATA LBR/479,543,607,671,735,799,863,927,991,1055,1119,1183,1247,
     &1311,1375,1440,1505,1570,1635,1700,1765,1830,1895,1960,2025,2090,
     &2155,2220,2285,2350,2415,2480,2545,2610,2675,2740,2805,2871,2937,
     &3003,3069,3135,3201,3267,3333,3399,3465,3531,3597,3663,3729/
C
      DATA RBR/497,563,629,695,761,827,893,959,1025,1091,1157,1223,1289,
     &1355,1421,1486,1551,1616,1681,1746,1811,1876,1941,2006,2071,2136,
     &2201,2266,2331,2396,2461,2526,2591,2656,2721,2786,2851,2915,2979,
     &3043,3107,3171,3235,3299,3363,3427,3491,3555,3619,3683,3747/
C
         N = 0
C
      DO 200 I = 1,51
        LB = LBR(I)
        RB = RBR(I)
C
      DO 100 J = LB,RB
        N = N + 1
        B(N) = A(J)
 100  CONTINUE
C
 200  CONTINUE
C
      RETURN
      END
