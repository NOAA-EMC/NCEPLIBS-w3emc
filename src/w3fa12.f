C> @file
C> @brief Computes legendre polynomials at a given latitude.
C> @author Joe Sela @date 1980-10-28
C>
C> Subroutine computes legendre polynomials at a given latitude.
C>
C> Program history log:
C> - Joe Sela 1980-10-20
C> - Ralph Jones 1984-06-01 Change to ibm vs fortran.
C> - Ralph Jones 1993-04-12 Changes for cray, double precision to real.
C>
C> @param[out] PLN Real locations contain legendre
C> polynomials, size is (jcap+2)*(jcap+1)
C> @param[in] COLRAD Colatitude in radians of desired point.
C> @param[in] JCAP For rhomboiadal truncation of zonal wave
C> @param[in] EPS Coeff. used in recursion equation.
C> Dimension of eps is (jcap+2)*(jcap+1)
C>
C> @author Joe Sela @date 1980-10-28
       SUBROUTINE W3FA12(PLN,COLRAD,JCAP,EPS)
       REAL A
       REAL B
       REAL COLRAD
       REAL COS2
       REAL EPS(*)
       REAL FL
       REAL PROD
       REAL P1
       REAL P2
       REAL P3
       REAL SINLAT
       REAL PLN(*)
C
       SAVE
C
         SINLAT = COS(COLRAD)
         COS2   = 1.0 - SINLAT * SINLAT
         PROD   = 1.0
         A      = 1.0
         B      = 0.0
         JCAP1  = JCAP+1
         JCAP2  = JCAP+2
C
         DO 300 LL = 1,JCAP1
           L   = LL - 1
           FL  = L
           JLE = L * JCAP2
           IF (L.EQ.0) GO TO 100
             A = A + 2.0
             B = B + 2.0
             PROD = PROD * COS2 * A / B
 100       CONTINUE
           P1 = SQRT (0.5 * PROD)
           PLN(JLE+1) = P1
           P2 = SQRT(2.0 * FL + 3.0) * SINLAT * P1
           PLN(JLE+2) = P2
C
           DO 200 N = 3,JCAP2
             LINDEX = JLE + N
             P3 = (SINLAT*P2 - EPS(LINDEX-1)*P1)/EPS(LINDEX)
             PLN(LINDEX) = P3
             P1 = P2
             P2 = P3
200        CONTINUE
300      CONTINUE
         RETURN
C
       END
