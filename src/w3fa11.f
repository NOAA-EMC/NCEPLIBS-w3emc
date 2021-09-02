C> @file
C> @brief Computes coefficients for use in w3fa12.
C> @author Joe Sela @date 1980-10-28

C> Subroutine computes double precision  coefficients
C> used in generating  legendre polynomials in subr. w3fa12.
C> on a cray double precision is changed to real, dsqrt to sqrt.
C>
C> Program history log:
C> - Joe Sela 1980-10-28
C> - Ralph Jones 1984-06-01 0change to ibm vs fortran.
C> - Ralph Jones 1993-04-12 0changes for cray, double precision to real.
C>
C> @param[out] EPS Real coefficients used in computing legendre polynomials.
C> dimension of eps is (jcap+2)*(jcap+1)
C> @param[in] JCAP Zonal wave number thirty, etc.
C>
C> @author Joe Sela @date 1980-10-28

       SUBROUTINE W3FA11 (EPS,JCAP)
C
       REAL  EPS(*)
       REAL  A
C
       SAVE
C
         JCAP1 = JCAP + 1
         JCAP2 = JCAP + 2
C
         DO 100 LL = 1,JCAP1
           L = LL - 1
           JLE = (LL-1) * JCAP2
C
         DO 100 INDE = 2,JCAP2
           N = L + INDE - 1
           A=(N*N-L*L)/(4.0*N*N-1.0)
           EPS(JLE+INDE) = SQRT(A)
 100     CONTINUE
C
         DO 200 LL = 1,JCAP1
           JLE = (LL-1) * JCAP2
           EPS(JLE+1) = 0.0
 200     CONTINUE
C
         RETURN
       END
