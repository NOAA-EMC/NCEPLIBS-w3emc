C> @file
C> @brief Computes 2.5x2.5 n. hemi. grid-vector.
C> @author Ralph Jones @date 1993-07-23

C> Computes 2.5 x 2.5 n. hemi. grid of 145 x 37 points
C> from spectral coefficients in a rhomboidal 30 resolution
C> representing a vector field.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1993-07-23 | Ralph Jones | New version of w3ft09(), takes out w3fa12()
C> makes pln 3 dimensions, pln is computed one time in main program, trades memory
C> for more speed. w3fa12() used 70% of cpu time.
C>
C> @param[in] VLN 992 complex coeff.
C> @param[in] PLN (32,31,37) real space with legendre polynomials
C> computed by w3fa12().
C> @param[in] FL 31 complex space for fourier coeff.
C> @param[in] WORK 144 work space for subr. w3ft12()
C> @param[in] TRIGS 216 precomputed trig funcs. used
C> in w3ft12, computed by w3fa13().
C> @param[in] RCOS 37 reciprocal cosine latitudes of
C> 2.5 x 2.5 grid must be computed before
C> first call to w3ft11 using sr w3fa13().
C> @param[out] GN (145,37) grid values. 5365 point grid is type 29 or 1d o.n. 84
C>
C> @note w3ft09() was optimized to run in a small amount of
C> memory, it was not optimized for speed, 70 percent of the time was
C> used by subroutine w3fa12() computing the legendre polynomials. Since
C> the legendre polynomials are constant they need to be computed
C> only once in a program. By moving w3fa12() to the main program and
C> computing pln as a (32,31,37) array and changing this subroutine
C> to use pln as a three dimension array the running time was cut
C> 70 percent. Add following code to main program to compute eps, pln,
C> trigs, and rcos one time in program.
C> @code
C>       DOUBLE PRECISION EPS(992)
C>       DOUBLE PRECISION COLRA
C>
C>       REAL             PLN( 32, 31, 37 )
C>       REAL             RCOS(37)
C>       REAL             TRIGS(216)
C>
C>       DATA  PI /3.14159265/
C>
C>       DRAD = 2.5 * PI / 180.0
C>       CALL W3FA11(EPS,30)
C>       CALL W3FA13(TRIGS,RCOS)
C>       DO LAT = 1,37
C>         COLRA = (LAT - 1) * DRAD
C>         CALL W3FA12 (PLN(1,1,LAT), COLRA, 30, EPS)
C>       END DO
C> @endcode
C>
C> @author Ralph Jones @date 1993-07-23
      SUBROUTINE W3FT39(VLN,GN,PLN,FL,WORK,TRIGS,RCOS)
C
       COMPLEX          FL( 31 )
       COMPLEX          VLN( 32 , 31 )
C
       REAL             GN(145,37)
       REAL             PLN( 32, 31, 37 )
       REAL             RCOS(37)
       REAL             TRIGS(216)
       REAL             WORK(144)
C
       SAVE
C
         DO 400 LAT = 2,37
           LATN  = 38 - LAT
C
           DO 100 L = 1, 31
             FL(L) = (0.,0.)
 100       CONTINUE
C
             DO 300 L = 1, 31
C
               DO 200 I = 1, 32
                 FL(L) = FL(L) + CMPLX(PLN(I,L,LAT) * REAL(VLN(I,L)),
     &           PLN(I,L,LAT) * AIMAG(VLN(I,L)) )
 200           CONTINUE
C
             FL(L)=CMPLX(REAL(FL(L))*RCOS(LAT),AIMAG(FL(L))*RCOS(LAT))
 300         CONTINUE
C
         CALL W3FT12(FL,WORK,GN(1,LATN),TRIGS)
C
 400     CONTINUE
C
C***   POLE ROW=CLOSEST LATITUDE ROW
C
         DO 500 I = 1,145
           GN(I,37) = GN(I,36)
 500     CONTINUE
C
         RETURN
       END
