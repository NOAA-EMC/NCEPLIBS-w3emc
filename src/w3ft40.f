C> @file
C> @brief Computes 2.5 x 2.5 s. hemi. grid-scaler
C> @author Ralph Jones @date 1993-07-23

C> Computes 2.5 x 2.5 s. hemi. grid of 145 x 37 points
C> from spectral coefficients in a rhomboidal 30 resolution
C> representing a scaler field.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1993-07-23 | Ralph Jones | New version of w3ft10(), takes out w3fa12()
C> makes pln 3 dimensions, pln is computed one time in main program, trades memory
C> for more speed. w3fa12() used 70% of cpu time.
C>
C> @param[in] FLN 961 complex coeff.
C> @param[in] PLN (32,31,37) real space with legendre polynomials
C> computed by w3fa12().
C> @param[in] FL 31 complex space for fourier coeff.
C> @param[in] WORK 144 real work space for subr. w3ft12()
C> @param[in] TRIGS 216 precomputed trig funcs. used
C> in w3ft12, computed by w3fa13().
C> @param[out] GN (145,37) grid values. 5365 point grid is type 30 or 1e o.n. 84
C>
C> @note w3ft10() was optimized to run in a small amount of
C> memory, it was not optimized for speed, 70 percent of the time was
C> used by subroutine w3fa12() computing the legendre polynomials. Since
C> the legendre polynomials are constant they need to be computed
C> only once in a program. By moving w3fa12() to the main program and
C> computing pln as a (32,31,37) array and changing this subroutine
C> to use pln as a three dimension array the running time was cut
C> 70 percent. Add following code to main program to compute eps, pln,
C> trigs, and rcos one time in program.
C> @code
C>       DOUBLE PRECISION EPS(992)    [CHANGE TO REAL ON CRAY]
C>       DOUBLE PRECISION COLRA       [CHANGE TO REAL ON CRAY]
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
C>       END DOC
C> @endcode
C>
C> @author Ralph Jones @date 1993-07-23
      SUBROUTINE W3FT40(FLN,GN,PLN,FL,WORK,TRIGS)
C
       COMPLEX          FL( 31 )
       COMPLEX          FLN( 31 , 31 )
C
       REAL             GN(145,37)
       REAL             PLN( 32, 31, 37 )
       REAL             TRIGS(216)
       REAL             WORK(144)
C
       SAVE
C
         DO 400 LAT = 1,37
C
           DO 100 L = 1, 31
             FL(L) = (0.,0.)
 100       CONTINUE
C
           DO 300 L = 1, 31
             I = 1
             FL(L) = FL(L)+CMPLX(PLN(I,L,LAT) * REAL(FLN(I,L)) ,
     &       PLN(I,L,LAT) * AIMAG(FLN(I,L)) )
C
             DO  200 I = 2, 30 ,2
               FL(L) = FL(L)-CMPLX(PLN(I,L,LAT) * REAL(FLN(I,L)) ,
     &         PLN(I,L,LAT) * AIMAG(FLN(I,L)) )
               FL(L) = FL(L)+CMPLX(PLN(I+1,L,LAT) * REAL(FLN(I+1,L)),
     &         PLN(I+1,L,LAT) * AIMAG(FLN(I+1,L)))
 200         CONTINUE
C
 300       CONTINUE
C
         CALL W3FT12(FL,WORK,GN(1,LAT ),TRIGS)
 400     CONTINUE
C
         RETURN
       END
