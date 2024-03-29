C> @file
C> @brief Computes 2.5 x 2.5 s. hemi. grid-scaler.
C> @author Joe Sela @date 1980-10-21

C> Computes 2.5 x 2.5 s. hemi. grid of 145 x 37 points
C> from spectral coefficients in a rhomboidal 30 resolution
C> representing a scaler field.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1980-10-21 | Joe Sela | Initial.
C> 1984-06-28 | Ralph Jones | Change to ibm vs fortran.
C> 1989-01-25 | Ralph Jones | Change to microsoft fortran 4.10.
C> 1990-06-12 | Ralph Jones | Change to sun fortran 1.3.
C> 1991-03-30 | Ralph Jones | Convert to silicongraphics fortran.
C> 1993-03-29 | Ralph Jones | Add save statement.
C> 1993-07-22 | Ralph Jones | Change double precision to real for cray.
C>
C> @param[in] FLN 961 complex coeff.
C> @param[in] PLN 992 real space for legendre polynomials.
C> @param[in] EPS 992 real space for coeffs. used in computing pln.
C> @param[in] FL 31 complex space for fourier coeff.
C> @param[in] WORK 144 real work space for subr. w3ft12()
C> @param[in] TRIGS 216 precomputed trig funcs. used in w3ft12(), computed by w3fa13()
C> @param[out] GN (145,37) grid values. 5365 point grid is type 30 or 1e o.n. 84
C>
C> @note This subroutine was optimized to run in a small amount of
C> memory, it is not optimized for speed, 70 percent of the time is
C> used by subroutine w3fa12() computing the legendre polynomials. Since
C> the legendre polynomials are constant they need to be computed
C> only once in a program. By moving w3fa12() to the main program and
C> computing pln as a (32,31,37) array and changing this subroutine
C> to use pln as a three dimension array you can cut the running time
C> 70 percent.
C>
C> @author Joe Sela @date 1980-10-21
      SUBROUTINE W3FT10(FLN,GN,PLN,EPS,FL,WORK,TRIGS)
C
       COMPLEX          FL( 31 )
       COMPLEX          FLN( 31 , 31 )
C
       REAL             COLRA
       REAL             EPS( 992)
       REAL             GN(145,37)
       REAL             PLN( 32 , 31 )
       REAL             TRIGS(216)
       REAL             WORK(144)
C
       SAVE
C
       DATA  PI    /3.14159265/
C
         DRAD = 2.5 * PI / 180.0
C
         DO 400 LAT = 1,37
           COLRA = (LAT-1) * DRAD
           CALL W3FA12(PLN,COLRA, 30 ,EPS)
C
           DO 100 L = 1, 31
             FL(L) = (0.,0.)
 100       CONTINUE
C
           DO 300 L = 1, 31
             I = 1
             FL(L) = FL(L)+CMPLX(PLN(I,L) * REAL(FLN(I,L)) ,
     &       PLN(I,L) * AIMAG(FLN(I,L)) )
C
             DO  200 I = 2, 30 ,2
               FL(L) = FL(L)-CMPLX(PLN(I,L) * REAL(FLN(I,L)) ,
     &         PLN(I,L) * AIMAG(FLN(I,L)) )
               FL(L) = FL(L)+CMPLX(PLN(I+1,L) * REAL(FLN(I+1,L)),
     &         PLN(I+1,L) * AIMAG(FLN(I+1,L)))
 200         CONTINUE
C
 300       CONTINUE
C
         CALL W3FT12(FL,WORK,GN(1,LAT ),TRIGS)
 400     CONTINUE
C
         RETURN
       END
