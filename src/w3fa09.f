C> @file
C> @brief Temperature to saturation vapor pressure.
C> @author P. Chase @date 1978-10-01

C> Computes saturation vapor pressure in kilopascals given temperataure in kelvins.
C>
C> Program history log:
C> - P. Chase 1978-10-01  P.CHASE
C> - Ralph Jones 1984-06-26 Change to ibm vs fortran.
C> - Ralph Jones 1984-06-26 Change to microsoft fortran 4.10.
C> - Ralph Jones 1990-06-08 Change to sun fortran 1.3.
C> - Ralph Jones 1991-03-29 Convert to silicongraphic fortran.
C> - Ralph Jones 1993-03-29 Add save statement.
C> - Ralph Jones 1995-09-25 Change tk to cray 64 bit real, change double.
C> precision to cray 64 bit real.
C>
C> @param[in] TK REAL*8 Temperature in kelvins. if tk < 223.16, the value
C> 223.16 will be used.  if tk > 323.16, the value 323.16
C> will be used as the argument. 'tk' itself is unchanged.
C> @return VP Saturation vapor pressure in kilopascals 0.0063558 < VP < 12.3395.
C>
C> @note W3FA09 may be declared real*8 so that a real*8 value is
C> returned, but no increase in accuracy is implied.
C>
C> @author P. Chase @date 1978-10-01
      REAL FUNCTION W3FA09 (TK)
C
C THE CHEBYSHEV COEFFICIENTS ARE IN ARRAY C, LOW-ORDER TERM FIRST.
C
      REAL   C(9)
      REAL   ARG,H0,H1,H2
C
      SAVE
C
      DATA  C     /
     & 0.313732865927E+01, 0.510038215244E+01, 0.277816535655E+01,
     & 0.102673379933E+01, 0.254577145215E+00, 0.396055201295E-01,
     & 0.292209288468E-02,-0.119497199712E-03,-0.352745603496E-04/
C
C SCALE TK TO RANGE -2, +2 FOR SERIES EVALUATION.  INITIALIZE TERMS.
C
      ARG = -1.09264E1+4.0E-2*AMAX1(223.16,AMIN1(323.16,TK))
      H0  = 0.0
      H1  = 0.0
C
C EVALUATE CHEBYSHEV POLYNOMIAL
C
      DO 10 I=1,9
        H2 = H1
        H1 = H0
        H0 = ARG * H1 - H2 + C(10-I)
 10   CONTINUE
      W3FA09 = 0.5 * (C(1) - H2 + H0)
      RETURN
      END
