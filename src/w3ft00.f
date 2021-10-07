C> @file
C> @brief Data field tranformation subroutine.
C> @author J. McDonell @date 1974-09-01

C> Transforms data contained in a grid array by translation, rotation about a
C> common point and dilatation to a new grid array.
C>
C> ### Program History Log:
C> Date | Programmer | Comments
C> -----|------------|---------
C> 1974-09-01 | J. McDonell | Initial.
C> 1984-06-27 | Ralph Jones | Change to ibm vs fortran.
C>
C> @param[in] IA (Integer) i-dimension of the input array fa
C> @param[in] JA (Integer) j-dimension of the input array fa
C> @param[in] IB (Integer) i-dimension of the output array fb
C> @param[in] JB (Integer) j-dimension of the output array fb
C> @param[in] SC (Real) Scale change (dilation) expressed as a ratio of the
C> transformed to the origional field.
C> @param[in] ARG (Real) Degree measure of the angle required to rotate the
C> j-row of the origional grid into coincidence with the new grid. (+ counter-
C> clockwise, - clockwise)
C> @param[in] LIN (Integer) Interpolation method switch
C> - .eq. 1 bilinear interpolation
C> - .ne. 1 biquadratic interpolation
C> @param FLD
C> @param B
C> @param CIP
C> @param CJP
C> @param FIPB
C> @param FJPB
C>
C> @remark In general 'fa' and 'fb' cannot be equivalenced although there are
C> situations in which it would be safe to do so. care should be taken that
C> all of the new grid points lie within the origional grid, no error checks
C> are made.
C>
C> @author J. McDonell @date 1974-09-01
      SUBROUTINE W3FT00(FLD,B,IA,JA,IB,JB,CIP,CJP,FIPB,FJPB,SC,ARG,LIN)
C
      REAL  B(IB,JB)
      REAL  ERAS(4)
      REAL  FLD(IA,JA)
C
      EQUIVALENCE (CI,STI), (CJ,STJ)
C
      THETA = ARG * (3.14159 / 180.0)
      SINT  = SIN (THETA)
      COST  = COS (THETA)
C
      DO 180 JN = 1,JB
        FJN   = JN
        FJ    = FJN - FJPB
      DO 180 IN = 1,IB
        FIN   = IN
        FI    = FIN - FIPB
        IOFF  = 0
        JOFF  = 0
        KQUAD = 0
        CI    = CIP + SC * (FI * COST - FJ * SINT)
        CJ    = CJP + SC * (FI * SINT + FJ * COST)
        IM    = CI
        JM    = CJ
        IF ((IM - 1).GT.0) GO TO 20
        IF ((IM - 1).EQ.0) GO TO 40
          II   = 1
          IOFF = 1
          GO TO 50
C
 20   CONTINUE
        IF ((IA - IM - 1).GT.0) GO TO 50
        IF ((IA - IM - 1).EQ.0) GO TO 40
          II   = IA
          IOFF = 1
          GO TO 50
C
 40   CONTINUE
        KQUAD = 5
C
 50   CONTINUE
        IF ((JM - 1).GT.0) GO TO 70
        IF ((JM - 1).EQ.0) GO TO 90
        JJ   = 1
        JOFF = 1
        GO TO 100
C
 70   CONTINUE
        IF ((JA - JM - 1).GT.0) GO TO 100
        IF ((JA - JM - 1).EQ.0) GO TO 90
        JJ   = JA
        JOFF = 1
        GO TO 100
C
 90   CONTINUE
        KQUAD = 5
C
 100  CONTINUE
        IF ((IOFF + JOFF) .EQ. 0)  GO TO 120
        IF ((IOFF + JOFF) .EQ. 2)  GO TO 110
        IF (IOFF .EQ. 1)  JJ = CJ
        IF (JOFF .EQ. 1)  II = CI
C
 110  CONTINUE
        B(IN,JN) = FLD(II,JJ)
        GO TO 180
C
 120  CONTINUE
        I     = STI
        J     = STJ
        FIX   = I
        XDELI = STI - FIX
        FJX   = J
        XDELJ = STJ - FJX
        IF ((KQUAD - 5).EQ.0) GO TO 140
C
        IF ((LIN-1).NE.0) GO TO 150
C
 140  CONTINUE
        ERAS(1) = FLD(I,J)
        ERAS(4) = FLD(I,J+1)
        ERAS(2) = ERAS(1) + (FLD(I+1,J)   - ERAS(1)) * XDELI
        ERAS(3) = ERAS(4) + (FLD(I+1,J+1) - ERAS(4)) * XDELI
        DI      = ERAS(2) + (ERAS(3)      - ERAS(2)) * XDELJ
        GO TO 170
C
 150  CONTINUE
        XI2TM = XDELI * (XDELI - 1.0) * 0.25
        XJ2TM = XDELJ * (XDELJ - 1.0) * 0.25
        J1 = J - 1
C
      DO 160 K = 1,4
        ERAS(K) = (FLD(I+1,J1) - FLD(I,J1)) * XDELI + FLD(I,J1) +
     & (FLD(I-1,J1) - FLD(I,J1) - FLD(I+1,J1) + FLD(I+2,J1)) * XI2TM
        J1 = J1 + 1
 160  CONTINUE
C
        DI = ERAS(2) + (ERAS(3) - ERAS(2)) * XDELJ + (ERAS(1) -
     &       ERAS(2) -  ERAS(3) + ERAS(4)) * XJ2TM
C
 170  CONTINUE
        B(IN,JN) = DI
C
 180  CONTINUE
C
      RETURN
      END
