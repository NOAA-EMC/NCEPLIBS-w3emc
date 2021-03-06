C> @file
C
C> SUBPROGRAM: W3FT00         DATA FIELD TRANFORMATION SUBROUTINE
C>   AUTHOR: MCDONELL, J.     ORG: W345        DATE: SEPTEMBER, 1974
C>           HOWCROFT, J.
C>   UPDATE: JONES,R.E.       ORG: W342        DATE: 27 JUN 84
C>
C> ABSTRACT: TRANSFORMS DATA CONTAINED IN A GRID ARRAY BY TRANSLATION,
C>   ROTATION ABOUT A COMMON POINT AND DILATATION TO A NEW GRID ARRAY.
C>
C> PROGRAM HISTORY LOG:
C>   74-09-01  J.MCDONELL
C>   84-06-27  R.E.JONES   CHANGE TO IBM VS FORTRAN
C>
C> USAGE:  CALL W3FT00 (FA,FB,IA,JA,IB,JB,AIP,AJP,BIP,BJP,SC,ARG,LIN)
C>
C>   INPUT VARIABLES:
C>     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C>     ------ --------- -----------------------------------------------
C>     FA     ARG LIST  REAL  ORIGIONAL FIELD DIMENSIONED (IA,IJ)
C>     IA     ARG LIST  INTEGER  I-DIMENSION OF THE INPUT ARRAY FA
C>     JA     ARG LIST  INTEGER  J-DIMENSION OF THE INPUT ARRAY FA
C>     IB     ARG LIST  INTEGER  I-DIMENSION OF THE OUTPUT ARRAY FB
C>     JB     ARG LIST  INTEGER  J-DIMENSION OF THE OUTPUT ARRAY FB
C>     AIP    ARG LIST  REAL   COMMON POINT I-COORDINATE OF THE ORIGIONAL
C>                      FIELD ASSUMING A RIGHT HAND CARTESIAN COORDINATE
C>                      SYSTEM. THE POINT NEED NOT BE IN EITHER GRID AND
C>                      CAN HAVE FRACTIONAL INDICES).
C>     AJP    ARG LIST  REAL  COMMON POINT J-COORDINATE AS AIP ABOVE
C>     BIP    ARG LIST  REAL  COMMON POINT I-COORDINATE FOR TRANSFORMED
C>                      GRID
C>     BJP    ARG LIST  REAL  COMMON POINT J-COORDINATE FOR TRANSFORMED
C>                      GRID
C>     SC     ARG LIST  REAL  SCALE CHANGE (DILATION) EXPRESSED AS
C>                      A RATIO OF THE TRANSFORMED TO THE ORIGIONAL FIELD
C>     ARG    ARG LIST  REAL  DEGREE MEASURE OF THE ANGLE REQUIRED TO
C>                      ROTATE THE J-ROW OF THE ORIGIONAL GRID INTO
C>                      COINCIDENCE WITH THE NEW GRID. (+ COUNTER-
C>                      CLOCKWISE, - CLOCKWISE)
C>     LIN    ARG LIST  INTEGER  INTERPOLATION METHOD SWITCH
C>                      .EQ. 1 BILINEAR INTERPOLATION
C>                      .NE. 1 BIQUADRATIC INTERPOLATION
C>
C>   OUTPUT VARIABLES:
C>     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
C>     ------ --------- -----------------------------------------------
C>     FB     ARG LIST  REAL  TRANSFORMED FIELD DIMENSIONED (IB,JB)
C>
C>   SUBPROGRAMS CALLED:
C>     NAMES                                                   LIBRARY
C>     ------------------------------------------------------- --------
C>     SIN    COS                                              SYSTEM
C>
C>   REMARKS: IN GENERAL 'FA' AND 'FB' CANNOT BE EQUIVALENCED
C>     ALTHOUGH THERE ARE SITUATIONS IN WHICH IT WOULD BE SAFE TO DO
C>     SO. CARE SHOULD BE TAKEN THAT ALL OF THE NEW GRID POINTS LIE
C>     WITHIN THE ORIGIONAL GRID, NO ERROR CHECKS ARE MADE.
C>
C> ATTRIBUTES:
C>   LANGUAGE: CRAY CFT77 FORTRAN
C>   MACHINE:  CRAY Y-MP8/832
C>
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
