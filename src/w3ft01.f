C> @file
C> @brief Interpolate values in a data field.
C> @author James McDonell @date 1984-06-27

C> For a given grid coordinate in a data array, estimates
C> a data value for that point using either a linear or quadratic
C> interpolation method.
C>
C> ### Program History Log:
C> Date | Programmer | Commment
C> -----|------------|---------
C> 1984-06-27 | James McDonell | Initial
C> 1989-11-01 | Ralph Jones | Change to cray cft77 fortran
C>
C> @param[in] STI Real*4 i grid coordinate of the point for which
C> an interpolated value is desired.
C> @param[in] STJ Real*4 j grid coordinate of the point for which
C> an interpolated value is desired.
C> @param[in] FLD Real*4 size(ii,jj) data field.
C> @param[in] II Integer*4 number of columns in 'fld'.
C> @param[in] JJ Integer*4 number of rows in 'fld'.
C> @param[in] NCYCLK Integer*4 code to specify if grid is cyclic or
C> not:
C> - = 0 Non-cyclic in ii, non-cyclic in jj
C> - = 1 Cyclic in ii, non-cyclic in jj
C> - = 2 Cyclic in jj, non-cyclic in ii
C> - = 3 Cyclic in ii, cyclic in jj
C> @param[in] LIN Integer*4 code specifying interpolation method:
C> - = 1 Linear interpolation
C> - .NE.1 Quadratic interpolation
C> @param[out] HI Real*4 data field value at (sti,stj) obtained
C> by interpolation.
C>
C> @author James McDonell @date 1984-06-27
      SUBROUTINE W3FT01(STI,STJ,FLD,HI,II,JJ,NCYCLK,LIN)
C
      REAL    ERAS(4)
      REAL    FLD(II,JJ)
      REAL    JY(4)
C
      I     = STI
      J     = STJ
      FI    = I
      FJ    = J
      XDELI = STI - FI
      XDELJ = STJ - FJ
      IP2   = I + 2
      IM1   = I - 1
      IP1   = I + 1
      JY(4) = J + 2
      JY(1) = J - 1
      JY(3) = J + 1
      JY(2) = J
      XI2TM = 0.0
      XJ2TM = 0.0
      IF (LIN.NE.1) THEN
        XI2TM = XDELI * (XDELI - 1.0) * 0.25
        XJ2TM = XDELJ * (XDELJ - 1.0) * 0.25
      ENDIF
      IF ((I.LT.2).OR.(J.LT.2))       GO TO 10
      IF ((I.GT.II-3).OR.(J.GT.JJ-3)) GO TO 10
C
C     QUADRATIC (LINEAR TOO) OK W/O FURTHER ADO SO GO TO 170
C
      GO TO 170
C
   10 CONTINUE
        ICYCLK = 0
        JCYCLK = 0
        IF (NCYCLK) 20,120,20
C
   20 CONTINUE
        IF (NCYCLK / 2 .NE. 0) JCYCLK = 1
        IF (NCYCLK .NE. 2)     ICYCLK = 1
        IF (ICYCLK) 30,70,30
C
   30 CONTINUE
        IF (I.EQ.1)      GO TO 40
        IF (I.EQ.(II-1)) GO TO 50
        IP2 = I + 2
        IM1 = I - 1
        GO TO 60
C
   40 CONTINUE
        IP2 = 3
        IM1 = II - 1
        GO TO 60
C
   50 CONTINUE
        IP2 = 2
        IM1 = II - 2
C
   60 CONTINUE
        IP1 = I + 1
C
   70 CONTINUE
        IF (JCYCLK) 80,120,80
C
   80 CONTINUE
        IF (J.EQ.1)      GO TO 90
        IF (J.EQ.(JJ-1)) GO TO 100
        JY(4) = J + 2
        JY(1) = J - 1
        GO TO 110
C
   90 CONTINUE
        JY(4) = 3
        JY(1) = JJ - 1
        GO TO 110
C
  100 CONTINUE
        JY(4) = 2
        JY(1) = JJ - 2
C
  110 CONTINUE
        JY(3) = J + 1
        JY(2) = J
C
  120 CONTINUE
        IF (LIN.EQ.1) GO TO 160
        IF (ICYCLK) 140,130,140
C
  130 CONTINUE
        IF ((I.LT.2).OR.(I.GE.(II-1)))  XI2TM = 0.0
C
  140 CONTINUE
        IF (JCYCLK) 160,150,160
C
  150 CONTINUE
        IF ((J.LT.2).OR.(J.GE.(JJ-1)))  XJ2TM = 0.0
C
  160 CONTINUE
C
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
C
        IF (I.LT.1)   I   = 1
        IF (IP1.LT.1) IP1 = 1
        IF (IP2.LT.1) IP2 = 1
        IF (IM1.LT.1) IM1 = 1
C
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
C
        IF (I.GT.II)   I   = II
        IF (IP1.GT.II) IP1 = II
        IF (IP2.GT.II) IP2 = II
        IF (IM1.GT.II) IM1 = II
C
  170 CONTINUE
      DO 180 K = 1,4
        J1 = JY(K)
C
C.....DO NOT ALLOW POINT OFF GRID,CYCLIC OR NOT
C
        IF (J1.LT.1)  J1 = 1
        IF (J1.GT.JJ) J1 = JJ
        ERAS(K) = (FLD(IP1,J1) - FLD(I,J1)) * XDELI + FLD(I,J1) +
     &  (FLD(IM1,J1) - FLD(I,J1) - FLD(IP1,J1) + FLD(IP2,J1)) * XI2TM
  180 CONTINUE
C
      HI = ERAS(2) + (ERAS(3) - ERAS(2)) * XDELJ + (ERAS(1) -
     &     ERAS(2) -  ERAS(3) + ERAS(4)) * XJ2TM
C
      RETURN
      END
