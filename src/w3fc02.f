C> @file
C> @brief Grid U,V wind comps. to dir. and speed.
C> @author John Stackpole @date 1981-12-30

C> Given the grid-oriented wind components on a northern
C> hemisphere polar stereographic grid point, compute the direction
C> and speed of the wind at that point. Input winds at the north
C> pole point are assumed to have their components follow the wmo
C> standards for reporting winds at the north pole.
C> (see office note 241 for wmo definition). Output direction
C> will follow wmo convention.
C>
C> Program history log:
C> - John Stackpole 1981-12-30
C> - Ralph Jones 1989-01-20 Convert to microsoft fortran 4.10.
C> - Ralph Jones 1990-06-11 Convert to sun fortran 1.3.
C> - Ralph Jones 1991-03-30 Convert to silicongraphics fortran.
C> - Ralph Jones 1993-03-29 Add save statement.
C> - Ralph Jones 1995-08-09 Compile on cray.
C>
C> @param[in] FFID REAL*4 I(north pole) - i(point).
C> @param[in] FFJD REAL*4 J(north pole) - j(point).
C> @param[in] FGU REAL*4 Grid-oriented u-component.
C> @param[in] FGV REAL*4 Grid-oriented v-component.
C>
C> @param[out] DIR REAL*4 Wind direction, degrees.
C> @param[out] SPD REAL*4 Wind speed.
C>
C> @note This job will not vectorize on a cray.
C>
C> @author John Stackpole @date 1981-12-30
      SUBROUTINE W3FC02(FFID,FFJD,FGU,FGV,DIR,SPD)
C
      SAVE
C
      SPD = SQRT(FGU * FGU + FGV * FGV)
      IF (SPD.NE.0.) GO TO 1000
         FGU = 0.
         FGV = 0.
         GO TO 3000
 1000 CONTINUE
      DFP = SQRT(FFID * FFID + FFJD * FFJD)
      IF (DFP.NE.0.) GO TO 2000
         XLAM = ACOS(FGU / SPD)
         XLAM = XLAM * 57.29578
         IF (FGV.LT.0.) DIR = 170. + XLAM
         IF ((FGV.GT.0.).AND.(XLAM.LT.170.)) DIR = 170. - XLAM
         IF ((FGV.GT.0.).AND.(XLAM.GE.170.)) DIR = 530. - XLAM
         IF ((ABS(FGV).LE.0.001).AND.(FGU.GT.0.)) DIR = 170.
         IF ((ABS(FGV).LE.0.001).AND.(FGU.LT.0.)) DIR = 350.
         GO TO 3000
 2000 CONTINUE
         CAL = FFJD / DFP
         SAL = FFID / DFP
         U = FGU * CAL - FGV * SAL
         V = FGU * SAL + FGV * CAL
         DIR = 57.29578 * ATAN2(U,V) + 180.
 3000 CONTINUE
      RETURN
      END
