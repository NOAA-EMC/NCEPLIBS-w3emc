C> @file
C> @brief Grid U-V to Earth U-V in north hem.
C> @author John Stackpole @date 1981-12-30

C> Given the grid-oriented wind components on a northern
C> hemisphere polar stereographic grid point, compute the Earth-
C> oriented wind components at that point. If the input winds
C> are at the north pole, the output components will be made
C> consistent with the WMO standards for reporting winds at the
C> north pole. (see office note 241 for WMO definition.)
C>
C> Program history log:
C> - John Stackpole 1981-12-30
C> - P. Chase 1988-10-13 Allow input and output to be the same
C> - Ralph Jones 1991-03-06 Change to cray cft77 fortran
C>
C> @param[in] FFID REAL I-displacement from point to north pole
C> @param[in] FFJD REAL J-displacement from point to north pole
C> @param[in] FGV REAL Grid-oriented V-component
C> @param[in] FGU REAL Grid-oriented U-component
C> @param[out] FU REAL Earth-oriented U-component, positive from west
C> may reference the same location as FGU.
C> @param[out] FV REAL Earth-oriented V-component, positive from south
C> may reference the same location as FGV.
C>
C> @note Calculate FFID and FFJD as follows...
C> FFID = real(ip - i)
C> FFJD = real(jp - j)
C> where (ip,jp) is the grid coordinates of the north pole and
C> (i,j) is the grid coordinates of the point where FGU and FGV
C> occur. See w3fc11 for a southern hemisphere companion subroutine.
C>
C> @author John Stackpole @date 1981-12-30
      SUBROUTINE W3FC07(FFID, FFJD, FGU, FGV, FU, FV)
C
      SAVE
C
      DATA  COS80 / 0.1736482 /
      DATA  SIN80 / 0.9848078 /

C     COS80 AND SIN80 ARE FOR WIND AT POLE
C     (USED FOR CO-ORDINATE ROTATION TO EARTH ORIENTATION)

      DFP = SQRT(FFID * FFID + FFJD * FFJD)
      IF (DFP .EQ. 0.0) THEN
        XFU = -(FGU * COS80 + FGV * SIN80)
        FV  = -(FGV * COS80 - FGU * SIN80)
      ELSE
        XFU = (FGU * FFJD - FGV * FFID) / DFP
        FV  = (FGU * FFID + FGV * FFJD) / DFP
      ENDIF
      FU = XFU
      RETURN
      END
