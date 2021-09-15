C> @file
C> @brief U-V Comps from Earth to north hem grid.
C> @author John Stackpole @date 1981-12-30

C> Given the Earth-oriented wind components on a northern
C> hemisphere polar stereographic grid point, compute the grid-
C> oriented components at that point. Input wind components at the
C> north pole point are assumed to conform to
C> the 'WMO' standards for reporting winds at the north pole, with
C> the output components computed relative to the X-Y axes on the
C> grid. (see office note 241 for WMO definition.)
C>
C> Program history log:
C> - John Stackpole 1981-12-30
C> - P. Chase 1988-10-18 Let output variables overlay input.
C> - Ralph Jones 1991-03-06 Change to cray cft77 fortran.
C>
C> @param[in] FFID REAL I-displacement from point to north pole in
C> grid units.
C> @param[in] FFJD REAL J-displacement from point to north pole in
C> grid units.
C> @param[in] FU REAL Earth-oriented u-component, positive from west.
C> @param[in] FV REAL Earth-oriented v-component, positive from east.
C> @param[out] FGU REAL Grid-oriented u-component. May reference
C> same location as FU.
C> @param[out] FGV REAL Grid-oriented v-component. May reference
C> same location as FV.
C>
C> @note FFID and FFJD may be calculated as followS.....
C> FFID = real(ip - i)
C> FFJD = real(jp - j)
C> where (ip, jp) are the grid coordinates of the north pole and
C> (i,j) are the grid coordinates of the point.
C>
C> @author John Stackpole @date 1981-12-30
      SUBROUTINE W3FC08(FFID, FFJD, FU, FV, FGU, FGV)
C
      SAVE
C
      DATA  COS280/  0.1736482 /
      DATA  SIN280/ -0.9848078 /
C
C     COS280 AND SIN280 ARE FOR WIND AT POLE
C     (USED FOR CO-ORDINATE ROTATION TO GRID ORIENTATION)
C
      DFP = SQRT(FFID * FFID + FFJD * FFJD)
      IF (DFP .EQ. 0.) THEN
        XFGU = -(FU * COS280 + FV * SIN280)
        FGV  = -(FV * COS280 - FU * SIN280)
      ELSE
        XFGU = (FU * FFJD + FV * FFID) / DFP
        FGV  = (FV * FFJD - FU * FFID) / DFP
      ENDIF
      FGU = XFGU
      RETURN
      END
