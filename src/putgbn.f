C> @file
C> @brief Packs and writes a grib message.
C> @author Mark Iredell @date 1994-04-01

C> This subprogram is nearly the inverse of getgb.
C>
C> Program history log:
C> - Mark Iredell 1994-04-01
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C>
C> @param[in] lugb integer unit of the unblocked grib data file.
C> @param[in] kf integer number of data points.
C> @param[in] kpds integer (200) pds parameters.
C> - 1): id of center.
C> - 2): generating process id number.
C> - 3): grid definition.
C> - 4): gds/bms flag (right adj copy of octet 8).
C> - 5): indicator of parameter.
C> - 6): type of level.
C> - 7): height/pressure , etc of level.
C> - 8): year including (century-1).
C> - 9): month of year.
C> - 10: day of month.
C> - 11: hour of day.
C> - 12: minute of hour.
C> - 13: indicator of forecast time unit.
C> - 14: time range 1.
C> - 15: time range 2.
C> - 16: time range flag.
C> - 17: number included in average.
C> - 18: version nr of grib specification.
C> - 19: version nr of parameter table.
C> - 20: nr missing from average/accumulation.
C> - 21: century of reference time of data.
C> - 22: units decimal scale factor.
C> - 23: subcenter number.
C> - 24: pds byte 29, for nmc ensemble products.
C>  - 128 if forecast field error.
C>  - 64 if bias corrected fcst field.
C>  - 32 if smoothed field.
C>  - warning: can be combination of more than 1.
C> - 25: pds byte 30, not used.
C> @param[in] kgds integer (200) gds parameters.
C> - 1): data representation type.
C> - 19: number of vertical coordinate parameters.
C> - 20: octet number of the list of vertical coordinate parameters or
C> octet number of the list of numbers of points in each row or
C> 255 if neither are present.
C> - 21: for grids with pl, number of points in grid.
C> - 22: number of words in each row.
C> - Latitude/longitude grids.
C>  - 2): n(i) nr points on latitude circle.
C>  - 3): n(j) nr points on longitude meridian.
C>  - 4): la(1) latitude of origin.
C>  - 5): lo(1) longitude of origin.
C>  - 6): resolution flag (right adj copy of octet 17).
C>  - 7): la(2) latitude of extreme point.
C>  - 8): lo(2) longitude of extreme point.
C>  - 9): di longitudinal direction of increment.
C>  - 10: dj latitudinal direction increment.
C>  - 11: scanning mode flag (right adj copy of octet 28).
C> - Gaussian  grids.
C>  - 2): n(i) nr points on latitude circle.
C>  - 3): n(j) nr points on longitude meridian.
C>  - 4): la(1) latitude of origin.
C>  - 5): lo(1) longitude of origin.
C>  - 6): resolution flag  (right adj copy of octet 17).
C>  - 7): la(2) latitude of extreme point.
C>  - 8): lo(2) longitude of extreme point.
C>  - 9): di longitudinal direction of increment.
C>  - 10: n - nr of circles pole to equator.
C>  - 11: scanning mode flag (right adj copy of octet 28).
C>  - 12: nv - nr of vert coord parameters.
C>  - 13: pv - octet nr of list of vert coord parameters or
C>  pl - location of the list of numbers of points in
C>  each row (if no vert coord parameters are present) or
C>  255 if neither are present.
C> - Polar stereographic grids.
C>  - 2): n(i) nr points along lat circle.
C>  - 3): n(j) nr points along lon circle.
C>  - 4): la(1) latitude of origin.
C>  - 5): lo(1) longitude of origin.
C>  - 6): resolution flag  (right adj copy of octet 17).
C>  - 7): lov grid orientation.
C>  - 8): dx - x direction increment.
C>  - 9): dy - y direction increment.
C>  - 10: projection center flag.
C>  - 11: scanning mode (right adj copy of octet 28).
C> - Spherical harmonic coefficients.
C>  - 2): j pentagonal resolution parameter.
C>  - 3): k pentagonal resolution parameter.
C>  - 4): m pentagonal resolution parameter.
C>  - 5): representation type.
C>  - 6): coefficient storage mode.
C> - Mercator grids.
C>  - 2): n(i) nr points on latitude circle.
C>  - 3): n(j) nr points on longitude meridian.
C>  - 4): la(1) latitude of origin.
C>  - 5): lo(1) longitude of origin.
C>  - 6): resolution flag (right adj copy of octet 17).
C>  - 7): la(2) latitude of last grid point.
C>  - 8): lo(2) longitude of last grid point.
C>  - 9): latit - latitude of projection intersection.
C>  - 10: reserved.
C>  - 11: scanning mode flag (right adj copy of octet 28).
C>  - 12: longitudinal dir grid length.
C>  - 13: latitudinal dir grid length.
C> - Lambert conformal grids.
C>  - 2): nx nr points along x-axis.
C>  - 3): ny nr points along y-axis.
C>  - 4): la1 lat of origin (lower left).
C>  - 5): lo1 lon of origin (lower left).
C>  - 6): resolution (right adj copy of octet 17).
C>  - 7): lov - orientation of grid.
C>  - 8): dx - x-dir increment.
C>  - 9): dy - y-dir increment.
C>  - 10: projection center flag.
C>  - 11: scanning mode flag (right adj copy of octet 28).
C>  - 12: latin 1 - first lat from pole of secant cone inter.
C>  - 13: latin 2 - second lat from pole of secant cone inter.
C> @param[in] ibs integer binary scale factor (0 to ignore).
C> @param[in] nbits integer number of bits in which to pack (0 to ignore).
C> @param[in] lb logical*1 (kf) bitmap if present.
C> @param[in] f real (kf) data.
C> @param[out] iret integer return code.
C> - 0 all ok.
C> - other w3fi72 grib packer return code.
C>
C> @note Subprogram can be called from a multiprocessing environment.
C> Do not engage the same logical unit from more than one processor.
C>
C> @author Mark Iredell @date 1994-04-01
C-----------------------------------------------------------------------
      SUBROUTINE PUTGBN(LUGB,KF,KPDS,KGDS,IBS,NBITS,LB,F,IRET)
      INTEGER KPDS(200),KGDS(200)
      LOGICAL*1 LB(KF)
      REAL F(KF)
      PARAMETER(MAXBIT=16)
      INTEGER IBM(KF),IPDS(200),IGDS(200),IBDS(200)
      REAL FR(KF)
      CHARACTER PDS(400),GRIB(1000+KF*(MAXBIT+1)/8)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET W3FI72 PARAMETERS
      CALL R63W72(KPDS,KGDS,IPDS,IGDS)
      IBDS=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COUNT VALID DATA
      KBM=KF
      IF(IPDS(7).NE.0) THEN
        KBM=0
        DO I=1,KF
          IF(LB(I)) THEN
            IBM(I)=1
            KBM=KBM+1
          ELSE
            IBM(I)=0
          ENDIF
        ENDDO
        IF(KBM.EQ.KF) IPDS(7)=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET NUMBER OF BITS AND ROUND DATA
      IF(NBITS.GT.0) THEN
        DO I=1,KF
          FR(I)=F(I)
        ENDDO
        NBIT=NBITS
      ELSE
        IF(KBM.EQ.0) THEN
          DO I=1,KF
            FR(I)=0.
          ENDDO
          NBIT=0
        ELSE
          CALL GETBIT(IPDS(7),IBS,IPDS(25),KF,IBM,F,FR,FMIN,FMAX,NBIT)
          NBIT=MIN(NBIT,MAXBIT)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PACK AND WRITE GRIB DATA
      CALL W3FI72(0,FR,0,NBIT,0,IPDS,PDS,
     &            1,255,IGDS,0,0,IBM,KF,IBDS,
     &            KFO,GRIB,LGRIB,IRET)
      IF(IRET.EQ.0) CALL WRYTE(LUGB,LGRIB,GRIB)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
