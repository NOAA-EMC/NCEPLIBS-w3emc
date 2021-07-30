C> @file
C> @brief Find and unpack a grib message.
C> @author Mark Iredell @date 1994-04-01

C> Find and unpack a grib message.
C> Read a grib index file (or optionally the grib file itself)
C> to get the index buffer (i.e. table of contents) for the grib file.
C> Find in the index buffer a reference to the grib message requested.
C> The grib message request specifies the number of messages to skip
C> and the unpacked pds and gds parameters. (A requested parameter
C> of -1 means to allow any value of this parameter to be found.)
C> If the requested grib message is found, then it is read from the
C> grib file and unpacked. Its message number is returned along with
C> the unpacked pds and gds parameters, the unpacked bitmap (if any),
C> and the unpacked data. if the grib message is not found, then the
C> return code will be nonzero.
C>
C> Program history log:
C> - Mark Iredell 1994-04-01
C> - Mark Iredell 1995-10-31 Modularized portions of code into subprograms
C> and allowed for unspecified index file.
C>
C> @param[in] LUGB Integer unit of the unblocked grib data file.
C> @param[in] LUGI Integer unit of the unblocked grib index file
C> (=0 to get index buffer from the grib file).
C> @param[in] JF Integer maximum number of data points to unpack.
C> @param[in] J integer number of messages to skip
C> (=0 to search from beginning)
C> (<0 to read index buffer and skip -1-j messages)
C> @param[in] jpds integer (200) pds parameters for which to search
C> (=-1 for wildcard)
C> - 1 id of center.
C> - 2 generating process id number.
C> - 3 grid definition.
C> - 4 gds/bms flag (right adj copy of octet 8).
C> - 5 indicator of parameter.
C> - 6 type of level.
C> - 7 height/pressure , etc of level.
C> - 8 year including (century-1).
C> - 9 month of year.
C> - 10 day of month.
C> - 11 hour of day.
C> - 12 minute of hour.
C> - 13 indicator of forecast time unit.
C> - 14 time range 1.
C> - 15 time range 2.
C> - 16 time range flag.
C> - 17 number included in average.
C> - 18 version nr of grib specification.
C> - 19 version nr of parameter table.
C> - 20 nr missing from average/accumulation.
C> - 21 century of reference time of data.
C> - 22 units decimal scale factor.
C> - 23 subcenter number.
C> - 24 pds byte 29, for nmc ensemble products.
C>  - 128 if forecast field error.
C>  - 64 if bias corrected fcst field.
C>  - 32 if smoothed field.
C>  - warning: can be combination of more than 1.
C> - (25)  - pds byte 30, not used.
C> @param[in] jgds integer (200) gds parameters for which to search
C> (only searched if jpds(3)=255)
C> (=-1 for wildcard).
C> - 1  data representation type.
C> - 19 number of vertical coordinate parameters.
C> - 20 octet number of the list of vertical coordinate parameters
C> or octet number of the list of numbers of points in each row or
C> 255 if neither are present.
C> - 21 for grids with pl, number of points in grid.
C> - 22 number of words in each row.
C> - Latitude/longitude grids.
C>  - 2 n(i) nr points on latitude circle.
C>  - 3 n(j) nr points on longitude meridian.
C>  - 4 la(1) latitude of origin.
C>  - 5 lo(1) longitude of origin.
C>  - 6 resolution flag (right adj copy of octet 17).
C>  - 7 la(2) latitude of extreme point.
C>  - 8 lo(2) longitude of extreme point.
C>  - 9 di longitudinal direction of increment.
C>  - 10 dj latitudinal direction increment.
C>  - 11 scanning mode flag (right adj copy of octet 28).
C> - Gaussian  grids.
C>  - 2 n(i) nr points on latitude circle.
C>  - 3 n(j) nr points on longitude meridian.
C>  - 4 la(1) latitude of origin.
C>  - 5 lo(1) longitude of origin.
C>  - 6 resolution flag  (right adj copy of octet 17).
C>  - 7 la(2) latitude of extreme point.
C>  - 8 lo(2) longitude of extreme point.
C>  - 9 di longitudinal direction of increment.
C>  - 10 n: nr of circles pole to equator.
C>  - 11 scanning mode flag (right adj copy of octet 28).
C>  - 12 nv: nr of vert coord parameters.
C>  - 13 pv: octet nr of list of vert coord parameters or
C>   - pl: location of the list of numbers of points in
C>   each row (if no vert coord parameters are present or
C>   - 255 if neither are present.
C> - Polar stereographic grids.
C>  - 2 n(i) nr points along lat circle.
C>  - 3 n(j) nr points along lon circle.
C>  - 4 la(1) latitude of origin.
C>  - 5 lo(1) longitude of origin.
C>  - 6 resolution flag  (right adj copy of octet 17).
C>  - 7 lov grid orientation.
C>  - 8 dx - x direction increment.
C>  - 9 dy - y direction increment.
C>  - 10 projection center flag.
C>  - 11 scanning mode (right adj copy of octet 28).
C> - Spherical harmonic coefficients.
C>  - 2 j pentagonal resolution parameter.
C>  - 3 k pentagonal resolution parameter.
C>  - 4 m pentagonal resolution parameter.
C>  - 5 representation type.
C>  - 6 coefficient storage mode.
C> - Mercator grids.
C>  - 2 n(i) nr points on latitude circle.
C>  - 3 n(j) nr points on longitude meridian.
C>  - 4 la(1) latitude of origin.
C>  - 5 lo(1) longitude of origin.
C>  - 6 resolution flag (right adj copy of octet 17).
C>  - 7 la(2) latitude of last grid point.
C>  - 8 lo(2) longitude of last grid point.
C>  - 9 latit - latitude of projection intersection.
C>  - 10 reserved.
C>  - 11 scanning mode flag (right adj copy of octet 28).
C>  - 12 longitudinal dir grid length.
C>  - 13 latitudinal dir grid length.
C> - Lambert conformal grids.
C>  - 2 nx nr points along x-axis.
C>  - 3 ny nr points along y-axis.
C>  - 4 la1 lat of origin (lower left).
C>  - 5 lo1 lon of origin (lower left).
C>  - 6 resolution (right adj copy of octet 17).
C>  - 7 lov - orientation of grid.
C>  - 8 dx - x-dir increment.
C>  - 9 dy - y-dir increment.
C>  - 10 projection center flag.
C>  - 11 scanning mode flag (right adj copy of octet 28).
C>  - 12 latin 1 - first lat from pole of secant cone inter.
C>  - 13 latin 2 - second lat from pole of secant cone inter.
C> @param[in] jens integer (200) ensemble pds parms for which to search
C> (only searched if jpds(23)=2)
C> (=-1 for wildcard).
C>  - 1 application identifier.
C>  - 2 ensemble type.
C>  - 3 ensemble identifier.
C>  - 4 product identifier.
C>  - 5 smoothing flag.
C> @param[in] mbuf integer length of index buffer in bytes.
C> @param[inout] nnum integer number of index records
C> (initialize by setting j=-1).
C> @param[inout] mnum integer number of index records skipped
C> (initialize by setting j=-1).
C> @param[inout] cbuf character*1 (mbuf) index buffer
C> (initialize by setting j=-1).
C> @param[inout] nlen integer length of each index record in bytes.
C> (initialize by setting j=-1).
C> @param[out] kf integer number of data points unpacked.
C> @param[out] k integer message number unpacked
C> (can be same as j in calling program
C> in order to facilitate multiple searches).
C> @param[out] kpds integer (200) unpacked pds parameters.
C> @param[out] kgds integer (200) unpacked gds parameters.
C> @param[out] kens integer (200) unpacked ensemble pds parms.
C> @param[out] lb logical*1 (kf) unpacked bitmap if present.
C> @param[out] f real (kf) unpacked data.
C> @param[out] iret integer return code.
C>  - 0 all ok.
C>  - 96 error reading index file.
C>  - 97 error reading grib file.
C>  - 98 number of data points greater than jf.
C>  - 99 request not found.
C>  - other w3fi63 grib unpacker return code.
C>
C> @note Specify an index file if feasible to increase speed.
C> Subprogram can be called from a multiprocessing environment.
C> Do not engage the same logical unit from more than one processor.
C>
C-----------------------------------------------------------------------
      SUBROUTINE GETGBEM(LUGB,LUGI,JF,J,JPDS,JGDS,JENS,
     &                   MBUF,CBUF,NLEN,NNUM,MNUM,
     &                   KF,K,KPDS,KGDS,KENS,LB,F,IRET)
      INTEGER JPDS(200),JGDS(200),JENS(200)
      INTEGER KPDS(200),KGDS(200),KENS(200)
      CHARACTER CBUF(MBUF)
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH PREVIOUS INDEX BUFFER IF POSSIBLE
      IF(J.GE.0) THEN
        IF(MNUM.GE.0) THEN
          IRGI=0
        ELSE
          MNUM=-1-MNUM
          IRGI=1
        ENDIF
        JR=J-MNUM
        IF(JR.GE.0.AND.(JR.LT.NNUM.OR.IRGI.EQ.0)) THEN
          CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &                 KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
          IF(IRGS.EQ.0) K=KR+MNUM
          IF(IRGI.EQ.1.AND.IRGS.EQ.0) MNUM=-1-MNUM
          IF(IRGI.EQ.1.AND.IRGS.GT.0) MNUM=MNUM+NNUM
        ELSE
          MNUM=J
          IRGI=1
          IRGS=1
        ENDIF
      ELSE
        MNUM=-1-J
        IRGI=1
        IRGS=1
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND SEARCH NEXT INDEX BUFFER
      JR=0
      DOWHILE(IRGI.EQ.1.AND.IRGS.EQ.1)
        IF(LUGI.GT.0) THEN
          CALL GETGI(LUGI,MNUM,MBUF,CBUF,NLEN,NNUM,IRGI)
        ELSE
          CALL GETGIR(LUGB,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRGI)
        ENDIF
        IF(IRGI.LE.1) THEN
          CALL GETGB1S(CBUF,NLEN,NNUM,JR,JPDS,JGDS,JENS,
     &                 KR,KPDS,KGDS,KENS,LSKIP,LGRIB,IRGS)
          IF(IRGS.EQ.0) K=KR+MNUM
          IF(IRGI.EQ.1.AND.IRGS.EQ.0) MNUM=-1-MNUM
          IF(IRGI.EQ.1.AND.IRGS.GT.0) MNUM=MNUM+NNUM
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND UNPACK GRIB RECORD
      IF(IRGI.GT.1) THEN
        IRET=96
      ELSEIF(IRGS.NE.0) THEN
        IRET=99
      ELSEIF(LENGDS(KGDS).GT.JF) THEN
        IRET=98
      ELSE
        CALL GETGB1R(LUGB,LSKIP,LGRIB,KF,KPDS,KGDS,KENS,LB,F,NBITS,
     &               IRET)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
