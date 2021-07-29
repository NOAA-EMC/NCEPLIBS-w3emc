C> @file
C> @brief Finds and unpacks a grib message.
C> @author Mark Iredell @date 1994-04-01

C> Find and unpack a grib message.
C> Read a grib index file (or optionally the grib file itself)
C> to get the index buffer (i.e. table of contents) for the grib file.
C> (The index buffer is saved for use by future prospective calls.)
C> Find in the index buffer a reference to the grib message requested.
C> The grib message request specifies the number of messages to skip
C> and the unpacked pds and gds parameters. (A requested parameter
C> of -1 means to allow any value of this parameter to be found.)
C> If the requested grib message is found, then it is read from the
C> grib file and unpacked. Its message number is returned along with
C> the unpacked pds and gds parameters, the unpacked bitmap (if any),
C> and the unpacked data. If the grib message is not found, then the
C> return code will be nonzero.
C>
C> Program history log:
C> - Mark Iredell 1994-04-01
C> - Mark Iredell 1995-10-31 Modularized portions of code into subprograms
C> and allowed for unspecified index file.
C>
C> @param[in] lugb Integer unit of the unblocked grib data file.
C> @param[in] lugi Integer unit of the unblocked grib index file
C> (=0 to get index buffer from the grib file).
C> @param[in] jf Integer maximum number of data points to unpack.
C> @param[in] j Integer number of messages to skip
C> (=0 to search from beginning)
C> (<0 to read index buffer and skip -1-j messages).
C> @param[in] jpds Integer (200) pds parameters for which to search
C> (=-1 for wildcard).
C> - 1 Id of center.
C> - 2 Generating process id number.
C> - 3 Grid definition.
C> - 4 Gds/bms flag (right adj copy of octet 8).
C> - 5 Indicator of parameter.
C> - 6 Type of level.
C> - 7 Height/pressure , etc of level.
C> - 8 Year including (century-1).
C> - 9 Month of year.
C> - 10 Day of month.
C> - 11 Hour of day.
C> - 12 Minute of hour.
C> - 13 Indicator of forecast time unit.
C> - 14 Time range 1.
C> - 15 Time range 2.
C> - 16 Time range flag.
C> - 17 Number included in average.
C> - 18 Version nr of grib specification.
C> - 19 Version nr of parameter table.
C> - 20 Nr missing from average/accumulation.
C> - 21 Century of reference time of data.
C> - 22 Units decimal scale factor.
C> - 23 Subcenter number.
C> - 24 Pds byte 29, for nmc ensemble products.
C>  - 128 If forecast field error.
C>  - 64 If bias corrected fcst field.
C>  - 32 If smoothed field (warning: can be combination of more than 1).
C> - 25 Pds byte 30, not used
C> @param[in] jgds Integer (200) gds parameters for which to search
C> (only searched if jpds(3)=255)
C> (=-1 for wildcard).
C> - 1 Data representation type.
C> - 19 Number of vertical coordinate parameters.
C> - 20 Octet number of the list of vertical coordinate parameters
C> or octet number of the list of numbers of points in each row or
C> 255 If neither are present.
C> - 21 For grids with pl, number of points in grid.
C> - 22 Number of words in each row.
C> Latitude/longitude grids.
C> - 2 n(i) Nr points on latitude circle.
C> - 3 n(j) Nr points on longitude meridian.
C> - 4 la(1) Latitude of origin.
C> - 5 lo(1) Longitude of origin.
C> - 6 Resolution flag (right adj copy of octet 17).
C> - 7 la(2) Latitude of extreme point.
C> - 8 lo(2) Longitude of extreme point.
C> - 9 di Longitudinal direction of increment.
C> - 10 dj Latitudinal direction increment.
C> - 11 Scanning mode flag (right adj copy of octet 28).
C> Gaussian  grids.
C> - 2 n(i) Nr points on latitude circle.
C> - 3 n(j) Nr points on longitude meridian.
C> - 4 la(1) Latitude of origin.
C> - 5 lo(1) Longitude of origin.
C> - 6 Resolution flag  (right adj copy of octet 17).
C> - 7 la(2) Latitude of extreme point.
C> - 8 lo(2) Longitude of extreme point.
C> - 9 di Longitudinal direction of increment.
C> - 10 n Nr of circles pole to equator.
C> - 11 Scanning mode flag (right adj copy of octet 28).
C> - 12 nv Nr of vert coord parameters.
C> - 13 pv Octet nr of list of vert coord parameters or
C>  - pl Location of the list of numbers of points in
C> each row (if no vert coord parameters are present) or
C>  - 255 If neither are present.
C> Polar stereographic grids.
C> - 2 n(i) Nr points along lat circle.
C> - 3 n(j) Nr points along lon circle.
C> - 4 la(1) Latitude of origin.
C> - 5 lo(1) Longitude of origin.
C> - 6 Resolution flag  (right adj copy of octet 17).
C> - 7 lov Grid orientation.
C> - 8 dx - X direction increment.
C> - 9 dy - Y direction increment.
C> - 10 Projection center flag.
C> - 11 Scanning mode (right adj copy of octet 28).
C> Spherical harmonic coefficients.
C> - 2 j Pentagonal resolution parameter.
C> - 3 k Pentagonal resolution parameter.
C> - 4 m Pentagonal resolution parameter.
C> - 5 Representation type.
C> - 6 Coefficient storage mode.
C> Mercator grids.
C> - 2 n(i) Nr points on latitude circle.
C> - 3 n(j) Nr points on longitude meridian.
C> - 4 la(1) Latitude of origin.
C> - 5 lo(1) Longitude of origin.
C> - 6 Resolution flag (right adj copy of octet 17).
C> - 7 la(2) Latitude of last grid point.
C> - 8 lo(2) Longitude of last grid point.
C> - 9 latit - Latitude of projection intersection.
C> - 10 Reserved.
C> - 11 Scanning mode flag (right adj copy of octet 28).
C> - 12 Longitudinal dir grid length.
C> - 13 Latitudinal dir grid length.
C> Lambert conformal grids.
C> - 2 nx Nr points along x-axis.
C> - 3 ny Nr points along y-axis.
C> - 4 la1 Lat of origin (lower left).
C> - 5 lo1 Lon of origin (lower left).
C> - 6 Resolution (right adj copy of octet 17).
C> - 7 lov - Orientation of grid.
C> - 8 dx - X-dir increment.
C> - 9 dy - Y-dir increment.
C> - 10 Projection center flag.
C> - 11 Scanning mode flag (right adj copy of octet 28).
C> - 12 latin 1 First lat from pole of secant cone inter.
C> - 13 latin 2 Second lat from pole of secant cone inter.
C> @param[in] jens Integer (200) ensemble pds parms for which to search
C> (only searched if jpds(23)=2) (=-1 for wildcard).
C> - 1 Application identifier.
C> - 2 Ensemble type.
C> - 3 Ensemble identifier.
C> - 4 Product identifier.
C> - 5 Smoothing flag.
C>
C> @param[out] kf Integer number of data points unpacked.
C> @param[out] k Integer message number unpacked
C> (can be same as j in calling program
C> in order to facilitate multiple searches).
C> @param[out] kpds Integer (200) unpacked pds parameters.
C> @param[out] kgds Integer (200) unpacked gds parameters.
C> @param[out] kens Integer (200) unpacked ensemble pds parms.
C> @param[out] lb Logical*1 (kf) unpacked bitmap if present.
C> @param[out] f Real (kf) unpacked data.
C> @param[out] iret Integer return code.
C> - 0 All ok
C> - 96 Error reading index file
C> - 97 Error reading grib file
C> - 98 Number of data points greater than jf
C> - 99 Request not found
C> - other w3fi63 grib unpacker return code
C>
C> @note In order to unpack grib from a multiprocessing environment
C> where each processor is attempting to read from its own pair of
C> logical units, one must directly call subprogram getgbem as below,
C> allocating a private copy of cbuf, nlen and nnum to each processor.
C> Do not engage the same logical unit from more than one processor.
C>
C> @author Mark Iredell @date 1994-04-01
C-----------------------------------------------------------------------
      SUBROUTINE GETGBE(LUGB,LUGI,JF,J,JPDS,JGDS,JENS,
     &                  KF,K,KPDS,KGDS,KENS,LB,F,IRET)
      INTEGER JPDS(200),JGDS(200),JENS(200)
      INTEGER KPDS(200),KGDS(200),KENS(200)
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MBUF=256*1024)
      CHARACTER CBUF(MBUF)
      SAVE CBUF,NLEN,NNUM,MNUM
      DATA LUX/0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE WHETHER INDEX BUFFER NEEDS TO BE INITIALIZED
      IF(LUGI.GT.0.AND.(J.LT.0.OR.LUGI.NE.LUX)) THEN
        LUX=LUGI
        JJ=MIN(J,-1-J)
      ELSEIF(LUGI.LE.0.AND.(J.LT.0.OR.LUGB.NE.LUX)) THEN
        LUX=LUGB
        JJ=MIN(J,-1-J)
      ELSE
        JJ=J
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FIND AND UNPACK GRIB MESSAGE
      CALL GETGBEM(LUGB,LUGI,JF,JJ,JPDS,JGDS,JENS,
     &             MBUF,CBUF,NLEN,NNUM,MNUM,
     &             KF,K,KPDS,KGDS,KENS,LB,F,IRET)
      IF(IRET.EQ.96) LUX=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
