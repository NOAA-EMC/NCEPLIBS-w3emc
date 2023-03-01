C> @file
C> @brief Unpack GRIB field to a GRIB grid.
C> @author Bill Cavanaugh @date 1991-09-13

C> Unpack a GRIB (edition 1) field to the exact grid
C> specified in the GRIB message, isolate the bit map, and make
C> the values of the product descripton section (PDS) and the
C> grid description section (GDS) available in return arrays.
C>
C> When decoding is completed, data at each grid point has been
C> returned in the units specified in the GRIB manual.
C>
C> See "GRIB - THE WMO FORMAT FOR THE STORAGE OF WEATHER PRODUCT
C> INFORMATION AND THE EXCHANGE OF WEATHER PRODUCT MESSAGES IN
C> GRIDDED BINARY FORM" dated July 1, 1988 by John D. Stackpolem
C> DOC, NOAA, NWS, National Meteorological Center.
C>
C>  List of text messages from code:
C>  - W3FI63/FI632
C>    - 'HAVE ENCOUNTERED A NEW GRID FOR NMC, PLEASE NOTIFY
C>       AUTOMATION DIVISION, PRODUCTION MANAGEMENT BRANCH
C>       (W/NMC42)'
C>
C>    - 'HAVE ENCOUNTERED A NEW GRID FOR ECMWF, PLEASE NOTIFY
C>       AUTOMATION DIVISION, PRODUCTION MANAGEMENT BRANCH
C>       (W/NMC42)'
C>
C>    - 'HAVE ENCOUNTERED A NEW GRID FOR U.K. METEOROLOGICAL
C>      OFFICE, BRACKNELL.  PLEASE NOTIFY AUTOMATION DIVISION,
C>      PRODUCTION MANAGEMENT BRANCH (W/NMC42)'
C>
C>    - 'HAVE ENCOUNTERED A NEW GRID FOR FNOC, PLEASE NOTIFY
C>      AUTOMATION DIVISION, PRODUCTION MANAGEMENT BRANCH
C>      (W/NMC42)'
C>
C>  - W3FI63/FI633
C>    - 'POLAR STEREO PROCESSING NOT AVAILABLE'
C>
C>  - W3FI63/FI634
C>    - 'WARNING - BIT MAP MAY NOT BE ASSOCIATED WITH SPHERICAL
C>      COEFFICIENTS'
C>
C>  - W3FI63/FI637
C>    - 'NO CURRENT LISTING OF FNOC GRIDS'
C>
C> @param[in] MSGA Grib field - "grib" thru "7777"   char*1
C> (message can be preceded by junk chars). Contains the grib message to be unpacked. characters
C> "GRIB" may begin anywhere within first 100 bytes.
C> @param[out] KPDS Array of size 100 containing PDS elements, GRIB (edition 1):
C> - 1 Id of center
C> - 2 Generating process id number
C> - 3 Grid definition
C> - 4 Gds/bms flag (right adj copy of octet 8)
C> - 5 Indicator of parameter
C> - 6 Type of level
C> - 7 Height/pressure , etc of level
C> - 8 Year including (century-1)
C> - 9 Month of year
C> - 10 Day of month
C> - 11 Hour of day
C> - 12 Minute of hour
C> - 13 Indicator of forecast time unit
C> - 14 Time range 1
C> - 15 Time range 2
C> - 16 Time range flag
C> - 17 Number included in average
C> - 18 Version nr of grib specification
C> - 19 Version nr of parameter table
C> - 20 Nr missing from average/accumulation
C> - 21 Century of reference time of data
C> - 22 Units decimal scale factor
C> - 23 Subcenter number
C> - 24 Pds byte 29, for nmc ensemble products
C>  - 128 If forecast field error
C>  - 64 If bias corrected fcst field
C>  - 32 If smoothed field
C>  - Warning: can be combination of more than 1
C> - 25 Pds byte 30, not used
C> - 26-35 Reserved
C> - 36-N Consecutive bytes extracted from program
C> Definition section (pds) of grib message
C> @param[out] KGDS ARRAY CONTAINING GDS ELEMENTS.
C>  - 1) Data representation type
C>  - 19 Number of vertical coordinate parameters
C>  - 20 Octet number of the list of vertical coordinate
C>    Parameters Or Octet number of the list of numbers of points
C>    In each row Or 255 if neither are present
C>  - 21 For grids with pl, number of points in grid
C>  - 22 Number of words in each row
C> - LATITUDE/LONGITUDE GRIDS
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag (right adj copy of octet 17)
C>  - 7 La(2) latitude of extreme point
C>  - 8 Lo(2) longitude of extreme point
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C> - GAUSSIAN  GRIDS
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag  (right adj copy of octet 17)
C>  - 7 La(2) latitude of extreme point
C>  - 8 Lo(2) longitude of extreme point
C>  - 9 Di longitudinal direction of increment
C>  - 10 N - nr of circles pole to equator
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C>  - 12 Nv - nr of vert coord parameters
C>  - 13 Pv - octet nr of list of vert coord parameters or
C>    Pl - location of the list of numbers of points in
C>    each row (if no vert coord parameters are present or
C>    255 if neither are present
C> - POLAR STEREOGRAPHIC GRIDS
C>  - 2 N(i) nr points along lat circle
C>  - 3 N(j) nr points along lon circle
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag  (right adj copy of octet 17)
C>  - 7 Lov grid orientation
C>  - 8 Dx - x direction increment
C>  - 9 Dy - y direction increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode (right adj copy of octet 28)
C> - SPHERICAL HARMONIC COEFFICIENTS
C>  - 2) J pentagonal resolution parameter
C>  - 3) K pentagonal resolution parameter
C>  - 4) M pentagonal resolution parameter
C>  - 5) Representation type
C>  - 6) Coefficient storage mode
C> - MERCATOR GRIDS
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag (right adj copy of octet 17)
C>  - 7 La(2) latitude of last grid point
C>  - 8 Lo(2) longitude of last grid point
C>  - 9 Latit - latitude of projection intersection
C>  - 10 Reserved
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C>  - 12 Longitudinal dir grid length
C>  - 13 Latitudinal dir grid length
C> - LAMBERT CONFORMAL GRIDS
C>  - 2 Nx nr points along x-axis
C>  - 3 Ny nr points along y-axis
C>  - 4 La1 lat of origin (lower left)
C>  - 5 Lo1 lon of origin (lower left)
C>  - 6 Resolution (right adj copy of octet 17)
C>  - 7 Lov - orientation of grid
C>  - 8 Dx - x-dir increment
C>  - 9 Dy - y-dir increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C>  - 12 Latin 1 - first lat from pole of secant cone inter
C>  - 13 Latin 2 - second lat from pole of secant cone inter
C> - E-STAGGERED ARAKAWA ROTATED LAT/LON GRIDS (TYPE 203)
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag (right adj copy of octet 17)
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C> - CURVILINEAR ORTHIGINAL GRID (TYPE 204)
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 Reserved set to 0
C>  - 5 Reserved set to 0
C>  - 6 Resolution flag (right adj copy of octet 17)
C>  - 7 Reserved set to 0
C>  - 8 Reserved set to 0
C>  - 9 Reserved set to 0
C>  - 10 Reserved set to 0
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C> - ROTATED LAT/LON A,B,C,D-STAGGERED (TYPE 205)
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of first point
C>  - 5 Lo(1) longitude of first point
C>  - 6 Resolution flag (right adj copy of octet 17)
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag (right adj copy of octet 28)
C>  - 12 Latitude of last point
C>  - 13 Longitude of last point
C> @param[out] KBMS Bitmap describing location of output elements.
C> (always constructed)
C> @param[out] DATA Array containing the unpacked data elements.
C> Note: 65160 is maximun field size allowable.
C> @param[out] KPTR Array containing storage for following parameters
C> - 1 Total length of grib message
C> - 2 Length of indicator (section  0)
C> - 3 Length of pds (section  1)
C> - 4 Length of gds (section  2)
C> - 5 Length of bms (section  3)
C> - 6 Length of bds (section  4)
C> - 7 Value of current byte
C> - 8 Bit pointer
C> - 9 Grib start bit nr
C> - 10 Grib/grid element count
C> - 11 Nr unused bits at end of section 3
C> - 12 Bit map flag (copy of bms octets 5,6)
C> - 13 Nr unused bits at end of section 2
C> - 14 Bds flags (right adj copy of octet 4)
C> - 15 Nr unused bits at end of section 4
C> - 16 Reserved
C> - 17 Reserved
C> - 18 Reserved
C> - 19 Binary scale factor
C> - 20 Num bits used to pack each datum
C> @param[out] KRET Flag indicating quality of completion.
C>
C> @note When decoding is completed, data at each grid point has been
C> returned in the units specified in the grib manual.
C>
C> - Values for return flag (kret)
C>  - 0 - Normal return, no errors
C>  - 1 - 'grib' not found in first 100 chars
C>  - 2 - '7777' not in correct location
C>  - 3 - Unpacked field is larger than 260000
C>  - 4 - Gds/ grid not one of currently accepted values
C>  - 5 - Grid not currently avail for center indicated
C>  - 8 - Temp gds indicated, but gds flag is off
C>  - 9 - Gds indicates size mismatch with std grid
C>  - 10 - Incorrect center indicator
C>  - 11 - Binary data section (bds) not completely processed.
C>  program is not set to process flag combinations
C>  shown in octets 4 and 14.
C>  - 12 - Binary data section (bds) not completely processed.
C>  program is not set to process flag combinations
C>
C> @author Bill Cavanaugh @date 1991-09-13
      SUBROUTINE W3FI63(MSGA,KPDS,KGDS,KBMS,DATA,KPTR,KRET)
C
C  * WILL BE AVAILABLE IN NEXT UPDATE
C  ***************************************************************
C
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C                       BIT MAP
      LOGICAL*1     KBMS(*)
C
C                       ELEMENTS OF PRODUCT DESCRIPTION SEC   (PDS)
      INTEGER       KPDS(*)
C                       ELEMENTS OF GRID DESCRIPTION SEC   (PDS)
      INTEGER       KGDS(*)
C
C                       CONTAINER FOR GRIB GRID
      REAL          DATA(*)
C
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(*)
C
C  *****************************************************************
      INTEGER       JSGN,JEXP,IFR,NPTS
      REAL          REALKK,FVAL1,FDIFF1
C  *****************************************************************
C        1.0 LOCATE BEGINNING OF 'GRIB' MESSAGE
C             FIND 'GRIB' CHARACTERS
C        2.0  USE COUNTS IN EACH DESCRIPTION SEC   TO DETERMINE
C             IF '7777' IS IN PROPER PLACE.
C        3.0  PARSE PRODUCT DEFINITION SECTION.
C        4.0  PARSE GRID DESCRIPTION SEC   (IF INCLUDED)
C        5.0  PARSE BIT MAP SEC   (IF INCLUDED)
C        6.0  USING INFORMATION FROM PRODUCT DEFINITION, GRID
C                  DESCRIPTION, AND BIT MAP SECTIONS.. EXTRACT
C                  DATA AND PLACE INTO PROPER ARRAY.
C  *******************************************************************
C
C                      MAIN DRIVER
C
C  *******************************************************************
      KPTR(10) = 0
C                  SEE IF PROPER 'GRIB' KEY EXISTS, THEN
C                  USING SEC   COUNTS, DETERMINE IF '7777'
C                  IS IN THE PROPER LOCATION
C
      CALL FI631(MSGA,KPTR,KPDS,KRET)
      IF(KRET.NE.0) THEN
          GO TO 900
      END IF
C     PRINT *,'FI631 KPTR',(KPTR(I),I=1,16)
C
C                  PARSE PARAMETERS FROM PRODUCT DESCRIPTION SECTION
C
      CALL FI632(MSGA,KPTR,KPDS,KRET)
      IF(KRET.NE.0) THEN
          GO TO 900
      END IF
C     PRINT *,'FI632 KPTR',(KPTR(I),I=1,16)
C
C                  IF AVAILABLE, EXTRACT NEW GRID DESCRIPTION
C
      IF (IAND(KPDS(4),128).NE.0) THEN
          CALL FI633(MSGA,KPTR,KGDS,KRET)
          IF(KRET.NE.0) THEN
              GO TO 900
          END IF
C         PRINT *,'FI633 KPTR',(KPTR(I),I=1,16)
      END IF
C
C                  EXTRACT OR GENERATE BIT MAP
C
      CALL FI634(MSGA,KPTR,KPDS,KGDS,KBMS,KRET)
      IF (KRET.NE.0) THEN
        IF (KRET.NE.9) THEN
          GO TO 900
        END IF
      END IF
C     PRINT *,'FI634 KPTR',(KPTR(I),I=1,16)
C
C                  USING INFORMATION FROM PDS, BMS AND BIT DATA SEC  ,
C                  EXTRACT AND SAVE IN GRIB GRID, ALL DATA ENTRIES.
C
      IF (KPDS(18).EQ.1) THEN
          CALL FI635(MSGA,KPTR,KPDS,KGDS,KBMS,DATA,KRET)
          IF (KPTR(3).EQ.50) THEN
C
C                     PDS EQUAL 50 BYTES
C                        THEREFORE SOMETHING SPECIAL IS GOING ON
C
C                        IN THIS CASE 2ND DIFFERENCE PACKING
C                                NEEDS TO BE UNDONE.
C
C                   EXTRACT FIRST VALUE FROM BYTE 41-44 PDS
C                              KPTR(9) CONTAINS OFFSET TO START OF
C                              GRIB MESSAGE.
C                   EXTRACT FIRST FIRST-DIFFERENCE FROM BYTES 45-48 PDS
C
C                  AND EXTRACT SCALE FACTOR (E) TO UNDO 2**E
C                  THAT WAS APPLIED PRIOR TO 2ND ORDER PACKING
C                  AND PLACED IN PDS BYTES 49-51
C                  FACTOR IS A SIGNED TWO BYTE INTEGER
C
C                  ALSO NEED THE DECIMAL SCALING FROM PDS(27-28)
C                  (AVAILABLE IN KPDS(22) FROM UNPACKER)
C                  TO UNDO THE DECIMAL SCALING APPLIED TO THE
C                  SECOND DIFFERENCES DURING UNPACKING.
C                  SECOND DIFFS ALWAYS PACKED WITH 0 DECIMAL SCALE
C                  BUT UNPACKER DOESNT KNOW THAT.
C
C             CALL GBYTE  (MSGA,FVAL1,KPTR(9)+384,32)
C
C         NOTE INTEGERS, CHARACTERS AND EQUIVALENCES
C         DEFINED ABOVE TO MAKE THIS KKK EXTRACTION
C         WORK AND LINE UP ON WORD BOUNDARIES
C
C       THE NEXT CODE WILL CONVERT THE IBM370 FOATING POINT
C       TO THE FLOATING POINT USED ON YOUR MACHINE.
C
        call gbytec(MSGA,JSGN,KPTR(9)+384,1)
        call gbytec(MSGA,JEXP,KPTR(9)+385,7)
        call gbytec(MSGA,IFR,KPTR(9)+392,24)
C
              IF (IFR.EQ.0) THEN
                  REALKK = 0.0
              ELSE IF (JEXP.EQ.0.AND.IFR.EQ.0) THEN
                  REALKK = 0.0
              ELSE
                  REALKK = FLOAT(IFR) * 16.0 ** (JEXP - 64 - 6)
                  IF (JSGN.NE.0) REALKK = -REALKK
              END IF
              FVAL1 = REALKK
C
C             CALL GBYTE  (MSGA,FDIFF1,KPTR(9)+416,32)
C          (REPLACED BY FOLLOWING EXTRACTION)
C
C       THE NEXT CODE WILL CONVERT THE IBM370 FOATING POINT
C       TO THE FLOATING POINT USED ON YOUR MACHINE.
C
             call gbytec(MSGA,JSGN,KPTR(9)+416,1)
             call gbytec(MSGA,JEXP,KPTR(9)+417,7)
             call gbytec(MSGA,IFR,KPTR(9)+424,24)
C
              IF (IFR.EQ.0) THEN
                  REALKK = 0.0
              ELSE IF (JEXP.EQ.0.AND.IFR.EQ.0) THEN
                  REALKK = 0.0
              ELSE
                  REALKK = FLOAT(IFR) * 16.0 ** (JEXP - 64 - 6)
                  IF (JSGN.NE.0) REALKK = -REALKK
              END IF
              FDIFF1 = REALKK
C
              CALL GBYTEC (MSGA,ISIGN,KPTR(9)+448,1)
              CALL GBYTEC (MSGA,ISCAL2,KPTR(9)+449,15)
              IF(ISIGN.GT.0) THEN
                  ISCAL2 = - ISCAL2
              ENDIF
C             PRINT *,'DELTA POINT 1-',FVAL1
C             PRINT *,'DELTA POINT 2-',FDIFF1
C             PRINT *,'DELTA POINT 3-',ISCAL2
              NPTS  = KPTR(10)
C             WRITE (6,FMT='(''  2ND DIFF POINTS IN FIELD = '',/,
C    &         10(3X,10F12.2,/))') (DATA(I),I=1,NPTS)
C             PRINT *,'DELTA POINT 4-',KPDS(22)
              CALL W3FI83 (DATA,NPTS,FVAL1,FDIFF1,
     &                            ISCAL2,KPDS(22),KPDS,KGDS)
C             WRITE (6,FMT='(''  2ND DIFF EXPANDED POINTS IN FIELD = '',
C    &            /,10(3X,10F12.2,/))') (DATA(I),I=1,NPTS)
C             WRITE (6,FMT='(''  END OF ARRAY IN FIELD = '',/,
C    &         10(3X,10F12.2,/))') (DATA(I),I=NPTS-5,NPTS)
          END IF
      ELSE
C         PRINT *,'FI635 NOT PROGRAMMED FOR EDITION NR',KPDS(18)
          KRET   = 7
      END IF
C
  900 RETURN
      END

C> @brief Find 'grib' chars & reset pointers
C> @author Bill Cavanaugh @date 1991-09-13

C> Find 'grib; characters and set pointers to the next
C> byte following 'grib'. If they exist extract counts from gds and
C> bms. Extract count from bds. Determine if sum of counts actually
C> places terminator '7777' at the correct location.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-09-13
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C>
C> @param[in] MSGA Grib field - "grib" thru "7777"
C> @param[inout] KPTR Array containing storage for following parameters
C> - 1 Total length of grib message
C> - 2 Length of indicator (section  0)
C> - 3 Length of pds (section  1)
C> - 4 Length of gds (section  2)
C> - 5 Length of bms (section  3)
C> - 6 Length of bds (section  4)
C> - 7 Value of current byte
C> - 8 Bit pointer
C> - 9 Grib start bit nr
C> - 10 Grib/grid element count
C> - 11 Nr unused bits at end of section 3
C> - 12 Bit map flag
C> - 13 Nr unused bits at end of section 2
C> - 14 Bds flags
C> - 15 Nr unused bits at end of section 4
C> @param[out] KPDS Array containing pds elements.
C> - 1 Id of center
C> - 2 Model identification
C> - 3 Grid identification
C> - 4 Gds/bms flag
C> - 5 Indicator of parameter
C> - 6 Type of level
C> - 7 Height/pressure , etc of level
C> - 8 Year of century
C> - 9 Month of year
C> - 10 Day of month
C> - 11 Hour of day
C> - 12 Minute of hour
C> - 13 Indicator of forecast time unit
C> - 14 Time range 1
C> - 15 Time range 2
C> - 16 Time range flag
C> - 17 Number included in average
C> @param[out] KRET Error return
C>
C> @note
C> ERROR RETURNS
C> KRET:
C> - 1 NO 'GRIB'
C> - 2 NO '7777' OR MISLOCATED (BY COUNTS)
C>
C> @author Bill Cavanaugh @date 1991-09-13
      SUBROUTINE FI631(MSGA,KPTR,KPDS,KRET)
C
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(*)
C                       PRODUCT DESCRIPTION SECTION DATA.
      INTEGER       KPDS(*)
C
      INTEGER       KRET
C
C  ******************************************************************
      KRET = 0
C  -------------------  FIND 'GRIB' KEY
      DO 50 I = 0, 839, 8
          CALL GBYTEC (MSGA,MGRIB,I,32)
          IF (MGRIB.EQ.1196575042) THEN
              KPTR(9)   = I
              GO TO 60
          END IF
   50 CONTINUE
      KRET  = 1
      RETURN
   60 CONTINUE
C  -------------FOUND 'GRIB'
C                        SKIP GRIB CHARACTERS
C     PRINT *,'FI631 GRIB AT',I
      KPTR(8)   = KPTR(9) + 32
      CALL GBYTEC (MSGA,ITOTAL,KPTR(8),24)
C                    HAVE LIFTED WHAT MAY BE A MSG TOTAL BYTE COUNT
      IPOINT    = KPTR(9) + ITOTAL * 8 - 32
      CALL GBYTEC (MSGA,I7777,IPOINT,32)
      IF (I7777.EQ.926365495) THEN
C                 HAVE FOUND END OF MESSAGE '7777' IN PROPER LOCATION
C                 MARK AND PROCESS AS GRIB VERSION 1 OR HIGHER
C         PRINT *,'FI631 7777 AT',IPOINT
          KPTR(8)   = KPTR(8) + 24
          KPTR(1)   = ITOTAL
          KPTR(2)   = 8
          CALL GBYTEC (MSGA,KPDS(18),KPTR(8),8)
          KPTR(8)   = KPTR(8) + 8
      ELSE
C                 CANNOT FIND END OF GRIB EDITION 1 MESSAGE
          KRET      = 2
          RETURN
      END IF
C  -------------------  PROCESS SECTION 1
C                   EXTRACT COUNT FROM PDS
C     PRINT *,'START OF PDS',KPTR(8)
      CALL GBYTEC (MSGA,KPTR(3),KPTR(8),24)
      LOOK      = KPTR(8) + 56
C                   EXTRACT GDS/BMS FLAG
      CALL GBYTEC (MSGA,KPDS(4),LOOK,8)
      KPTR(8)   = KPTR(8) + KPTR(3) * 8
C     PRINT *,'START OF GDS',KPTR(8)
      IF (IAND(KPDS(4),128).NE.0) THEN
C                   EXTRACT COUNT FROM GDS
          CALL GBYTEC (MSGA,KPTR(4),KPTR(8),24)
          KPTR(8)   = KPTR(8) + KPTR(4) * 8
      ELSE
          KPTR(4)   = 0
      END IF
C     PRINT *,'START OF BMS',KPTR(8)
      IF (IAND(KPDS(4),64).NE.0) THEN
C                   EXTRACT COUNT FROM BMS
          CALL GBYTEC (MSGA,KPTR(5),KPTR(8),24)
      ELSE
          KPTR(5)   = 0
      END IF
      KPTR(8)   = KPTR(8) + KPTR(5) * 8
C     PRINT *,'START OF BDS',KPTR(8)
C                   EXTRACT COUNT FROM BDS
      CALL GBYTEC (MSGA,KPTR(6),KPTR(8),24)
C  ---------------  TEST FOR '7777'
C     PRINT *,(KPTR(KJ),KJ=1,10)
      KPTR(8)   = KPTR(8) + KPTR(6) * 8
C                   EXTRACT FOUR BYTES FROM THIS LOCATION
C     PRINT *,'FI631 LOOKING FOR 7777 AT',KPTR(8)
      CALL GBYTEC (MSGA,K7777,KPTR(8),32)
      MATCH  = KPTR(2) + KPTR(3) + KPTR(4) + KPTR(5) + KPTR(6) + 4
      IF (K7777.NE.926365495.OR.MATCH.NE.KPTR(1)) THEN
          KRET  = 2
      ELSE
C         PRINT *,'FI631 7777 AT',KPTR(8)
          IF (KPDS(18).EQ.0) THEN
              KPTR(1)  = KPTR(2) + KPTR(3) + KPTR(4) + KPTR(5) +
     *                KPTR(6) + 4
          END IF
      END IF
C     PRINT *,'KPTR',(KPTR(I),I=1,16)
      RETURN
      END


C> @brief Gather info from product definition sec.
C> @author Bill Cavanaugh @date 1991-09-13

C> Extract information from the product description
C> sec  , and generate label information to permit storage
C> in office note 84 format.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-09-13
C> - Bill Cavanaugh 1993-12-08 Corrected test for edition number instead
C> of version number.
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C> - M. Baldwin 1999-01-20 Modified to handle grid 237.
C>
C> @param[in] MSGA Array containing grib message.
C> @param[inout] KPTR Array containing storage for following parameters.
C> - 1 Total length of grib message
C> - 2 Length of indicator (section  0)
C> - 3 Length of pds (section  1)
C> - 4 Length of gds (section  2)
C> - 5 Length of bms (section  3)
C> - 6 Length of bds (section  4)
C> - 7 Value of current byte
C> - 8 Bit pointer
C> - 9 Grib start bit nr
C> - 10 Grib/grid element count
C> - 11 Nr unused bits at end of section 3
C> - 12 Bit map flag
C> - 13 Nr unused bits at end of section 2
C> - 14 Bds flags
C> - 15 Nr unused bits at end of section 4
C> @param[out] KPDS Array containing pds elements.
C> - 1 Id of center
C> - 2 Model identification
C> - 3 Grid identification
C> - 4 Gds/bms flag
C> - 5 Indicator of parameter
C> - 6 Type of level
C> - 7 Height/pressure , etc of level
C> - 8 Year of century
C> - 9 Month of year
C> - 10 Day of month
C> - 11 Hour of day
C> - 12 Minute of hour
C> - 13 Indicator of forecast time unit
C> - 14 Time range 1
C> - 15 Time range 2
C> - 16 Time range flag
C> - 17 Number included in average
C> - 18
C> - 19
C> - 20 Number missing from avgs/accumulations
C> - 21 Century
C> - 22 Units decimal scale factor
C> - 23 Subcenter
C> @param[out] KRET Error return.
C>
C> @note ERROR RETURN:
C> - 0 - NO ERRORS
C> - 8 - TEMP GDS INDICATED, BUT NO GDS
C>
C> @author Bill Cavanaugh @date 1991-09-13

      SUBROUTINE FI632(MSGA,KPTR,KPDS,KRET)

C
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(*)
C                       PRODUCT DESCRIPTION SECTION ENTRIES
      INTEGER       KPDS(*)
C
      INTEGER       KRET
      KRET=0
C  -------------------  PROCESS SECTION 1
      KPTR(8)  = KPTR(9) + KPTR(2) * 8 + 24
C  BYTE 4
C                   PARAMETER TABLE VERSION NR
          CALL GBYTEC (MSGA,KPDS(19),KPTR(8),8)
          KPTR(8)   = KPTR(8) + 8
C  BYTE 5           IDENTIFICATION OF CENTER
      CALL GBYTEC (MSGA,KPDS(1),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 6
C                       GET GENERATING PROCESS ID NR
      CALL GBYTEC (MSGA,KPDS(2),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 7
C                      GRID DEFINITION
      CALL GBYTEC (MSGA,KPDS(3),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 8
C                      GDS/BMS FLAGS
C     CALL GBYTEC (MSGA,KPDS(4),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 9
C                      INDICATOR OF PARAMETER
      CALL GBYTEC (MSGA,KPDS(5),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 10
C                      TYPE OF LEVEL
      CALL GBYTEC (MSGA,KPDS(6),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 11,12
C                      HEIGHT/PRESSURE
      CALL GBYTEC (MSGA,KPDS(7),KPTR(8),16)
      KPTR(8)   = KPTR(8) + 16
C  BYTE 13
C                      YEAR OF CENTURY
      CALL GBYTEC (MSGA,KPDS(8),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 14
C                      MONTH OF YEAR
      CALL GBYTEC (MSGA,KPDS(9),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 15
C                      DAY OF MONTH
      CALL GBYTEC (MSGA,KPDS(10),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 16
C                      HOUR OF DAY
      CALL GBYTEC (MSGA,KPDS(11),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 17
C                      MINUTE
      CALL GBYTEC (MSGA,KPDS(12),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 18
C                      INDICATOR TIME UNIT RANGE
      CALL GBYTEC (MSGA,KPDS(13),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 19
C                      P1 - PERIOD OF TIME
      CALL GBYTEC (MSGA,KPDS(14),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 20
C                      P2 - PERIOD OF TIME
      CALL GBYTEC (MSGA,KPDS(15),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 21
C                      TIME RANGE INDICATOR
      CALL GBYTEC (MSGA,KPDS(16),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C
C     IF TIME RANGE INDICATOR IS 10, P1 IS PACKED IN
C     PDS BYTES 19-20
C
      IF (KPDS(16).EQ.10) THEN
          KPDS(14)  = KPDS(14) * 256 + KPDS(15)
          KPDS(15)  = 0
      END IF
C  BYTE 22,23
C                      NUMBER INCLUDED IN AVERAGE
      CALL GBYTEC (MSGA,KPDS(17),KPTR(8),16)
      KPTR(8)   = KPTR(8) + 16
C  BYTE 24
C                      NUMBER MISSING FROM AVERAGES/ACCUMULATIONS
      CALL GBYTEC (MSGA,KPDS(20),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C  BYTE 25
C                      IDENTIFICATION OF CENTURY
      CALL GBYTEC (MSGA,KPDS(21),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
      IF (KPTR(3).GT.25) THEN
C  BYTE 26              SUB CENTER NUMBER
          CALL GBYTEC (MSGA,KPDS(23),KPTR(8),8)
          KPTR(8)   = KPTR(8) + 8
          IF (KPTR(3).GE.28) THEN
C  BYTE 27-28
C                          UNITS DECIMAL SCALE FACTOR
              CALL GBYTEC (MSGA,ISIGN,KPTR(8),1)
              KPTR(8)  = KPTR(8) + 1
              CALL GBYTEC (MSGA,IDEC,KPTR(8),15)
              KPTR(8)  = KPTR(8) + 15
              IF (ISIGN.GT.0) THEN
                  KPDS(22)  = - IDEC
              ELSE
                  KPDS(22)  = IDEC
              END IF
              ISIZ  = KPTR(3) - 28
              IF (ISIZ.LE.12) THEN
C  BYTE  29
                  CALL GBYTEC (MSGA,KPDS(24),KPTR(8)+8,8)
C  BYTE  30
                  CALL GBYTEC (MSGA,KPDS(25),KPTR(8)+16,8)
C  BYTES 31-40                  CURRENTLY RESERVED FOR FUTURE USE
                  KPTR(8)  = KPTR(8) + ISIZ * 8
              ELSE
C  BYTE  29
                  CALL GBYTEC (MSGA,KPDS(24),KPTR(8)+8,8)
C  BYTE  30
                  CALL GBYTEC (MSGA,KPDS(25),KPTR(8)+16,8)
C  BYTES 31-40                  CURRENTLY RESERVED FOR FUTURE USE
                  KPTR(8)  = KPTR(8) + 12 * 8
C  BYTES 41 - N                 LOCAL USE DATA
                  CALL W3FI01(LW)
C                  MWDBIT  = LW * 8
                  MWDBIT  = bit_size(KPDS)
                  ISIZ    = KPTR(3) - 40
                  ITER    = ISIZ / LW
                  IF (MOD(ISIZ,LW).NE.0) ITER = ITER + 1
                  CALL GBYTESC (MSGA,KPDS(36),KPTR(8),MWDBIT,0,ITER)
                  KPTR(8)  = KPTR(8) + ISIZ * 8
              END IF
          END IF
      END IF
C  ----------- TEST FOR NEW GRID
      IF (IAND(KPDS(4),128).NE.0) THEN
          IF (IAND(KPDS(4),64).NE.0) THEN
              IF (KPDS(3).NE.255) THEN
                  IF (KPDS(3).GE.21.AND.KPDS(3).LE.26)THEN
                      RETURN
                  ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.44)THEN
                      RETURN
                  ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
                      RETURN
                  END IF
                  IF (KPDS(1).EQ.7) THEN
                      IF (KPDS(3).GE.2.AND.KPDS(3).LE.3) THEN
                      ELSE IF (KPDS(3).GE.5.AND.KPDS(3).LE.6) THEN
                      ELSE IF (KPDS(3).EQ.8) THEN
                      ELSE IF (KPDS(3).EQ.10) THEN
                      ELSE IF (KPDS(3).GE.27.AND.KPDS(3).LE.34) THEN
                      ELSE IF (KPDS(3).EQ.50) THEN
                      ELSE IF (KPDS(3).EQ.53) THEN
                      ELSE IF (KPDS(3).GE.70.AND.KPDS(3).LE.77) THEN
                      ELSE IF (KPDS(3).EQ.98) THEN
                      ELSE IF (KPDS(3).EQ.99) THEN
                      ELSE IF (KPDS(3).GE.100.AND.KPDS(3).LE.105) THEN
                      ELSE IF (KPDS(3).EQ.126) THEN
                      ELSE IF (KPDS(3).EQ.195) THEN
                      ELSE IF (KPDS(3).EQ.196) THEN
                      ELSE IF (KPDS(3).EQ.197) THEN
                      ELSE IF (KPDS(3).EQ.198) THEN
                      ELSE IF (KPDS(3).GE.200.AND.KPDS(3).LE.237) THEN
                      ELSE
C                         PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
C    *                    ' NMC WITHOUT A GRID DESCRIPTION SECTION'
C                         PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
C                         PRINT *,' PRODUCTION MANAGEMENT BRANCH'
C                         PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.98) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.16) THEN
                      ELSE
C                         PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
C    *                    ' ECMWF WITHOUT A GRID DESCRIPTION SECTION'
C                         PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
C                         PRINT *,' PRODUCTION MANAGEMENT BRANCH'
C                         PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.74) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
                      ELSE IF (KPDS(3).GE.21.AND.KPDS(3).LE.26)THEN
                      ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
                      ELSE IF (KPDS(3).GE.70.AND.KPDS(3).LE.77) THEN
                      ELSE
C                         PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
C    *                            ' U.K. MET OFFICE, BRACKNELL',
C    *                            ' WITHOUT A GRID DESCRIPTION SECTION'
C                         PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
C                         PRINT *,' PRODUCTION MANAGEMENT BRANCH'
C                         PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.58) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
                      ELSE
C                         PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
C    *                      ' FNOC WITHOUT A GRID DESCRIPTION SECTION'
C                         PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
C                         PRINT *,' PRODUCTION MANAGEMENT BRANCH'
C                         PRINT *,' W/NMC42)'
                      END IF
                  END IF
              END IF
          END IF
      END IF
      RETURN
      END

C> @brief Extract info from grib-gds
C> @author Bill Cavanaugh @date 1991-09-13

C> Extract information on unlisted grid to allow
C> conversion to office note 84 format.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-09-13
C> - M. Baldwin 1995-03-20 fi633 modification to get
C> data rep types [kgds(1)] 201 and 202 to work.
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - M. Baldwin 1998-09-08 Add data rep type [kgds(1)] 203
C> - Boi Vuong 2007-04-24 Add data rep type [kgds(1)] 204
C> - George Gayno 2010-07-20 Add data rep type [kgds(1)] 205
C>
C> @param[in] MSGA Array containing grib message
C> @param[inout] KPTR Array containing storage for following parameters
C> - 1 Total length of grib message
C> - 2 Length of indicator (section  0)
C> - 3 Length of pds (section  1)
C> - 4 Length of gds (section  2)
C> - 5 Length of bms (section  3)
C> - 6 Length of bds (section  4)
C> - 7 Value of current byte
C> - 8 Bit pointer
C> - 9 Grib start bit nr
C> - 10 Grib/grid element count
C> - 11 Nr unused bits at end of section 3
C> - 12 Bit map flag
C> - 13 Nr unused bits at end of section 2
C> - 14 Bds flags
C> - 15 Nr unused bits at end of section 4
C> @param[out] KGDS Array containing gds elements.
C>  - 1) Data representation type
C>  - 19 Number of vertical coordinate parameters
C>  - 20 Octet number of the list of vertical coordinate
C>  parameters Or Octet number of the list of numbers of points
C>  in each row Or 255 if neither are present.
C>  - 21 For grids with pl, number of points in grid
C>  - 22 Number of words in each row
C> - Longitude grids
C>  - 2) N(i) nr points on latitude circle
C>  - 3) N(j) nr points on longitude meridian
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Resolution flag
C>  - 7) La(2) latitude of extreme point
C>  - 8) Lo(2) longitude of extreme point
C>  - 9) Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Polar stereographic grids
C>  - 2) N(i) nr points along lat circle
C>  - 3) N(j) nr points along lon circle
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Reserved
C>  - 7) Lov grid orientation
C>  - 8) Dx - x direction increment
C>  - 9) Dy - y direction increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode
C> - Spherical harmonic coefficients
C>  - 2 J pentagonal resolution parameter
C>  - 3 K pentagonal resolution parameter
C>  - 4 M pentagonal resolution parameter
C>  - 5 Representation type
C>  - 6 Coefficient storage mode
C> - Mercator grids
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of last grid point
C>  - 8 Lo(2) longitude of last grid point
C>  - 9 Latin - latitude of projection intersection
C>  - 10 Reserved
C>  - 11 Scanning mode flag
C>  - 12 Longitudinal dir grid length
C>  - 13 Latitudinal dir grid length
C> - Lambert conformal grids
C>  - 2 Nx nr points along x-axis
C>  - 3 Ny nr points along y-axis
C>  - 4 La1 lat of origin (lower left)
C>  - 5 Lo1 lon of origin (lower left)
C>  - 6 Resolution (right adj copy of octet 17)
C>  - 7 Lov - orientation of grid
C>  - 8 Dx - x-dir increment
C>  - 9 Dy - y-dir increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode flag
C>  - 12 Latin 1 - first lat from pole of secant cone inter
C>  - 13 Latin 2 - second lat from pole of secant cone inter
C> - Staggered arakawa rotated lat/lon grids (203 e stagger)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Staggered arakawa rotated lat/lon grids (205 a,b,c,d staggers)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C>  - 12 Latitude of last point
C>  - 13 Longitude of last point
C> @param[out] KRET Error return
C>
C> @note
C> - KRET
C>  - 0
C>  - 4   - Data representation type not currently acceptable
C>
C> @author Bill Cavanaugh @date 1991-09-13

      SUBROUTINE FI633(MSGA,KPTR,KGDS,KRET)

C  ************************************************************
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C
C                       ARRAY GDS ELEMENTS
      INTEGER       KGDS(*)
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(*)
C
      INTEGER       KRET
C  ---------------------------------------------------------------
      KRET    = 0
C                PROCESS GRID DEFINITION SECTION (IF PRESENT)
C             MAKE SURE BIT POINTER IS PROPERLY SET
      KPTR(8)  = KPTR(9) + (KPTR(2)*8) + (KPTR(3)*8) + 24
      NSAVE    = KPTR(8) - 24
C  BYTE 4
C                   NV - NR OF VERT COORD PARAMETERS
      CALL GBYTEC (MSGA,KGDS(19),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  BYTE 5
C                   PV - LOCATION - SEE FM92 MANUAL
      CALL GBYTEC (MSGA,KGDS(20),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  BYTE 6
C                      DATA REPRESENTATION TYPE
      CALL GBYTEC (MSGA,KGDS(1),KPTR(8),8)
      KPTR(8)   = KPTR(8) + 8
C           BYTES 7-32 ARE GRID DEFINITION DEPENDING ON
C           DATA REPRESENTATION TYPE
      IF (KGDS(1).EQ.0) THEN
          GO TO 1000
      ELSE IF (KGDS(1).EQ.1) THEN
          GO TO 4000
      ELSE IF (KGDS(1).EQ.2.OR.KGDS(1).EQ.5) THEN
          GO TO 2000
      ELSE IF (KGDS(1).EQ.3) THEN
          GO TO 5000
      ELSE IF (KGDS(1).EQ.4) THEN
          GO TO 1000
C     ELSE IF (KGDS(1).EQ.10) THEN
C     ELSE IF (KGDS(1).EQ.14) THEN
C     ELSE IF (KGDS(1).EQ.20) THEN
C     ELSE IF (KGDS(1).EQ.24) THEN
C     ELSE IF (KGDS(1).EQ.30) THEN
C     ELSE IF (KGDS(1).EQ.34) THEN
      ELSE IF (KGDS(1).EQ.50) THEN
          GO TO 3000
C     ELSE IF (KGDS(1).EQ.60) THEN
C     ELSE IF (KGDS(1).EQ.70) THEN
C     ELSE IF (KGDS(1).EQ.80) THEN
      ELSE IF (KGDS(1).EQ.201.OR.KGDS(1).EQ.202.OR.
     &      KGDS(1).EQ.203.OR.KGDS(1).EQ.204.OR.KGDS(1).EQ.205) THEN
          GO TO 1000
      ELSE
C                      MARK AS GDS/ UNKNOWN DATA REPRESENTATION TYPE
          KRET     = 4
          RETURN
      END IF
C     BYTE 33-N   VERTICAL COORDINATE PARAMETERS
C  -----------
C     BYTES 33-42 EXTENSIONS OF GRID DEFINITION FOR ROTATION
C                 OR STRETCHING OF THE COORDINATE SYSTEM OR
C                 LAMBERT CONFORMAL PROJECTION.
C     BYTE 43-N   VERTICAL COORDINATE PARAMETERS
C  -----------
C     BYTES 33-52 EXTENSIONS OF GRID DEFINITION FOR STRETCHED
C                 AND ROTATED COORDINATE SYSTEM
C     BYTE 53-N   VERTICAL COORDINATE PARAMETERS
C  -----------
C ************************************************************
C  ------------------- LATITUDE/LONGITUDE GRIDS
C  ------------------- ARAKAWA STAGGERED, SEMI-STAGGERED, OR FILLED
C       ROTATED LAT/LON GRIDS OR CURVILINEAR ORTHIGINAL GRIDS
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG LATITUDE CIRCLE
 1000 CONTINUE
      CALL GBYTEC (MSGA,KGDS(2),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 9-10    NR OF POINTS ALONG LONG MERIDIAN
      CALL GBYTEC (MSGA,KGDS(3),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 11-13   LATITUDE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(4),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  IAND(KGDS(4),8388607) * (-1)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(5),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  =  - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESOLUTION FLAG
      CALL GBYTEC (MSGA,KGDS(6),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 18-20   LATITUDE OF LAST GRID POINT
      CALL GBYTEC (MSGA,KGDS(7),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  =  - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   LONGITUDE OF LAST GRID POINT
      CALL GBYTEC (MSGA,KGDS(8),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(8),8388608).NE.0) THEN
          KGDS(8)  =  - IAND(KGDS(8),8388607)
      END IF
C  ------------------- BYTE 24-25   LATITUDINAL DIR INCREMENT
      CALL GBYTEC (MSGA,KGDS(9),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 26-27   IF REGULAR LAT/LON GRID
C                                       HAVE LONGIT DIR INCREMENT
C                                   ELSE IF GAUSSIAN GRID
C                                       HAVE NR OF LAT CIRCLES
C                                       BETWEEN POLE AND EQUATOR
      CALL GBYTEC (MSGA,KGDS(10),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 28      SCANNING MODE FLAGS
      CALL GBYTEC (MSGA,KGDS(11),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
      IF(KGDS(1).EQ.205)THEN
C  ------------------- BYTE 29-31   LATITUDE OF LAST GRID POINT
        CALL GBYTEC (MSGA,KGDS(12),KPTR(8),24)
        KPTR(8)  = KPTR(8) + 24
        IF (IAND(KGDS(12),8388608).NE.0) THEN
            KGDS(12)  =  - IAND(KGDS(12),8388607)
        END IF
C  ------------------- BYTE 32-34   LONGITUDE OF LAST GRID POINT
        CALL GBYTEC (MSGA,KGDS(13),KPTR(8),24)
        KPTR(8)  = KPTR(8) + 24
        IF (IAND(KGDS(13),8388608).NE.0) THEN
            KGDS(13)  =  - IAND(KGDS(13),8388607)
        END IF
      ELSE

C  ------------------- BYTE 29-32   RESERVED
C                             SKIP TO START OF BYTE 33
      CALL GBYTEC (MSGA,KGDS(12),KPTR(8),32)
      KPTR(8)  = KPTR(8) + 32
      ENDIF
C  -------------------
      GO TO 900
C  ******************************************************************
C            ' POLAR STEREO PROCESSING '
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG X=AXIS
 2000 CONTINUE
      CALL GBYTEC (MSGA,KGDS(2),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 9-10    NR OF POINTS ALONG Y-AXIS
      CALL GBYTEC (MSGA,KGDS(3),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 11-13   LATITUDE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(4),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  - IAND(KGDS(4),8388607)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(5),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  =   - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESERVED
      CALL GBYTEC (MSGA,KGDS(6),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 18-20   LOV ORIENTATION OF THE GRID
      CALL GBYTEC (MSGA,KGDS(7),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  =  - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   DX - THE X DIRECTION INCREMENT
      CALL GBYTEC (MSGA,KGDS(8),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(8),8388608).NE.0) THEN
          KGDS(8)  =  - IAND(KGDS(8),8388607)
      END IF
C  ------------------- BYTE 24-26   DY - THE Y DIRECTION INCREMENT
      CALL GBYTEC (MSGA,KGDS(9),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(9),8388608).NE.0) THEN
          KGDS(9)  =  - IAND(KGDS(9),8388607)
      END IF
C  ------------------- BYTE 27      PROJECTION CENTER FLAG
      CALL GBYTEC (MSGA,KGDS(10),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 28      SCANNING MODE
      CALL GBYTEC (MSGA,KGDS(11),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 29-32   RESERVED
C                             SKIP TO START OF BYTE 33
      CALL GBYTEC (MSGA,KGDS(12),KPTR(8),32)
      KPTR(8)  = KPTR(8) + 32
C
C  -------------------
      GO TO 900
C
C  ******************************************************************
C  ------------------- GRID DESCRIPTION FOR SPHERICAL HARMONIC COEFF.
C
C  ------------------- BYTE 7-8     J PENTAGONAL RESOLUTION PARAMETER
 3000 CONTINUE
      CALL GBYTEC (MSGA,KGDS(2),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 9-10    K PENTAGONAL RESOLUTION PARAMETER
      CALL GBYTEC (MSGA,KGDS(3),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 11-12   M PENTAGONAL RESOLUTION PARAMETER
      CALL GBYTEC (MSGA,KGDS(4),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 13 REPRESENTATION TYPE
      CALL GBYTEC (MSGA,KGDS(5),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 14 COEFFICIENT STORAGE MODE
      CALL GBYTEC (MSGA,KGDS(6),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  -------------------        EMPTY FIELDS - BYTES 15 - 32
C                 SET TO START OF BYTE 33
      KPTR(8)  = KPTR(8) + 18 * 8
      GO TO 900
C  ******************************************************************
C                      PROCESS MERCATOR GRIDS
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG LATITUDE CIRCLE
 4000 CONTINUE
      CALL GBYTEC (MSGA,KGDS(2),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 9-10    NR OF POINTS ALONG LONG MERIDIAN
      CALL GBYTEC (MSGA,KGDS(3),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 11-13   LATITUE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(4),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  - IAND(KGDS(4),8388607)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(5),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  =  - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESOLUTION FLAG
      CALL GBYTEC (MSGA,KGDS(6),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 18-20   LATITUDE OF EXTREME POINT
      CALL GBYTEC (MSGA,KGDS(7),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  =  - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   LONGITUDE OF EXTREME POINT
      CALL GBYTEC (MSGA,KGDS(8),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(8),8388608).NE.0) THEN
          KGDS(8)  =  - IAND(KGDS(8),8388607)
      END IF
C  ------------------- BYTE 24-26   LATITUDE OF PROJECTION INTERSECTION
      CALL GBYTEC (MSGA,KGDS(9),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(9),8388608).NE.0) THEN
          KGDS(9)  =  - IAND(KGDS(9),8388607)
      END IF
C  ------------------- BYTE 27   RESERVED
      CALL GBYTEC (MSGA,KGDS(10),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 28      SCANNING MODE
      CALL GBYTEC (MSGA,KGDS(11),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 29-31   LONGITUDINAL DIR INCREMENT
      CALL GBYTEC (MSGA,KGDS(12),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(12),8388608).NE.0) THEN
          KGDS(12)  =  - IAND(KGDS(12),8388607)
      END IF
C  ------------------- BYTE 32-34   LATITUDINAL DIR INCREMENT
      CALL GBYTEC (MSGA,KGDS(13),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(13),8388608).NE.0) THEN
          KGDS(13)  =  - IAND(KGDS(13),8388607)
      END IF
C  ------------------- BYTE 35-42   RESERVED
C                        SKIP TO START OF BYTE 43
      KPTR(8)  = KPTR(8) + 8 * 8
C  -------------------
      GO TO 900
C  ******************************************************************
C                      PROCESS LAMBERT CONFORMAL
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG X-AXIS
 5000 CONTINUE
      CALL GBYTEC (MSGA,KGDS(2),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 9-10    NR OF POINTS ALONG Y-AXIS
      CALL GBYTEC (MSGA,KGDS(3),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  ------------------- BYTE 11-13   LATITUDE OF ORIGIN
      CALL GBYTEC (MSGA,KGDS(4),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  - IAND(KGDS(4),8388607)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN (LOWER LEFT)
      CALL GBYTEC (MSGA,KGDS(5),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  = - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESOLUTION
      CALL GBYTEC (MSGA,KGDS(6),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 18-20   LOV -ORIENTATION OF GRID
      CALL GBYTEC (MSGA,KGDS(7),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  = - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   DX - X-DIR INCREMENT
      CALL GBYTEC (MSGA,KGDS(8),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
C  ------------------- BYTE 24-26   DY - Y-DIR INCREMENT
      CALL GBYTEC (MSGA,KGDS(9),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
C  ------------------- BYTE 27       PROJECTION CENTER FLAG
      CALL GBYTEC (MSGA,KGDS(10),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 28      SCANNING MODE
      CALL GBYTEC (MSGA,KGDS(11),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C  ------------------- BYTE 29-31   LATIN1 - 1ST LAT FROM POLE
      CALL GBYTEC (MSGA,KGDS(12),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(12),8388608).NE.0) THEN
          KGDS(12)  =  - IAND(KGDS(12),8388607)
      END IF
C  ------------------- BYTE 32-34   LATIN2 - 2ND LAT FROM POLE
      CALL GBYTEC (MSGA,KGDS(13),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(13),8388608).NE.0) THEN
          KGDS(13)  =  - IAND(KGDS(13),8388607)
      END IF
C  ------------------- BYTE 35-37   LATITUDE OF SOUTHERN POLE
      CALL GBYTEC (MSGA,KGDS(14),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(14),8388608).NE.0) THEN
          KGDS(14)  =  - IAND(KGDS(14),8388607)
      END IF
C  ------------------- BYTE 38-40   LONGITUDE OF SOUTHERN POLE
      CALL GBYTEC (MSGA,KGDS(15),KPTR(8),24)
      KPTR(8)  = KPTR(8) + 24
      IF (IAND(KGDS(15),8388608).NE.0) THEN
          KGDS(15)  =  - IAND(KGDS(15),8388607)
      END IF
C  ------------------- BYTE 41-42   RESERVED
      CALL GBYTEC (MSGA,KGDS(16),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C  -------------------
  900 CONTINUE
C
C                        MORE CODE FOR GRIDS WITH PL
C
      IF (KGDS(19).EQ.0.OR.KGDS(19).EQ.255) THEN
        IF (KGDS(20).NE.255) THEN
          ISUM  = 0
          KPTR(8)  = NSAVE + (KGDS(20) - 1) * 8
          CALL GBYTESC (MSGA,KGDS(22),KPTR(8),16,0,KGDS(3))
          DO 910 J = 1, KGDS(3)
              ISUM  = ISUM + KGDS(21+J)
  910     CONTINUE
          KGDS(21)  = ISUM
        END IF
      END IF
      RETURN
      END


C> @brief Extract or generate bit map for output
C> @author Bill Cavanaugh @date 1991-09-13

C> If bit map sec is available in grib message, extract
C> for program use, otherwise generate an appropriate bit map.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-09-13
C> - Bill Cavanaugh 1991-11-12 Modified size of ecmwf grids 5 - 8.
C> - Mark Iredell 1995-10-31 removed saves and prints
C> - W. Bostelman 1997-02-12 corrects ecmwf us grid 2 processing
C> - Mark Iredell 1997-09-19 vectorized bitmap decoder
C> - Stephen Gilbert 1998-09-02 corrected error in map size for u.s. grid 92
C> - M. Baldwin 1998-09-08 add grids 190,192
C> - M. Baldwin 1999-01-20 add grids 236,237
C> - Eric Rogers 2001-10-02 redefined grid #218 for 12 km eta
C> redefined grid 192 for new 32-km eta grid
C> - Stephen Gilbert 2003-06-30 added grids 145 and 146 for cmaq
C> and grid 175 for awips over guam.
C> - Boi Vuong 2004-09-02 Added awips grids 147, 148, 173 and 254
C> - Boi Vuong 2006-12-12 Added awips grids 120
C> - Boi Vuong 2007-04-20 Added awips grids 176
C> - Boi Vuong 2007-06-11 Added awips grids 11 to 18 and 122 to 125
C> and 180 to 183
C> - Boi Vuong 2010-08-05 Added new grid 184, 199, 83 and
C> redefined grid 90 for new rtma conus 1.27-km
C> redefined grid 91 for new rtma alaska 2.976-km
C> redefined grid 92 for new rtma alaska 1.488-km
C> - Boi Vuong 2012-02-28 Added new grid 200
C>
C> @param[in] MSGA Bufr message
C> @param[inout] KPTR Array containing storage for following parameters
C> - 1 Total length of grib message
C> - 2 Length of indicator (section  0)
C> - 3 Length of pds (section  1)
C> - 4 Length of gds (section  2)
C> - 5 Length of bms (section  3)
C> - 6 Length of bds (section  4)
C> - 7 Value of current byte
C> - 8 Bit pointer
C> - 9 Grib start bit nr
C> - 10 Grib/grid element count
C> - 11 Nr unused bits at end of section 3
C> - 12 Bit map flag
C> - 13 Nr unused bits at end of section 2
C> - 14 Bds flags
C> - 15 Nr unused bits at end of section 4
C> @param[in] KPDS Array containing pds elements.
C> - 1 Id of center
C> - 2 Model identification
C> - 3 Grid identification
C> - 4 Gds/bms flag
C> - 5 Indicator of parameter
C> - 6 Type of level
C> - 7 Height/pressure , etc of level
C> - 8 Year of century
C> - 9 Month of year
C> - 10 Day of month
C> - 11 Hour of day
C> - 12 Minute of hour
C> - 13 Indicator of forecast time unit
C> - 14 Time range 1
C> - 15 Time range 2
C> - 16 Time range flag
C> - 17 Number included in average
C> @param[in] KGDS Array containing gds elements.
C>  - 1) Data representation type
C>  - 19 Number of vertical coordinate parameters
C>  - 20 Octet number of the list of vertical coordinate
C>  parameters Or Octet number of the list of numbers of points
C>  in each row Or 255 if neither are present.
C>  - 21 For grids with pl, number of points in grid
C>  - 22 Number of words in each row
C> - Longitude grids
C>  - 2) N(i) nr points on latitude circle
C>  - 3) N(j) nr points on longitude meridian
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Resolution flag
C>  - 7) La(2) latitude of extreme point
C>  - 8) Lo(2) longitude of extreme point
C>  - 9) Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Polar stereographic grids
C>  - 2) N(i) nr points along lat circle
C>  - 3) N(j) nr points along lon circle
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Reserved
C>  - 7) Lov grid orientation
C>  - 8) Dx - x direction increment
C>  - 9) Dy - y direction increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode
C> - Spherical harmonic coefficients
C>  - 2 J pentagonal resolution parameter
C>  - 3 K pentagonal resolution parameter
C>  - 4 M pentagonal resolution parameter
C>  - 5 Representation type
C>  - 6 Coefficient storage mode
C> - Mercator grids
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of last grid point
C>  - 8 Lo(2) longitude of last grid point
C>  - 9 Latin - latitude of projection intersection
C>  - 10 Reserved
C>  - 11 Scanning mode flag
C>  - 12 Longitudinal dir grid length
C>  - 13 Latitudinal dir grid length
C> - Lambert conformal grids
C>  - 2 Nx nr points along x-axis
C>  - 3 Ny nr points along y-axis
C>  - 4 La1 lat of origin (lower left)
C>  - 5 Lo1 lon of origin (lower left)
C>  - 6 Resolution (right adj copy of octet 17)
C>  - 7 Lov - orientation of grid
C>  - 8 Dx - x-dir increment
C>  - 9 Dy - y-dir increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode flag
C>  - 12 Latin 1 - first lat from pole of secant cone inter
C>  - 13 Latin 2 - second lat from pole of secant cone inter
C> - Staggered arakawa rotated lat/lon grids (203 e stagger)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Staggered arakawa rotated lat/lon grids (205 a,b,c,d staggers)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C>  - 12 Latitude of last point
C>  - 13 Longitude of last point
C> @param[out] KBMS Bitmap describing location of output elements.
C> @param[out] KRET Error return
C>
C> @note
C> - KRET
C>  -  0 - No error
C>  -  5 - Grid not avail for center indicated
C>  - 10 - Incorrect center indicator
C>  - 12 - Bytes 5-6 are not zero in bms, predefined bit map
C>  not provided by this center
C>
C> @author Bill Cavanaugh @date 1991-09-13

      SUBROUTINE FI634(MSGA,KPTR,KPDS,KGDS,KBMS,KRET)

C
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C
C                       BIT MAP
      LOGICAL*1     KBMS(*)
C
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(*)
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPDS(*)
      INTEGER       KGDS(*)
C
      INTEGER       KRET
      INTEGER       MASK(8)
C  ----------------------GRID 21 AND GRID 22 ARE THE SAME
      LOGICAL*1     GRD21( 1369)
C  ----------------------GRID 23 AND GRID 24 ARE THE SAME
      LOGICAL*1     GRD23( 1369)
      LOGICAL*1     GRD25( 1368)
      LOGICAL*1     GRD26( 1368)
C  ----------------------GRID 27 AND GRID 28 ARE THE SAME
C  ----------------------GRID 29 AND GRID 30 ARE THE SAME
C  ----------------------GRID 33 AND GRID 34 ARE THE SAME
      LOGICAL*1     GRD50( 1188)
C  -----------------------GRID 61 AND GRID 62 ARE THE SAME
      LOGICAL*1     GRD61( 4186)
C  -----------------------GRID 63 AND GRID 64 ARE THE SAME
      LOGICAL*1     GRD63( 4186)
C     LOGICAL*1     GRD70(16380)/16380*.TRUE./
C  -------------------------------------------------------------
      DATA  GRD21 /1333*.TRUE.,36*.FALSE./
      DATA  GRD23 /.TRUE.,36*.FALSE.,1332*.TRUE./
      DATA  GRD25 /1297*.TRUE.,71*.FALSE./
      DATA  GRD26 /.TRUE.,71*.FALSE.,1296*.TRUE./
      DATA  GRD50/
C LINE 1-4
     &  7*.FALSE.,22*.TRUE.,14*.FALSE.,22*.TRUE.,
     & 14*.FALSE.,22*.TRUE.,14*.FALSE.,22*.TRUE.,7*.FALSE.,
C LINE 5-8
     &  6*.FALSE.,24*.TRUE.,12*.FALSE.,24*.TRUE.,
     & 12*.FALSE.,24*.TRUE.,12*.FALSE.,24*.TRUE.,6*.FALSE.,
C LINE 9-12
     &  5*.FALSE.,26*.TRUE.,10*.FALSE.,26*.TRUE.,
     & 10*.FALSE.,26*.TRUE.,10*.FALSE.,26*.TRUE.,5*.FALSE.,
C LINE 13-16
     &  4*.FALSE.,28*.TRUE., 8*.FALSE.,28*.TRUE.,
     &  8*.FALSE.,28*.TRUE., 8*.FALSE.,28*.TRUE.,4*.FALSE.,
C LINE 17-20
     &  3*.FALSE.,30*.TRUE., 6*.FALSE.,30*.TRUE.,
     &  6*.FALSE.,30*.TRUE., 6*.FALSE.,30*.TRUE.,3*.FALSE.,
C LINE 21-24
     &  2*.FALSE.,32*.TRUE., 4*.FALSE.,32*.TRUE.,
     &  4*.FALSE.,32*.TRUE., 4*.FALSE.,32*.TRUE.,2*.FALSE.,
C LINE 25-28
     &    .FALSE.,34*.TRUE., 2*.FALSE.,34*.TRUE.,
     &  2*.FALSE.,34*.TRUE., 2*.FALSE.,34*.TRUE.,  .FALSE.,
C LINE 29-33
     &           180*.TRUE./
      DATA  GRD61 /4096*.TRUE.,90*.FALSE./
      DATA  GRD63 /.TRUE.,90*.FALSE.,4095*.TRUE./
      DATA  MASK  /128,64,32,16,8,4,2,1/
C
C     PRINT *,'FI634'
      IF (IAND(KPDS(4),64).EQ.64) THEN
C
C                   SET UP BIT POINTER
C                          SECTION 0    SECTION 1     SECTION 2
      KPTR(8) = KPTR(9) + (KPTR(2)*8) + (KPTR(3)*8) + (KPTR(4)*8) + 24
C
C  BYTE 4           NUMBER OF UNUSED BITS AT END OF SECTION 3
C
      CALL GBYTEC (MSGA,KPTR(11),KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
C
C  BYTE 5,6         TABLE REFERENCE IF 0, BIT MAP FOLLOWS
C
      CALL GBYTEC (MSGA,KPTR(12),KPTR(8),16)
      KPTR(8)  = KPTR(8) + 16
C                   IF TABLE REFERENCE = 0, EXTRACT BIT MAP
        IF (KPTR(12).EQ.0) THEN
C                   CALCULATE NR OF BITS IN BIT MAP
          IBITS   = (KPTR(5) - 6) * 8 - KPTR(11)
          KPTR(10)  = IBITS
          IF (KPDS(3).EQ.21.OR.KPDS(3).EQ.22.OR.KPDS(3).EQ.25.
     *             OR.KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
C                    NORTHERN HEMISPHERE  21, 22, 25, 61, 62
              CALL FI634X(IBITS,KPTR(8),MSGA,KBMS)
              IF (KPDS(3).EQ.25) THEN
                  KADD     = 71
              ELSE IF (KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
                  KADD     = 90
              ELSE
                  KADD     = 36
              END IF
              DO 25 I = 1, KADD
                  KBMS(I+IBITS)  = .FALSE.
   25         CONTINUE
              KPTR(10)   = KPTR(10) + KADD
              RETURN
          ELSE IF (KPDS(3).EQ.23.OR.KPDS(3).EQ.24.OR.KPDS(3).EQ.26.
     *             OR.KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
C                    SOUTHERN HEMISPHERE  23, 24, 26, 63, 64
              CALL FI634X(IBITS,KPTR(8),MSGA,KBMS)
              IF (KPDS(3).EQ.26) THEN
                  KADD     = 72
              ELSE IF (KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
                  KADD     = 91
              ELSE
                  KADD     = 37
              END IF
              DO 26 I = 1, KADD
                  KBMS(I+IBITS)  = .FALSE.
   26         CONTINUE
              KPTR(10)   = KPTR(10) + KADD - 1
              RETURN
          ELSE IF (KPDS(3).EQ.50) THEN
              KPAD    = 7
              KIN     = 22
              KBITS   = 0
              DO 55 I = 1, 7
                  DO 54 J = 1, 4
                      DO 51 K = 1, KPAD
                          KBITS   = KBITS + 1
                          KBMS(KBITS)  = .FALSE.
   51                 CONTINUE
                      CALL FI634X(KIN,KPTR(8),MSGA,KBMS(KBITS+1))
                      KPTR(8)=KPTR(8)+KIN
                      KBITS=KBITS+KIN
                      DO 53 K = 1, KPAD
                          KBITS   = KBITS + 1
                          KBMS(KBITS)  = .FALSE.
   53                 CONTINUE
   54             CONTINUE
                  KIN    = KIN + 2
                  KPAD   = KPAD - 1
   55         CONTINUE
              DO 57 II = 1, 5
                  CALL FI634X(KIN,KPTR(8),MSGA,KBMS(KBITS+1))
                  KPTR(8)=KPTR(8)+KIN
                  KBITS=KBITS+KIN
   57         CONTINUE
          ELSE
C                        EXTRACT BIT MAP FROM BMS FOR OTHER GRIDS
              CALL FI634X(IBITS,KPTR(8),MSGA,KBMS)
          END IF
          RETURN
        ELSE
C         PRINT *,'FI634-NO PREDEFINED BIT MAP PROVIDED BY THIS CENTER'
          KRET = 12
          RETURN
        END IF
C
      END IF
      KRET = 0
C  -------------------------------------------------------
C                   PROCESS NON-STANDARD GRID
C  -------------------------------------------------------
      IF (KPDS(3).EQ.255) THEN
C         PRINT *,'NON STANDARD GRID, CENTER = ',KPDS(1)
          J      = KGDS(2) * KGDS(3)
          KPTR(10) = J
          DO 600 I = 1, J
              KBMS(I) = .TRUE.
  600     CONTINUE
          RETURN
      END IF
C  -------------------------------------------------------
C                   CHECK INTERNATIONAL SET
C  -------------------------------------------------------
      IF (KPDS(3).EQ.21.OR.KPDS(3).EQ.22) THEN
C                   ----- INT'L GRIDS 21, 22 - MAP SIZE 1369
          J   = 1369
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 820
          DO 3021 I = 1, 1369
              KBMS(I) = GRD21(I)
 3021     CONTINUE
          RETURN
      ELSE IF (KPDS(3).EQ.23.OR.KPDS(3).EQ.24) THEN
C                   ----- INT'L GRIDS 23, 24 - MAP SIZE 1369
          J   = 1369
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 820
          DO 3023 I = 1, 1369
              KBMS(I) = GRD23(I)
 3023     CONTINUE
          RETURN
      ELSE IF (KPDS(3).EQ.25) THEN
C                   ----- INT'L GRID 25 - MAP SIZE 1368
          J   = 1368
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 820
          DO 3025 I = 1, 1368
              KBMS(I) = GRD25(I)
 3025     CONTINUE
          RETURN
      ELSE IF (KPDS(3).EQ.26) THEN
C                  ----- INT'L GRID  26 - MAP SIZE 1368
          J   = 1368
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 820
          DO 3026 I = 1, 1368
              KBMS(I) = GRD26(I)
 3026     CONTINUE
          RETURN
      ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.44) THEN
C                  ----- INT'L GRID  37-44 - MAP SIZE 3447
          J   = 3447
          GO TO 800
      ELSE IF (KPDS(1).EQ.7.AND.KPDS(3).EQ.50) THEN
C                   ----- INT'L GRIDS 50 - MAP SIZE 964
          J     = 1188
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 890
          DO 3050 I = 1, J
              KBMS(I) = GRD50(I)
 3050     CONTINUE
          RETURN
      ELSE IF (KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
C                   ----- INT'L GRIDS 61, 62 - MAP SIZE 4186
          J     = 4186
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 820
          DO 3061 I = 1, 4186
              KBMS(I) = GRD61(I)
 3061     CONTINUE
          RETURN
      ELSE IF (KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
C                  ----- INT'L GRIDS 63, 64 - MAP SIZE 4186
          J     = 4186
          KPTR(10)  = J
          CALL FI637(J,KPDS,KGDS,KRET)
          IF(KRET.NE.0) GO TO 820
          DO 3063 I = 1, 4186
              KBMS(I) = GRD63(I)
 3063     CONTINUE
          RETURN
      END IF
C  -------------------------------------------------------
C                   CHECK UNITED STATES SET
C  -------------------------------------------------------
      IF (KPDS(1).EQ.7) THEN
          IF (KPDS(3).LT.100) THEN
              IF (KPDS(3).EQ.1) THEN
C                       ----- U.S. GRID 1 - MAP SIZE 1679
                  J   = 1679
                  GO TO 800
              END IF
              IF (KPDS(3).EQ.2) THEN
C                       ----- U.S. GRID 2 - MAP SIZE 10512
                  J   = 10512
                  GO TO 800
              ELSE IF (KPDS(3).EQ.3) THEN
C                       ----- U.S. GRID 3 - MAP SIZE 65160
                  J   = 65160
                  GO TO 800
              ELSE IF (KPDS(3).EQ.4) THEN
C                       ----- U.S. GRID 4 - MAP SIZE 259920
                  J   = 259920
                  GO TO 800
              ELSE IF (KPDS(3).EQ.5) THEN
C                       ----- U.S. GRID 5 - MAP SIZE 3021
                  J   = 3021
                  GO TO 800
              ELSE IF (KPDS(3).EQ.6) THEN
C                       ----- U.S. GRID 6 - MAP SIZE 2385
                  J   = 2385
                  GO TO 800
              ELSE IF (KPDS(3).EQ.8) THEN
C                       ----- U.S. GRID 8 - MAP SIZE 5104
                  J   = 5104
                  GO TO 800
              ELSE IF (KPDS(3).EQ.10) THEN
C                       ----- U.S. GRID 10 - MAP SIZE 25020
                  J   = 25020
                  GO TO 800
              ELSE IF (KPDS(3).EQ.11) THEN
C                       ----- U.S. GRID 11 - MAP SIZE 223920
                  J   = 223920
                  GO TO 800
              ELSE IF (KPDS(3).EQ.12) THEN
C                       ----- U.S. GRID 12 - MAP SIZE 99631
                  J   = 99631
                  GO TO 800
              ELSE IF (KPDS(3).EQ.13) THEN
C                       ----- U.S. GRID 13 - MAP SIZE 36391
                  J   = 36391
                  GO TO 800
              ELSE IF (KPDS(3).EQ.14) THEN
C                       ----- U.S. GRID 14 - MAP SIZE 153811
                  J   = 153811
                  GO TO 800
              ELSE IF (KPDS(3).EQ.15) THEN
C                       ----- U.S. GRID 15 - MAP SIZE 74987
                  J   = 74987
                  GO TO 800
              ELSE IF (KPDS(3).EQ.16) THEN
C                       ----- U.S. GRID 16 - MAP SIZE 214268
                  J   = 214268
                  GO TO 800
              ELSE IF (KPDS(3).EQ.17) THEN
C                       ----- U.S. GRID 17 - MAP SIZE 387136
                  J   = 387136
                  GO TO 800
              ELSE IF (KPDS(3).EQ.18) THEN
C                       ----- U.S. GRID 18 - MAP SIZE 281866
                  J   = 281866
                  GO TO 800
              ELSE IF (KPDS(3).EQ.27.OR.KPDS(3).EQ.28) THEN
C                       ----- U.S. GRIDS 27, 28 - MAP SIZE 4225
                  J     = 4225
                  GO TO 800
              ELSE IF (KPDS(3).EQ.29.OR.KPDS(3).EQ.30) THEN
C                       ----- U.S. GRIDS 29,30 - MAP SIZE 5365
                  J     = 5365
                  GO TO 800
              ELSE IF (KPDS(3).EQ.33.OR.KPDS(3).EQ.34) THEN
C                       ----- U.S GRID 33, 34 - MAP SIZE 8326
                  J     = 8326
                  GO TO 800
              ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.44) THEN
C                  -----  U.S. GRID  37-44 - MAP SIZE 3447
                  J   = 3447
                  GO TO 800
              ELSE IF (KPDS(3).EQ.45) THEN
C                  ----- U.S.  GRID  45    - MAP SIZE 41760
                  J   = 41760
                  GO TO 800
              ELSE IF (KPDS(3).EQ.53) THEN
C                  ----- U.S.  GRID  53    - MAP SIZE 5967
                  J   = 5967
                  GO TO 800
              ELSE IF (KPDS(3).EQ.55.OR.KPDS(3).EQ.56) THEN
C                       ----- U.S GRID 55, 56 - MAP SIZE 6177
                  J     = 6177
                  GO TO 800
              ELSE IF (KPDS(3).GE.67.AND.KPDS(3).LE.71) THEN
C                       ----- U.S GRID 67-71 - MAP SIZE 13689
                  J     = 13689
                  GO TO 800
              ELSE IF (KPDS(3).EQ.72) THEN
C                       ----- U.S GRID    72 - MAP SIZE 406
                  J     = 406
                  GO TO 800
              ELSE IF (KPDS(3).EQ.73) THEN
C                       ----- U.S GRID    73 - MAP SIZE 13056
                  J     = 13056
                  GO TO 800
              ELSE IF (KPDS(3).EQ.74) THEN
C                       ----- U.S GRID    74 - MAP SIZE 10800
                  J     = 10800
                  GO TO 800
              ELSE IF (KPDS(3).GE.75.AND.KPDS(3).LE.77) THEN
C                       ----- U.S GRID 75-77 - MAP SIZE 12321
                  J     = 12321
                  GO TO 800
              ELSE IF (KPDS(3).EQ.83) THEN
C                       ----- U.S GRID 83 - MAP SIZE 429786
                  J     = 429786
                  GO TO 800
              ELSE IF (KPDS(3).EQ.85.OR.KPDS(3).EQ.86) THEN
C                       ----- U.S GRID 85,86 - MAP SIZE 32400
                  J     = 32400
                  GO TO 800
              ELSE IF (KPDS(3).EQ.87) THEN
C                       ----- U.S GRID 87     - MAP SIZE 5022
                  J     = 5022
                  GO TO 800
              ELSE IF (KPDS(3).EQ.88) THEN
C                       ----- U.S GRID 88     - MAP SIZE 317840
                  J     = 317840
                  GO TO 800
              ELSE IF (KPDS(3).EQ.90) THEN
C                       ----- U.S GRID 90     - MAP SIZE 11807617
                  J     = 11807617
                  GO TO 800
              ELSE IF (KPDS(3).EQ.91) THEN
C                       ----- U.S GRID 91     - MAP SIZE 1822145
                  J     = 1822145
                  GO TO 800
              ELSE IF (KPDS(3).EQ.92) THEN
C                       ----- U.S GRID 92     - MAP SIZE 7283073
                  J     = 7283073
                  GO TO 800
              ELSE IF (KPDS(3).EQ.93) THEN
C                       ----- U.S GRID 93     - MAP SIZE 111723
                  J     = 111723
                  GO TO 800
              ELSE IF (KPDS(3).EQ.94) THEN
C                       ----- U.S GRID 94     - MAP SIZE 371875
                  J     = 371875
                  GO TO 800
              ELSE IF (KPDS(3).EQ.95) THEN
C                       ----- U.S GRID 95     - MAP SIZE 130325
                  J     = 130325
                  GO TO 800
              ELSE IF (KPDS(3).EQ.96) THEN
C                       ----- U.S GRID 96     - MAP SIZE 209253
                  J     = 209253
                  GO TO 800
              ELSE IF (KPDS(3).EQ.97) THEN
C                       ----- U.S GRID 97     - MAP SIZE 1508100
                  J     = 1508100
                  GO TO 800
              ELSE IF (KPDS(3).EQ.98) THEN
C                       ----- U.S GRID 98     - MAP SIZE 18048
                  J     = 18048
                  GO TO 800
              ELSE IF (KPDS(3).EQ.99) THEN
C                       ----- U.S GRID 99     - MAP SIZE 779385
                  J     = 779385
                  GO TO 800
              END IF
          ELSE IF (KPDS(3).GE.100.AND.KPDS(3).LT.200) THEN
              IF (KPDS(3).EQ.100) THEN
C                       ----- U.S. GRID 100 - MAP SIZE 6889
                  J     = 6889
                  GO TO 800
              ELSE IF (KPDS(3).EQ.101) THEN
C                    ----- U.S. GRID 101 - MAP SIZE 10283
                  J     = 10283
                  GO TO 800
              ELSE IF (KPDS(3).EQ.103) THEN
C                     ----- U.S. GRID 103 - MAP SIZE 3640
                  J     = 3640
                  GO TO 800
              ELSE IF (KPDS(3).EQ.104) THEN
C                     ----- U.S. GRID 104 - MAP SIZE 16170
                  J     = 16170
                  GO TO 800
              ELSE IF (KPDS(3).EQ.105) THEN
C                 ----- U.S. GRID 105 - MAP SIZE 6889
                  J     = 6889
                  GO TO 800
              ELSE IF (KPDS(3).EQ.106) THEN
C                     ----- U.S. GRID 106 - MAP SIZE 19305
                  J     = 19305
                  GO TO 800
              ELSE IF (KPDS(3).EQ.107) THEN
C                 ----- U.S. GRID 107 - MAP SIZE 11040
                  J     = 11040
                  GO TO 800
              ELSE IF (KPDS(3).EQ.110) THEN
C                 ----- U.S. GRID 110 - MAP SIZE 103936
                  J     = 103936
                  GO TO 800
              ELSE IF (KPDS(3).EQ.120) THEN
C                 ----- U.S. GRID 120 - MAP SIZE 2020800
                  J     = 2020800
                  GO TO 800
              ELSE IF (KPDS(3).EQ.122) THEN
C                 ----- U.S. GRID 122 - MAP SIZE 162750
                  J     = 162750
                  GO TO 800
              ELSE IF (KPDS(3).EQ.123) THEN
C                 ----- U.S. GRID 123 - MAP SIZE 100800
                  J     = 100800
                  GO TO 800
              ELSE IF (KPDS(3).EQ.124) THEN
C                 ----- U.S. GRID 124 - MAP SIZE 75360
                  J     = 75360
                  GO TO 800
              ELSE IF (KPDS(3).EQ.125) THEN
C                 ----- U.S. GRID 125 - MAP SIZE 102000
                  J     = 102000
                  GO TO 800
              ELSE IF (KPDS(3).EQ.126) THEN
C                 ----- U.S. GRID 126 - MAP SIZE 72960
                  J     = 72960
                  GO TO 800
              ELSE IF (KPDS(3).EQ.127) THEN
C                 ----- U.S. GRID 127 - MAP SIZE 294912
                  J     = 294912
                  GO TO 800
              ELSE IF (KPDS(3).EQ.128) THEN
C                 ----- U.S. GRID 128 - MAP SIZE 663552
                  J     = 663552
                  GO TO 800
              ELSE IF (KPDS(3).EQ.129) THEN
C                 ----- U.S. GRID 129 - MAP SIZE 1548800
                  J     = 1548800
                  GO TO 800
              ELSE IF (KPDS(3).EQ.130) THEN
C                 ----- U.S. GRID 130 - MAP SIZE 151987
                  J     = 151987
                  GO TO 800
              ELSE IF (KPDS(3).EQ.132) THEN
C                 ----- U.S. GRID 132 - MAP SIZE 385441
                  J     = 385441
                  GO TO 800
              ELSE IF (KPDS(3).EQ.138) THEN
C                 ----- U.S. GRID 138 - MAP SIZE 134784
                  J     = 134784
                  GO TO 800
              ELSE IF (KPDS(3).EQ.139) THEN
C                 ----- U.S. GRID 139 - MAP SIZE 4160
                  J     = 4160
                  GO TO 800
              ELSE IF (KPDS(3).EQ.140) THEN
C                 ----- U.S. GRID 140 - MAP SIZE 32437
                  J     = 32437
                  GO TO 800
C
              ELSE IF (KPDS(3).EQ.145) THEN
C                 ----- U.S. GRID 145 - MAP SIZE 24505
                  J     = 24505
                  GO TO 800
              ELSE IF (KPDS(3).EQ.146) THEN
C                 ----- U.S. GRID 146 - MAP SIZE 23572
                  J     = 23572
                  GO TO 800
              ELSE IF (KPDS(3).EQ.147) THEN
C                 ----- U.S. GRID 147 - MAP SIZE 69412
                  J     = 69412
                  GO TO 800
              ELSE IF (KPDS(3).EQ.148) THEN
C                 ----- U.S. GRID 148 - MAP SIZE 117130
                  J     = 117130
                  GO TO 800
              ELSE IF (KPDS(3).EQ.150) THEN
C                 ----- U.S. GRID 150 - MAP SIZE 806010
                  J     = 806010
                  GO TO 800
              ELSE IF (KPDS(3).EQ.151) THEN
C                 ----- U.S. GRID 151 - MAP SIZE 205062
                  J     = 205062
                  GO TO 800
              ELSE IF (KPDS(3).EQ.160) THEN
C                 ----- U.S. GRID 160 - MAP SIZE 28080
                  J     = 28080
                  GO TO 800
              ELSE IF (KPDS(3).EQ.161) THEN
C                 ----- U.S. GRID 161 - MAP SIZE 14111
                  J     = 14111
                  GO TO 800
              ELSE IF (KPDS(3).EQ.163) THEN
C                 ----- U.S. GRID 163 - MAP SIZE 727776
                  J     = 727776
                  GO TO 800
              ELSE IF (KPDS(3).EQ.170) THEN
C                 ----- U.S. GRID 170 - MAP SIZE 131072
                  J     =  131072
                  GO TO 800
              ELSE IF (KPDS(3).EQ.171) THEN
C                 ----- U.S. GRID 171 - MAP SIZE 716100
                  J     = 716100
                  GO TO 800
              ELSE IF (KPDS(3).EQ.172) THEN
C                 ----- U.S. GRID 172 - MAP SIZE 489900
                  J     = 489900
                  GO TO 800
              ELSE IF (KPDS(3).EQ.173) THEN
C                 ----- U.S. GRID 173 - MAP SIZE 9331200
                  J     = 9331200
                  GO TO 800
              ELSE IF (KPDS(3).EQ.174) THEN
C                 ----- U.S. GRID 174 - MAP SIZE 4147200
                  J     = 4147200
                  GO TO 800
              ELSE IF (KPDS(3).EQ.175) THEN
C                 ----- U.S. GRID 175 - MAP SIZE 185704
                  J     = 185704
                  GO TO 800
              ELSE IF (KPDS(3).EQ.176) THEN
C                 ----- U.S. GRID 176 - MAP SIZE 76845
                  J     = 76845
                  GO TO 800
              ELSE IF (KPDS(3).EQ.179) THEN
C                 ----- U.S. GRID 179 - MAP SIZE 977132
                  J     = 977132
                  GO TO 800
              ELSE IF (KPDS(3).EQ.180) THEN
C                 ----- U.S. GRID 180 - MAP SIZE 267168
                  J     = 267168
                  GO TO 800
              ELSE IF (KPDS(3).EQ.181) THEN
C                 ----- U.S. GRID 181 - MAP SIZE 102860
                  J     = 102860
                  GO TO 800
              ELSE IF (KPDS(3).EQ.182) THEN
C                 ----- U.S. GRID 182 - MAP SIZE 64218
                  J     = 64218
                  GO TO 800
              ELSE IF (KPDS(3).EQ.183) THEN
C                 ----- U.S. GRID 183 - MAP SIZE 180144
                  J     = 180144
                  GO TO 800
              ELSE IF (KPDS(3).EQ.184) THEN
C                 ----- U.S. GRID 184 - MAP SIZE 2953665
                  J     = 2953665
                  GO TO 800
              ELSE IF (KPDS(3).EQ.187) THEN
C                 ----- U.S. GRID 187 - MAP SIZE 3425565
                  J     = 3425565
                  GO TO 800
              ELSE IF (KPDS(3).EQ.188) THEN
C                 ----- U.S. GRID 188 - MAP SIZE 563655
                  J     = 563655
                  GO TO 800
              ELSE IF (KPDS(3).EQ.189) THEN
C                 ----- U.S. GRID 189 - MAP SIZE 560025
                  J     = 560025
                  GO TO 800
              ELSE IF (KPDS(3).EQ.190) THEN
C                 ----- U.S GRID 190  - MAP SIZE 796590
                  J     = 796590
                  GO TO 800
              ELSE IF (KPDS(3).EQ.192) THEN
C                 ----- U.S GRID 192  - MAP SIZE 91719
                  J     = 91719
                  GO TO 800
              ELSE IF (KPDS(3).EQ.193) THEN
C                 ----- U.S GRID 193  - MAP SIZE 1038240
                  J     = 1038240
                  GO TO 800
              ELSE IF (KPDS(3).EQ.194) THEN
C                 ----- U.S GRID 194  - MAP SIZE 168640
                  J     = 168640
                  GO TO 800
              ELSE IF (KPDS(3).EQ.195) THEN
C                 ----- U.S. GRID 195 - MAP SIZE 22833
                  J     = 22833
                  GO TO 800
              ELSE IF (KPDS(3).EQ.196) THEN
C                 ----- U.S. GRID 196 - MAP SIZE 72225
                  J     = 72225
                  GO TO 800
              ELSE IF (KPDS(3).EQ.197) THEN
C                 ----- U.S. GRID 197 - MAP SIZE 739297
                  J     = 739297
                  GO TO 800
              ELSE IF (KPDS(3).EQ.198) THEN
C                 ----- U.S. GRID 198 - MAP SIZE 456225
                  J     = 456225
                  GO TO 800
              ELSE IF (KPDS(3).EQ.199) THEN
C                 ----- U.S. GRID 199 - MAP SIZE 37249
                  J     = 37249
                  GO TO 800
              ELSE IF (IAND(KPDS(4),128).EQ.128) THEN
C                     ----- U.S. NON-STANDARD GRID
                  GO TO 895
              END IF
          ELSE IF (KPDS(3).GE.200) THEN
              IF (KPDS(3).EQ.200) THEN
                  J = 10152
                  GO TO 800
              ELSE IF (KPDS(3).EQ.201) THEN
                  J = 4225
                  GO TO 800
              ELSE IF (KPDS(3).EQ.202) THEN
                  J = 2795
                  GO TO 800
              ELSE IF (KPDS(3).EQ.203.OR.KPDS(3).EQ.205) THEN
                  J = 1755
                  GO TO 800
              ELSE IF (KPDS(3).EQ.204) THEN
                  J = 6324
                  GO TO 800
              ELSE IF (KPDS(3).EQ.206) THEN
                  J = 2091
                  GO TO 800
              ELSE IF (KPDS(3).EQ.207) THEN
                  J = 1715
                  GO TO 800
              ELSE IF (KPDS(3).EQ.208) THEN
                  J = 783
                  GO TO 800
              ELSE IF (KPDS(3).EQ.209) THEN
                  J = 61325
                  GO TO 800
              ELSE IF (KPDS(3).EQ.210) THEN
                  J = 625
                  GO TO 800
              ELSE IF (KPDS(3).EQ.211) THEN
                  J = 6045
                  GO TO 800
              ELSE IF (KPDS(3).EQ.212) THEN
                  J = 23865
                  GO TO 800
              ELSE IF (KPDS(3).EQ.213) THEN
                  J = 10965
                  GO TO 800
              ELSE IF (KPDS(3).EQ.214) THEN
                  J = 6693
                  GO TO 800
              ELSE IF (KPDS(3).EQ.215) THEN
                  J = 94833
                  GO TO 800
              ELSE IF (KPDS(3).EQ.216) THEN
                  J = 14873
                  GO TO 800
              ELSE IF (KPDS(3).EQ.217) THEN
                  J = 59001
                  GO TO 800
              ELSE IF (KPDS(3).EQ.218) THEN
                  J = 262792
                  GO TO 800
              ELSE IF (KPDS(3).EQ.219) THEN
                  J = 179025
                  GO TO 800
              ELSE IF (KPDS(3).EQ.220) THEN
                  J = 122475
                  GO TO 800
              ELSE IF (KPDS(3).EQ.221) THEN
                  J = 96673
                  GO TO 800
              ELSE IF (KPDS(3).EQ.222) THEN
                  J = 15456
                  GO TO 800
              ELSE IF (KPDS(3).EQ.223) THEN
                  J = 16641
                  GO TO 800
              ELSE IF (KPDS(3).EQ.224) THEN
                  J = 4225
                  GO TO 800
              ELSE IF (KPDS(3).EQ.225) THEN
                  J = 24975
                  GO TO 800
              ELSE IF (KPDS(3).EQ.226) THEN
                  J = 381029
                  GO TO 800
              ELSE IF (KPDS(3).EQ.227) THEN
                  J = 1509825
                  GO TO 800
              ELSE IF (KPDS(3).EQ.228) THEN
                  J = 10512
                  GO TO 800
              ELSE IF (KPDS(3).EQ.229) THEN
                  J = 65160
                  GO TO 800
              ELSE IF (KPDS(3).EQ.230) THEN
                  J = 259920
                  GO TO 800
              ELSE IF (KPDS(3).EQ.231) THEN
                  J = 130320
                  GO TO 800
              ELSE IF (KPDS(3).EQ.232) THEN
                  J = 32760
                  GO TO 800
              ELSE IF (KPDS(3).EQ.233) THEN
                  J = 45216
                  GO TO 800
              ELSE IF (KPDS(3).EQ.234) THEN
                  J = 16093
                  GO TO 800
              ELSE IF (KPDS(3).EQ.235) THEN
                  J = 259200
                  GO TO 800
              ELSE IF (KPDS(3).EQ.236) THEN
                  J = 17063
                  GO TO 800
              ELSE IF (KPDS(3).EQ.237) THEN
                  J = 2538
                  GO TO 800
              ELSE IF (KPDS(3).EQ.238) THEN
                  J = 55825
                  GO TO 800
              ELSE IF (KPDS(3).EQ.239) THEN
                  J = 19065
                  GO TO 800
              ELSE IF (KPDS(3).EQ.240) THEN
                  J = 987601
                  GO TO 800
              ELSE IF (KPDS(3).EQ.241) THEN
                  J = 244305
                  GO TO 800
              ELSE IF (KPDS(3).EQ.242) THEN
                  J = 235025
                  GO TO 800
              ELSE IF (KPDS(3).EQ.243) THEN
                  J = 12726
                  GO TO 800
              ELSE IF (KPDS(3).EQ.244) THEN
                  J = 55825
                  GO TO 800
              ELSE IF (KPDS(3).EQ.245) THEN
                  J = 124992
                  GO TO 800
              ELSE IF (KPDS(3).EQ.246) THEN
                  J = 123172
                  GO TO 800
              ELSE IF (KPDS(3).EQ.247) THEN
                  J = 124992
                  GO TO 800
              ELSE IF (KPDS(3).EQ.248) THEN
                  J = 13635
                  GO TO 800
              ELSE IF (KPDS(3).EQ.249) THEN
                  J = 125881
                  GO TO 800
              ELSE IF (KPDS(3).EQ.250) THEN
                  J = 13635
                  GO TO 800
              ELSE IF (KPDS(3).EQ.251) THEN
                  J = 69720
                  GO TO 800
              ELSE IF (KPDS(3).EQ.252) THEN
                  J = 67725
                  GO TO 800
              ELSE IF (KPDS(3).EQ.253) THEN
                  J = 83552
                  GO TO 800
              ELSE IF (KPDS(3).EQ.254) THEN
                  J = 110700
                  GO TO 800
              ELSE IF (IAND(KPDS(4),128).EQ.128) THEN
                  GO TO 895
              END IF
              KRET  = 5
              RETURN
          END IF
      END IF
C  -------------------------------------------------------
C                   CHECK JAPAN METEOROLOGICAL AGENCY SET
C  -------------------------------------------------------
      IF (KPDS(1).EQ.34) THEN
          IF (IAND(KPDS(4),128).EQ.128) THEN
C             PRINT *,'JMA MAP IS NOT PREDEFINED, THE GDS WILL'
C             PRINT *,'BE USED TO UNPACK THE DATA, MAP = ',KPDS(3)
              GO TO 900
          END IF
      END IF
C  -------------------------------------------------------
C                   CHECK CANADIAN SET
C  -------------------------------------------------------
      IF (KPDS(1).EQ.54) THEN
          IF (IAND(KPDS(4),128).EQ.128) THEN
C             PRINT *,'CANADIAN MAP IS NOT PREDEFINED, THE GDS WILL'
C             PRINT *,'BE USED TO UNPACK THE DATA, MAP = ',KPDS(3)
              GO TO 900
          END IF
      END IF
C  -------------------------------------------------------
C                   CHECK FNOC SET
C  -------------------------------------------------------
      IF (KPDS(1).EQ.58) THEN
          IF (KPDS(3).EQ.220.OR.KPDS(3).EQ.221) THEN
C                      FNOC GRID 220, 221 - MAPSIZE 3969 (63 * 63)
              J  = 3969
              KPTR(10)  = J
              DO I = 1, J
                  KBMS(I)  = .TRUE.
              END DO
              RETURN
          END IF
          IF (KPDS(3).EQ.223) THEN
C                      FNOC GRID 223 - MAPSIZE 10512 (73 * 144)
              J  = 10512
              KPTR(10)  = J
              DO I = 1, J
                  KBMS(I)  = .TRUE.
              END DO
              RETURN
          END IF
          IF (IAND(KPDS(4),128).EQ.128) THEN
C             PRINT *,'FNOC MAP IS NOT PREDEFINED, THE GDS WILL'
C             PRINT *,'BE USED TO UNPACK THE DATA, MAP = ',KPDS(3)
              GO TO 900
          END IF
      END IF
C  -------------------------------------------------------
C                   CHECK UKMET SET
C  -------------------------------------------------------
      IF (KPDS(1).EQ.74) THEN
          IF (IAND(KPDS(4),128).EQ.128) THEN
              GO TO 820
          END IF
      END IF
C  -------------------------------------------------------
C                   CHECK ECMWF SET
C  -------------------------------------------------------
      IF (KPDS(1).EQ.98) THEN
          IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
              IF (KPDS(3).GE.5.AND.KPDS(3).LE.8) THEN
                  J     = 1073
              ELSE
                  J     = 1369
              END IF
              KPTR(10)  = J
              CALL FI637(J,KPDS,KGDS,KRET)
              IF(KRET.NE.0) GO TO 810
              KPTR(10)  = J  ! Reset For Modified J
              DO 1000 I = 1, J
                  KBMS(I) = .TRUE.
 1000         CONTINUE
              RETURN
          ELSE IF (KPDS(3).GE.13.AND.KPDS(3).LE.16) THEN
              J         = 361
              KPTR(10)  = J
              CALL FI637(J,KPDS,KGDS,KRET)
              IF(KRET.NE.0) GO TO 810
              DO 1013 I = 1, J
                  KBMS(I) = .TRUE.
 1013         CONTINUE
              RETURN
          ELSE IF (IAND(KPDS(4),128).EQ.128) THEN
                  GO TO 810
          ELSE
              KRET  = 5
              RETURN
          END IF
      ELSE
C         PRINT *,'CENTER ',KPDS(1),' IS NOT DEFINED'
          IF (IAND(KPDS(4),128).EQ.128) THEN
C             PRINT *,'GDS WILL BE USED TO UNPACK THE DATA',
C    *                        ' MAP = ',KPDS(3)
              GO TO 900
          ELSE
              KRET  = 10
              RETURN
          END IF
      END IF
C =======================================
C
  800 CONTINUE
      KPTR(10)  = J
      CALL FI637 (J,KPDS,KGDS,KRET)
      IF(KRET.NE.0) GO TO 801
      DO 2201 I = 1, J
          KBMS(I)  = .TRUE.
 2201 CONTINUE
      RETURN
  801 CONTINUE
C
C  ----- THE MAP HAS A GDS, BYTE 7 OF THE (PDS) THE GRID IDENTIFICATION
C  ----- IS NOT 255, THE SIZE OF THE GRID IS NOT THE SAME AS THE
C  ----- PREDEFINED SIZES OF THE U.S. GRIDS, OR KNOWN GRIDS OF THE
C  ----- OF THE OTHER CENTERS. THE GRID CAN BE UNKNOWN, OR FROM AN
C  ----- UNKNOWN CENTER, WE WILL USE THE INFORMATION IN THE GDS TO MAKE
C  ----- A BIT MAP.
C
  810 CONTINUE
C     PRINT *,'ECMWF PREDEFINED MAP SIZE DOES NOT MATCH, I WILL USE'
      GO TO 895
C
  820 CONTINUE
C     PRINT *,'U.K. MET PREDEFINED MAP SIZE DOES NOT MATCH, I WILL USE'
      GO TO 895
C
  890 CONTINUE
C     PRINT *,'PREDEFINED MAP SIZE DOES NOT MATCH, I WILL USE'
  895 CONTINUE
C     PRINT *,'THE GDS TO UNPACK THE DATA, MAP TYPE = ',KPDS(3)
C
  900 CONTINUE
        J      = KGDS(2) * KGDS(3)
C                    AFOS AFOS AFOS        SPECIAL CASE
C                             INVOLVES NEXT SINGLE STATEMENT ONLY
        IF (KPDS(3).EQ.211) KRET = 0
        KPTR(10) = J
        DO 2203 I = 1, J
          KBMS(I) = .TRUE.
 2203   CONTINUE
C     PRINT *,'EXIT FI634'
      RETURN
      END
C-----------------------------------------------------------------------

C> @brief Extract bit map.
C> @author Mark Iredell @date 1997-09-19

C> Extract the packed bitmap into a logical array.
C>
C> Program history log:
C>   97-09-19 Vectorized bitmap decoder.
C>
C> @param[in] NPTS XInteger number of points in the bitmap field
C> @param[in] NSKP Integer number of bits to skip in grib message
C> @param[in] MSGA Character*1 grib message
C> @param[out] KBMS Logical*1 bitmap
C>
C> @note Subprogram can be called from a multiprocessing environment.
C>
C> @author Mark Iredell @date 1997-09-19

      SUBROUTINE FI634X(NPTS,NSKP,MSGA,KBMS)

      CHARACTER*1   MSGA(*)
      LOGICAL*1     KBMS(NPTS)
      INTEGER       ICHK(NPTS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL GBYTESC(MSGA,ICHK,NSKP,1,0,NPTS)
      KBMS=ICHK.NE.0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END


C> @brief Extract grib data elements from bds
C> @author Bill Cavanaugh @date 1991-09-13

C> Extract grib data from binary data section and place
C> into output array in proper position.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-09-13
C> - Bill Cavanaugh 1994-04-01 Modified code to include decimal scaling when
C> calculating the value of data points specified
C> as being equal to the reference value
C> - Farley 1994-11-10 Increased mxsize from 72960 to 260000
C> for .5 degree sst analysis fields.
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - Mark Iredell 1998-08-31 Eliminated need for mxsize
C>
C> @param[in] MSGA Array containing grib message
C> @param[inout] KPTR Array containing storage for following parameters
C> - 1 Total length of grib message
C> - 2 Length of indicator (section  0)
C> - 3 Length of pds (section  1)
C> - 4 Length of gds (section  2)
C> - 5 Length of bms (section  3)
C> - 6 Length of bds (section  4)
C> - 7 Value of current byte
C> - 8 Bit pointer
C> - 9 Grib start bit nr
C> - 10 Grib/grid element count
C> - 11 Nr unused bits at end of section 3
C> - 12 Bit map flag
C> - 13 Nr unused bits at end of section 2
C> - 14 Bds flags
C> - 15 Nr unused bits at end of section 4
C> - 16 Reserved
C> - 17 Reserved
C> - 18 Reserved
C> - 19 Binary scale factor
C> - 20 Num bits used to pack each datum
C> @param[in] KPDS Array containing pds elements.
C> See initial routine
C> @param[in] KGDS Array containing gds elements.
C>  - 1) Data representation type
C>  - 19 Number of vertical coordinate parameters
C>  - 20 Octet number of the list of vertical coordinate
C>  parameters Or Octet number of the list of numbers of points
C>  in each row Or 255 if neither are present.
C>  - 21 For grids with pl, number of points in grid
C>  - 22 Number of words in each row
C> - Longitude grids
C>  - 2) N(i) nr points on latitude circle
C>  - 3) N(j) nr points on longitude meridian
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Resolution flag
C>  - 7) La(2) latitude of extreme point
C>  - 8) Lo(2) longitude of extreme point
C>  - 9) Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Polar stereographic grids
C>  - 2) N(i) nr points along lat circle
C>  - 3) N(j) nr points along lon circle
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Reserved
C>  - 7) Lov grid orientation
C>  - 8) Dx - x direction increment
C>  - 9) Dy - y direction increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode
C> - Spherical harmonic coefficients
C>  - 2 J pentagonal resolution parameter
C>  - 3 K pentagonal resolution parameter
C>  - 4 M pentagonal resolution parameter
C>  - 5 Representation type
C>  - 6 Coefficient storage mode
C> - Mercator grids
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of last grid point
C>  - 8 Lo(2) longitude of last grid point
C>  - 9 Latin - latitude of projection intersection
C>  - 10 Reserved
C>  - 11 Scanning mode flag
C>  - 12 Longitudinal dir grid length
C>  - 13 Latitudinal dir grid length
C> - Lambert conformal grids
C>  - 2 Nx nr points along x-axis
C>  - 3 Ny nr points along y-axis
C>  - 4 La1 lat of origin (lower left)
C>  - 5 Lo1 lon of origin (lower left)
C>  - 6 Resolution (right adj copy of octet 17)
C>  - 7 Lov - orientation of grid
C>  - 8 Dx - x-dir increment
C>  - 9 Dy - y-dir increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode flag
C>  - 12 Latin 1 - first lat from pole of secant cone inter
C>  - 13 Latin 2 - second lat from pole of secant cone inter
C> - Staggered arakawa rotated lat/lon grids (203 e stagger)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Staggered arakawa rotated lat/lon grids (205 a,b,c,d staggers)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C>  - 12 Latitude of last point
C>  - 13 Longitude of last point
C> @param[in] KBMS Bitmap describing location of output elements.
C> -KBDS Information extracted from binary data section
C>  - KBDS(1)  - N1
C>  - KBDS(2)  - N2
C>  - KBDS(3)  - P1
C>  - KBDS(4)  - P2
C>  - KBDS(5)  - Bit pointer to 2nd order widths
C>  - KBDS(6)  - Bit pointer to 2nd order bit maps
C>  - KBDS(7)  - Bit pointer to first order values
C>  - KBDS(8)  - Bit pointer to second order values
C>  - KBDS(9)  - Bit pointer start of bds
C>  - KBDS(10) - Bit pointer main bit map
C>  - KBDS(11) - Binary scaling
C>  - KBDS(12) - Decimal scaling
C>  - KBDS(13) - Bit width of first order values
C>  - KBDS(14) - Bit map flag
C> 0 = no second order bit map
C> 1 = second order bit map present
C> - KBDS(15) - Second order bit width
C> - KBDS(16) - Constant / different widths
C> 0 = constant widths
C> 1 = different widths
C> - KBDS(17) - Single datum / matrix
C>  - 0 = single datum at each grid point
C>  - 1 = matrix of values at each grid point
C>  - (18-20) - Unused
C> @param[out] DATA Real*4 array of gridded elements in grib message.
C> @param[out] KRET Error return
C>
C> @note
C> - Error return
C>  - 3 = Unpacked field is larger than 65160
C>  - 6 = Does not match nr of entries for this grib/grid
C>  - 7 = Number of bits in fill too large
C>
C> @author Bill Cavanaugh @date 1991-09-13
      SUBROUTINE FI635(MSGA,KPTR,KPDS,KGDS,KBMS,DATA,KRET)

C
      CHARACTER*1   MSGA(*)
C
      LOGICAL*1     KBMS(*)
C
      INTEGER       KPDS(*)
      INTEGER       KGDS(*)
      INTEGER       KBDS(20)
      INTEGER       KPTR(*)
      INTEGER       NRBITS
      INTEGER,ALLOCATABLE::  KSAVE(:)
      INTEGER       KSCALE
C
      REAL          DATA(*)
      REAL          REFNCE
      REAL          SCALE
      REAL          REALKK
C
C
C     CHANGED HEX VALUES TO DECIMAL TO MAKE CODE MORE PORTABLE
C
C  *************************************************************
C     PRINT *,'ENTER FI635'
C              SET UP BIT POINTER
      KPTR(8) = KPTR(9) + (KPTR(2)*8) + (KPTR(3)*8) + (KPTR(4)*8)
     *                + (KPTR(5)*8) + 24
C  ------------- EXTRACT FLAGS
C            BYTE 4
      CALL GBYTEC(MSGA,KPTR(14),KPTR(8),4)
      KPTR(8)  = KPTR(8) + 4
C  --------- NR OF UNUSED BITS IN SECTION 4
      CALL GBYTEC(MSGA,KPTR(15),KPTR(8),4)
      KPTR(8)  = KPTR(8) + 4
      KEND    = KPTR(9) + (KPTR(2)*8) + (KPTR(3)*8) + (KPTR(4)*8)
     *                + (KPTR(5)*8) + KPTR(6) * 8 - KPTR(15)
C  ------------- GET SCALE FACTOR
C            BYTES 5,6
C                                  CHECK SIGN
      CALL GBYTEC (MSGA,KSIGN,KPTR(8),1)
      KPTR(8)  = KPTR(8) + 1
C                                  GET ABSOLUTE SCALE VALUE
      CALL GBYTEC (MSGA,KSCALE,KPTR(8),15)
      KPTR(8)  = KPTR(8) + 15
      IF (KSIGN.GT.0) THEN
          KSCALE  = - KSCALE
      END IF
      SCALE = 2.0**KSCALE
      KPTR(19)=KSCALE
C  ------------ GET REFERENCE VALUE
C            BYTES 7,10
C      CALL GBYTE (MSGA,KREF,KPTR(8),32)
      call gbytec(MSGA,JSGN,KPTR(8),1)
      call gbytec(MSGA,JEXP,KPTR(8)+1,7)
      call gbytec(MSGA,IFR,KPTR(8)+8,24)
      KPTR(8)  = KPTR(8) + 32
C
C     THE NEXT CODE WILL CONVERT THE IBM370 FLOATING POINT
C     TO THE FLOATING POINT USED ON YOUR COMPUTER.
C
C
C     PRINT *,109,JSGN,JEXP,IFR
C 109 FORMAT (' JSGN,JEXP,IFR = ',3(1X,Z8))
      IF (IFR.EQ.0) THEN
          REFNCE  = 0.0
      ELSE IF (JEXP.EQ.0.AND.IFR.EQ.0) THEN
          REFNCE  = 0.0
      ELSE
          REFNCE  = FLOAT(IFR) * 16.0 ** (JEXP - 64 - 6)
          IF (JSGN.NE.0) REFNCE = - REFNCE
      END IF
C     PRINT *,'SCALE ',SCALE,' REF VAL ',REFNCE
C  ------------- NUMBER OF BITS SPECIFIED FOR EACH ENTRY
C            BYTE 11
      CALL GBYTEC (MSGA,KBITS,KPTR(8),8)
      KPTR(8)  = KPTR(8) + 8
      KBDS(4)  = KBITS
C     KBDS(13) = KBITS
      KPTR(20) = KBITS
      IBYT12   = KPTR(8)
C  ------------------ IF THERE ARE NO EXTENDED FLAGS PRESENT
C                     THIS IS WHERE DATA BEGINS AND AND THE PROCESSING
C                     INCLUDED IN THE FOLLOWING IF...END IF
C                     WILL BE SKIPPED
C     PRINT *,'BASIC FLAGS =',KPTR(14) ,IAND(KPTR(14),1)
      IF (IAND(KPTR(14),1).EQ.0) THEN
C         PRINT *,'NO EXTENDED FLAGS'
      ELSE
C            BYTES 12,13
          CALL GBYTEC (MSGA,KOCTET,KPTR(8),16)
          KPTR(8)  = KPTR(8) + 16
C  --------------------------- EXTENDED FLAGS
C            BYTE 14
          CALL GBYTEC (MSGA,KXFLAG,KPTR(8),8)
C         PRINT *,'HAVE EXTENDED FLAGS',KXFLAG
          KPTR(8)  = KPTR(8) + 8
          IF (IAND(KXFLAG,16).EQ.0) THEN
C                          SECOND ORDER VALUES CONSTANT WIDTHS
              KBDS(16)  = 0
          ELSE
C                          SECOND ORDER VALUES DIFFERENT WIDTHS
              KBDS(16)  = 1
          END IF
          IF (IAND (KXFLAG,32).EQ.0) THEN
C                         NO SECONDARY BIT MAP
              KBDS(14)  = 0
          ELSE
C                         HAVE SECONDARY BIT MAP
              KBDS(14)  = 1
          END IF
          IF (IAND (KXFLAG,64).EQ.0) THEN
C                         SINGLE DATUM AT GRID POINT
              KBDS(17)  = 0
          ELSE
C                         MATRIX OF VALUES AT GRID POINT
              KBDS(17)  = 1
          END IF
C  ---------------------- NR - FIRST DIMENSION (ROWS) OF EACH MATRIX
C            BYTES 15,16
          CALL GBYTEC (MSGA,NR,KPTR(8),16)
          KPTR(8)  = KPTR(8) + 16
C  ---------------------- NC - SECOND DIMENSION (COLS) OF EACH MATRIX
C            BYTES 17,18
          CALL GBYTEC (MSGA,NC,KPTR(8),16)
          KPTR(8)  = KPTR(8) + 16
C  ---------------------- NRV - FIRST DIM COORD VALS
C            BYTE 19
          CALL GBYTEC (MSGA,NRV,KPTR(8),8)
          KPTR(8)  = KPTR(8) + 8
C  ---------------------- NC1 - NR COEFF'S OR VALUES
C            BYTE 20
          CALL GBYTEC (MSGA,NC1,KPTR(8),8)
          KPTR(8)  = KPTR(8) + 8
C  ---------------------- NCV - SECOND DIM COORD OR VALUE
C            BYTE 21
          CALL GBYTEC (MSGA,NCV,KPTR(8),8)
          KPTR(8)  = KPTR(8) + 8
C  ---------------------- NC2 - NR COEFF'S OR VALS
C            BYTE 22
          CALL GBYTEC (MSGA,NC2,KPTR(8),8)
          KPTR(8)  = KPTR(8) + 8
C  ---------------------- KPHYS1 - FIRST DIM PHYSICAL SIGNIF
C            BYTE 23
          CALL GBYTEC (MSGA,KPHYS1,KPTR(8),8)
          KPTR(8)  = KPTR(8) + 8
C  ---------------------- KPHYS2 - SECOND DIM PHYSICAL SIGNIF
C            BYTE 24
          CALL GBYTEC (MSGA,KPHYS2,KPTR(8),8)
          KPTR(8)  = KPTR(8) + 8
C            BYTES 25-N
      END IF
      IF (KBITS.EQ.0) THEN
C                       HAVE NO BDS ENTRIES, ALL ENTRIES = REFNCE
          SCAL10  = 10.0 ** KPDS(22)
          SCAL10  = 1.0 / SCAL10
          REFN10  = REFNCE * SCAL10
          KENTRY = KPTR(10)
          DO 210 I = 1, KENTRY
              DATA(I) = 0.0
              IF (KBMS(I)) THEN
                   DATA(I) = REFN10
              END IF
  210     CONTINUE
          GO TO 900
      END IF
C     PRINT *,'KEND ',KEND,' KPTR(8) ',KPTR(8),'KBITS ',KBITS
      KNR     = (KEND - KPTR(8)) / KBITS
C     PRINT *,'NUMBER OF ENTRIES IN DATA ARRAY',KNR
C  --------------------
C       CYCLE THRU BDS UNTIL HAVE USED ALL (SPECIFIED NUMBER)
C       ENTRIES.
C  ------------- UNUSED BITS IN DATA AREA
C NUMBER OF BYTES IN DATA AREA
      NRBYTE  = KPTR(6) - 11
C  ------------- TOTAL NR OF USABLE BITS
      NRBITS  = NRBYTE * 8  - KPTR(15)
C  ------------- TOTAL NR OF ENTRIES
      KENTRY = NRBITS / KBITS
C                             ALLOCATE KSAVE
      ALLOCATE(KSAVE(KENTRY))
C
C     IF (IAND(KPTR(14),2).EQ.0) THEN
C        PRINT *,'SOURCE VALUES IN FLOATING POINT'
C     ELSE
C        PRINT *,'SOURCE VALUES IN INTEGER'
C     END IF
C
      IF (IAND(KPTR(14),8).EQ.0) THEN
C        PRINT *,'PROCESSING GRID POINT DATA'
         IF (IAND(KPTR(14),4).EQ.0) THEN
C            PRINT *,'    WITH SIMPLE PACKING'
             IF (IAND(KPTR(14),1).EQ.0) THEN
C                PRINT *,'        WITH NO ADDITIONAL FLAGS'
                 GO TO 4000
             ELSE IF (IAND(KPTR(14),1).NE.0) THEN
C                PRINT *,'        WITH ADDITIONAL FLAGS',KXFLAG
                 IF (KBDS(17).EQ.0) THEN
C                    PRINT *,'            SINGLE DATUM EACH GRID PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                          ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                            ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                             PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                             ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 ELSE IF (KBDS(17).NE.0) THEN
C                    PRINT *,'            MATRIX OF VALS EACH PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                          ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                             ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 END IF
             END IF
         ELSE IF (IAND(KPTR(14),4).NE.0) THEN
C            PRINT *,'    WITH COMPLEX/SECOND ORDER PACKING'
             IF (IAND(KPTR(14),1).EQ.0) THEN
C                    PRINT *,'        WITH NO ADDITIONAL FLAGS'
             ELSE IF (IAND(KPTR(14),1).NE.0) THEN
C                PRINT *,'        WITH ADDITIONAL FLAGS'
                 IF (KBDS(17).EQ.0) THEN
C                    PRINT *,'            SINGLE DATUM AT EACH PT'
                     IF (KBDS(14).EQ.0) THEN
C                            PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                             ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
C                                       ROW BY ROW - COL BY COL
                         CALL FI636 (DATA,MSGA,KBMS,
     *                                         REFNCE,KPTR,KPDS,KGDS)
                         GO TO 900
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                                PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                                PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                         CALL FI636 (DATA,MSGA,KBMS,
     *                                         REFNCE,KPTR,KPDS,KGDS)
                         GO TO 900
                     END IF
                 ELSE IF (KBDS(17).NE.0) THEN
C                    PRINT *,'            MATRIX OF VALS EACH PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                              PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                              PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                                PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 END IF
             END IF
         END IF
      ELSE IF (IAND(KPTR(14),8).NE.0) THEN
C        PRINT *,'PROCESSING SPHERICAL HARMONIC COEFFICIENTS'
         IF (IAND(KPTR(14),4).EQ.0) THEN
C            PRINT *,'    WITH SIMPLE PACKING'
             IF (IAND(KPTR(14),1).EQ.0) THEN
C                PRINT *,'        WITH NO ADDITIONAL FLAGS'
                 GO TO 5000
             ELSE IF (IAND(KPTR(14),1).NE.0) THEN
C                PRINT *,'        WITH ADDITIONAL FLAGS'
                 IF (KBDS(17).EQ.0) THEN
C                    PRINT *,'            SINGLE DATUM EACH GRID PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                            ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 ELSE IF (KBDS(17).NE.0) THEN
C                    PRINT *,'            MATRIX OF VALS EACH PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                             ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                             ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 END IF
             END IF
         ELSE IF (IAND(KPTR(14),4).NE.0) THEN
C                                  COMPLEX/SECOND ORDER PACKING
C            PRINT *,'    WITH COMPLEX/SECOND ORDER PACKING'
             IF (IAND(KPTR(14),1).EQ.0) THEN
C                PRINT *,'        WITH NO ADDITIONAL FLAGS'
             ELSE IF (IAND(KPTR(14),1).NE.0) THEN
C                PRINT *,'        WITH ADDITIONAL FLAGS'
                 IF (KBDS(17).EQ.0) THEN
C                    PRINT *,'            SINGLE DATUM EACH GRID PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                             ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 ELSE IF (KBDS(17).NE.0) THEN
C                    PRINT *,'            MATRIX OF VALS EACH PT'
                     IF (KBDS(14).EQ.0) THEN
C                        PRINT *,'            NO SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                            ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     ELSE IF (KBDS(14).NE.0) THEN
C                        PRINT *,'            SEC BIT MAP'
                         IF (KBDS(16).EQ.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES CONSTANT WIDTH'
                         ELSE IF (KBDS(16).NE.0) THEN
C                            PRINT *,'            SECOND ORDER',
C    *                              ' VALUES DIFFERENT WIDTHS'
                         END IF
                     END IF
                 END IF
             END IF
         END IF
      END IF
      IF(ALLOCATED(KSAVE)) DEALLOCATE(KSAVE)
C     PRINT *,' NOT PROCESSED - NOT PROCESSED - NOT PROCESSED'
      KRET   = 11
      RETURN
 4000 CONTINUE
C  ****************************************************************
C
C GRID POINT DATA, SIMPLE PACKING, FLOATING POINT, NO ADDN'L FLAGS
C
      SCAL10  = 10.0 ** KPDS(22)
      SCAL10  = 1.0 / SCAL10
      IF (KPDS(3).EQ.23.OR.KPDS(3).EQ.24.OR.KPDS(3).EQ.26.
     *            OR.KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
          IF (KPDS(3).EQ.26) THEN
              KADD    = 72
          ELSE IF (KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
              KADD    = 91
          ELSE
              KADD    = 37
          END IF
          CALL GBYTESC (MSGA,KSAVE,KPTR(8),KBITS,0,KNR)
          KPTR(8)   = KPTR(8) + KBITS * KNR
          II        = 1
          KENTRY    = KPTR(10)
          DO 4001 I = 1, KENTRY
              IF (KBMS(I)) THEN
                  DATA(I)   = (REFNCE+FLOAT(KSAVE(II))*SCALE)*SCAL10
                  II        = II + 1
              ELSE
                  DATA(I)   = 0.0
              END IF
 4001     CONTINUE
          DO 4002 I = 2, KADD
              DATA(I)   = DATA(1)
 4002     CONTINUE
      ELSE IF (KPDS(3).EQ.21.OR.KPDS(3).EQ.22.OR.KPDS(3).EQ.25.
     *            OR.KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
          CALL GBYTESC (MSGA,KSAVE,KPTR(8),KBITS,0,KNR)
          II    = 1
          KENTRY = KPTR(10)
          DO 4011 I = 1, KENTRY
              IF (KBMS(I)) THEN
                  DATA(I) = (REFNCE + FLOAT(KSAVE(II)) * SCALE) * SCAL10
                  II  = II + 1
              ELSE
                  DATA(I) = 0.0
              END IF
 4011     CONTINUE
          IF (KPDS(3).EQ.25) THEN
              KADD    = 71
          ELSE IF (KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
              KADD    = 90
          ELSE
              KADD    = 36
          END IF
          LASTP   = KENTRY - KADD
          DO 4012 I = LASTP+1, KENTRY
              DATA(I) = DATA(LASTP)
 4012     CONTINUE
      ELSE
          CALL GBYTESC (MSGA,KSAVE,KPTR(8),KBITS,0,KNR)
          II    = 1
          KENTRY = KPTR(10)
          DO 500 I = 1, KENTRY
              IF (KBMS(I)) THEN
                  DATA(I) = (REFNCE + FLOAT(KSAVE(II)) * SCALE) * SCAL10
                  II  = II + 1
              ELSE
                  DATA(I) = 0.0
              END IF
  500     CONTINUE
      END IF
      GO TO 900
C  ------------- PROCESS SPHERICAL HARMONIC COEFFICIENTS,
C               SIMPLE PACKING, FLOATING POINT, NO ADDN'L FLAGS
 5000 CONTINUE
C     PRINT *,'CHECK POINT SPECTRAL COEFF'
      KPTR(8)  = IBYT12
C      CALL GBYTE (MSGA,KKK,KPTR(8),32)
      call gbytec(MSGA,JSGN,KPTR(8),1)
      call gbytec(MSGA,JEXP,KPTR(8)+1,7)
      call gbytec(MSGA,IFR,KPTR(8)+8,24)
      KPTR(8)  = KPTR(8) + 32
C
C     THE NEXT CODE WILL CONVERT THE IBM370 FOATING POINT
C     TO THE FLOATING POINT USED ON YOUR MACHINE.
C
      IF (IFR.EQ.0) THEN
          REALKK  = 0.0
      ELSE IF (JEXP.EQ.0.AND.IFR.EQ.0) THEN
          REALKK  = 0.0
      ELSE
          REALKK  = FLOAT(IFR) * 16.0 ** (JEXP - 64 - 6)
          IF (JSGN.NE.0) REALKK  = -REALKK
      END IF
      DATA(1)  = REALKK
      CALL GBYTESC (MSGA,KSAVE,KPTR(8),KBITS,0,KNR)
C  --------------
      DO 6000 I = 1, KENTRY
          DATA(I+1)  = REFNCE + FLOAT(KSAVE(I)) * SCALE
 6000 CONTINUE
  900 CONTINUE
      IF(ALLOCATED(KSAVE)) DEALLOCATE(KSAVE)
C     PRINT *,'EXIT FI635'
      RETURN
      END

C> @brief Process second order packing.
C> @author Bill Cavanaugh @date 1992-09-22

C> Process second order packing from the binary data section
C> (bds) for single data items grid point data.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-06-08
C> - Bill Cavanaugh 1993-12-15 Modified second order pointers to first order
C> values and second order values correctly.
C> - Ralph Jones 1995-04-26 Fi636 corection for 2nd order complex
C> Unpacking.
C> - Mark Iredell 1995-10-31 Saves and prints.
C>
C> @param[in] MSGA Array containing grib message
C> @param[in] REFNCE Reference value
C> @param[in] KPTR Work array
C> @param[out] DATA Location of output array
C> - KBDS Working array
C>  - KBDS(1) N1
C>  - KBDS(2) N2
C>  - KBDS(3) P1
C>  - KBDS(4) P2
C>  - KBDS(5) Bit pointer to 2nd order widths
C>  - KBDS(6) Bit pointer to 2nd order bit maps
C>  - KBDS(7) Bit pointer to first order values
C>  - KBDS(8) Bit pointer to second order values
C>  - KBDS(9) Bit pointer start of bds
C>  - KBDS(10) Bit pointer main bit map
C>  - KBDS(11) Binary scaling
C>  - KBDS(12) Decimal scaling
C>  - KBDS(13) Bit width of first order values
C>  - KBDS(14) Bit map flag
C>   - 0 = No second order bit map
C>   - 1 = Second order bit map present
C>  - KBDS(15) Second order bit width
C>  - KBDS(16) Constant / different widths
C>   - 0 = Constant widths
C>   - 1 = Different widths
C>  - KBDS(17) Single datum / matrix
C>   - 0 = Single datum at each grid point
C>   - 1 = Matrix of values at each grid point
C>  - KBDS(18-20) Unused
C> @param[in] KBMS
C> @param[in] KPDS
C> @param[in] KGDS Array containing gds elements.
C>  - 1) Data representation type
C>  - 19 Number of vertical coordinate parameters
C>  - 20 Octet number of the list of vertical coordinate
C>  parameters Or Octet number of the list of numbers of points
C>  in each row Or 255 if neither are present.
C>  - 21 For grids with pl, number of points in grid
C>  - 22 Number of words in each row
C> - Longitude grids
C>  - 2) N(i) nr points on latitude circle
C>  - 3) N(j) nr points on longitude meridian
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Resolution flag
C>  - 7) La(2) latitude of extreme point
C>  - 8) Lo(2) longitude of extreme point
C>  - 9) Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Polar stereographic grids
C>  - 2) N(i) nr points along lat circle
C>  - 3) N(j) nr points along lon circle
C>  - 4) La(1) latitude of origin
C>  - 5) Lo(1) longitude of origin
C>  - 6) Reserved
C>  - 7) Lov grid orientation
C>  - 8) Dx - x direction increment
C>  - 9) Dy - y direction increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode
C> - Spherical harmonic coefficients
C>  - 2 J pentagonal resolution parameter
C>  - 3 K pentagonal resolution parameter
C>  - 4 M pentagonal resolution parameter
C>  - 5 Representation type
C>  - 6 Coefficient storage mode
C> - Mercator grids
C>  - 2 N(i) nr points on latitude circle
C>  - 3 N(j) nr points on longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of last grid point
C>  - 8 Lo(2) longitude of last grid point
C>  - 9 Latin - latitude of projection intersection
C>  - 10 Reserved
C>  - 11 Scanning mode flag
C>  - 12 Longitudinal dir grid length
C>  - 13 Latitudinal dir grid length
C> - Lambert conformal grids
C>  - 2 Nx nr points along x-axis
C>  - 3 Ny nr points along y-axis
C>  - 4 La1 lat of origin (lower left)
C>  - 5 Lo1 lon of origin (lower left)
C>  - 6 Resolution (right adj copy of octet 17)
C>  - 7 Lov - orientation of grid
C>  - 8 Dx - x-dir increment
C>  - 9 Dy - y-dir increment
C>  - 10 Projection center flag
C>  - 11 Scanning mode flag
C>  - 12 Latin 1 - first lat from pole of secant cone inter
C>  - 13 Latin 2 - second lat from pole of secant cone inter
C> - Staggered arakawa rotated lat/lon grids (203 e stagger)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C> - Staggered arakawa rotated lat/lon grids (205 a,b,c,d staggers)
C>  - 2 N(i) nr points on rotated latitude circle
C>  - 3 N(j) nr points on rotated longitude meridian
C>  - 4 La(1) latitude of origin
C>  - 5 Lo(1) longitude of origin
C>  - 6 Resolution flag
C>  - 7 La(2) latitude of center
C>  - 8 Lo(2) longitude of center
C>  - 9 Di longitudinal direction of increment
C>  - 10 Dj latitudinal direction increment
C>  - 11 Scanning mode flag
C>  - 12 Latitude of last point
C>  - 13 Longitude of last point
C>
C> @author Bill Cavanaugh @date 1992-09-22
      SUBROUTINE FI636 (DATA,MSGA,KBMS,REFNCE,KPTR,KPDS,KGDS)

      REAL         DATA(*)
      REAL         REFN
      REAL         REFNCE
C
      INTEGER      KBDS(20)
      INTEGER      KPTR(*)
      character(len=1)      BMAP2(1000000)
      INTEGER      I,IBDS
      INTEGER      KBIT,IFOVAL,ISOVAL
      INTEGER      KPDS(*),KGDS(*)
C
      LOGICAL*1    KBMS(*)
C
      CHARACTER*1  MSGA(*)
C
C  *******************     SETUP     ******************************
C     PRINT *,'ENTER FI636'
C                                START OF BMS (BIT POINTER)
      DO I = 1,20
        KBDS(I)  = 0
      END DO
C                BYTE START OF BDS
      IBDS  = KPTR(2) + KPTR(3) + KPTR(4) + KPTR(5)
C     PRINT *,'KPTR(2-5) ',KPTR(2),KPTR(3),KPTR(4),KPTR(5)
C                BIT START OF BDS
      JPTR  = IBDS * 8
C     PRINT *,'JPTR ',JPTR
      KBDS(9) = JPTR
C     PRINT *,'START OF BDS         ',KBDS(9)
C                    BINARY SCALE VALUE  BDS BYTES 5-6
      CALL GBYTEC (MSGA,ISIGN,JPTR+32,1)
      CALL GBYTEC (MSGA,KBDS(11),JPTR+33,15)
      IF (ISIGN.GT.0) THEN
          KBDS(11)  = - KBDS(11)
      END IF
C     PRINT *,'BINARY SCALE VALUE =',KBDS(11)
C                  EXTRACT REFERENCE VALUE
C      CALL GBYTEC(MSGA,JREF,JPTR+48,32)
      call gbytec(MSGA,JSGN,KPTR(8),1)
      call gbytec(MSGA,JEXP,KPTR(8)+1,7)
      call gbytec(MSGA,IFR,KPTR(8)+8,24)
      IF (IFR.EQ.0) THEN
          REFNCE  = 0.0
      ELSE IF (JEXP.EQ.0.AND.IFR.EQ.0) THEN
          REFNCE  = 0.0
      ELSE
          REFNCE  = FLOAT(IFR) * 16.0 ** (JEXP - 64 - 6)
          IF (JSGN.NE.0) REFNCE = - REFNCE
      END IF
C     PRINT *,'DECODED REFERENCE VALUE =',REFN,REFNCE
C                F O BIT WIDTH
      CALL GBYTEC(MSGA,KBDS(13),JPTR+80,8)
      JPTR  = JPTR + 88
C              AT START OF BDS BYTE 12
C                EXTRACT N1
      CALL GBYTEC (MSGA,KBDS(1),JPTR,16)
C     PRINT *,'N1  = ',KBDS(1)
      JPTR  = JPTR + 16
C                 EXTENDED FLAGS
      CALL GBYTEC (MSGA,KFLAG,JPTR,8)
C                 ISOLATE BIT MAP FLAG
      IF (IAND(KFLAG,32).NE.0) THEN
        KBDS(14)  = 1
      ELSE
        KBDS(14)  = 0
      END IF
      IF (IAND(KFLAG,16).NE.0) THEN
        KBDS(16)  = 1
      ELSE
        KBDS(16)  = 0
      END IF
      IF (IAND(KFLAG,64).NE.0) THEN
        KBDS(17)  = 1
      ELSE
        KBDS(17)  = 0
      END IF
      JPTR  = JPTR + 8
C                EXTRACT N2
      CALL GBYTEC (MSGA,KBDS(2),JPTR,16)
C     PRINT *,'N2  = ',KBDS(2)
      JPTR  = JPTR + 16
C                EXTRACT P1
      CALL GBYTEC (MSGA,KBDS(3),JPTR,16)
C     PRINT *,'P1  = ',KBDS(3)
      JPTR  = JPTR + 16
C                EXTRACT P2
      CALL GBYTEC (MSGA,KBDS(4),JPTR,16)
C     PRINT *,'P2  = ',KBDS(4)
      JPTR  = JPTR + 16
C                 SKIP RESERVED BYTE
      JPTR    = JPTR + 8
C                START OF SECOND ORDER BIT WIDTHS
      KBDS(5) = JPTR
C                COMPUTE START OF SECONDARY BIT MAP
      IF (KBDS(14).NE.0) THEN
C                           FOR INCLUDED SECONDARY BIT MAP
          JPTR    = JPTR + (KBDS(3) * 8)
          KBDS(6) = JPTR
      ELSE
C                           FOR CONSTRUCTED SECONDARY BIT MAP
          KBDS(6)  = 0
      END IF
C                CREATE POINTER TO START OF FIRST ORDER VALUES
      KBDS(7) =  KBDS(9) + KBDS(1) * 8 - 8
C     PRINT *,'BIT POINTER TO START OF FOVALS',KBDS(7)
C                CREATE POINTER TO START OF SECOND ORDER VALUES
      KBDS(8) =  KBDS(9) + KBDS(2) * 8 - 8
C     PRINT *,'BIT POINTER TO START OF SOVALS',KBDS(8)
C     PRINT *,'KBDS( 1) - N1                         ',KBDS( 1)
C     PRINT *,'KBDS( 2) - N2                         ',KBDS( 2)
C     PRINT *,'KBDS( 3) - P1                         ',KBDS( 3)
C     PRINT *,'KBDS( 4) - P2                         ',KBDS( 4)
C     PRINT *,'KBDS( 5) - BIT PTR - 2ND ORDER WIDTHS ',KBDS( 5)
C     PRINT *,'KBDS( 6) -  "   "     "   " BIT MAPS  ',KBDS( 6)
C     PRINT *,'KBDS( 7) -  "   "     F O VALS        ',KBDS( 7)
C     PRINT *,'KBDS( 8) -  "   "     S O VALS        ',KBDS( 8)
C     PRINT *,'KBDS( 9) -  "   "    START OF BDS     ',KBDS( 9)
C     PRINT *,'KBDS(10) -  "   "    MAIN BIT MAP     ',KBDS(10)
C     PRINT *,'KBDS(11) - BINARY SCALING             ',KBDS(11)
C     PRINT *,'KPDS(22) - DECIMAL SCALING            ',KPDS(22)
C     PRINT *,'KBDS(13) - FO BIT WIDTH               ',KBDS(13)
C     PRINT *,'KBDS(14) - 2ND ORDER BIT MAP FLAG     ',KBDS(14)
C     PRINT *,'KBDS(15) - 2ND ORDER BIT WIDTH        ',KBDS(15)
C     PRINT *,'KBDS(16) - CONSTANT/DIFFERENT WIDTHS  ',KBDS(16)
C     PRINT *,'KBDS(17) - SINGLE DATUM/MATRIX        ',KBDS(17)
C     PRINT *,'REFNCE VAL                            ',REFNCE
C  ************************* PROCESS DATA  **********************
      IJ  = 0
C  ========================================================
      IF (KBDS(14).EQ.0) THEN
C                           NO BIT MAP, MUST CONSTRUCT ONE
          IF (KGDS(2).EQ.65535) THEN
              IF (KGDS(20).EQ.255) THEN
C                 PRINT *,'CANNOT BE USED HERE'
              ELSE
C                               POINT TO PL
          LP  = KPTR(9) + KPTR(2)*8 + KPTR(3)*8 + KGDS(20)*8 - 8
C                 PRINT *,'LP = ',LP
                  JT  = 0
                  DO 2000 JZ = 1, KGDS(3)
C                               GET NUMBER IN CURRENT ROW
                      CALL GBYTEC (MSGA,NUMBER,LP,16)
C                               INCREMENT TO NEXT ROW NUMBER
                      LP  = LP + 16
C                     PRINT *,'NUMBER IN ROW',JZ,' = ',NUMBER
                      DO 1500 JQ = 1, NUMBER
                          IF (JQ.EQ.1) THEN
                              CALL SBYTEC (BMAP2,1,JT,1)
                          ELSE
                              CALL SBYTEC (BMAP2,0,JT,1)
                          END IF
                          JT  = JT + 1
 1500                 CONTINUE
 2000             CONTINUE
              END IF
          ELSE
              IF (IAND(KGDS(11),32).EQ.0) THEN
C                           ROW BY ROW
C                 PRINT *,'     ROW BY ROW'
                  KOUT  = KGDS(3)
                  KIN   = KGDS(2)
              ELSE
C                           COL BY COL
C                 PRINT *,'     COL BY COL'
                  KIN   = KGDS(3)
                  KOUT  = KGDS(2)
              END IF
C             PRINT *,'KIN=',KIN,' KOUT= ',KOUT
              DO 200 I = 1, KOUT
                  DO 150 J = 1, KIN
                      IF (J.EQ.1) THEN
                          CALL SBYTEC (BMAP2,1,IJ,1)
                      ELSE
                          CALL SBYTEC (BMAP2,0,IJ,1)
                      END IF
                      IJ  = IJ + 1
  150             CONTINUE
  200         CONTINUE
          END IF
      END IF
C  ========================================================
C     PRINT 99,(BMAP2(J),J=1,110)
C99   FORMAT ( 10(1X,Z8.8))
C     CALL BINARY (BMAP2,2)
C                FOR EACH GRID POINT ENTRY
C
         SCALE2  = 2.0**KBDS(11)
         SCAL10  = 10.0**KPDS(22)
C     PRINT *,'SCALE VALUES - ',SCALE2,SCAL10
      DO 1000 I = 1, KPTR(10)
C                    GET NEXT MASTER BIT MAP BIT POSITION
C                    IF NEXT MASTER BIT MAP BIT POSITION IS 'ON' (1)
          IF (KBMS(I)) THEN
C             WRITE(6,900)I,KBMS(I)
C 900         FORMAT (1X,I4,3X,14HMAIN BIT IS ON,3X,L4)
              IF (KBDS(14).NE.0) THEN
                  CALL GBYTEC (MSGA,KBIT,KBDS(6),1)
              ELSE
                  CALL GBYTEC (BMAP2,KBIT,KBDS(6),1)
              END IF
C             PRINT *,'KBDS(6) =',KBDS(6),' KBIT =',KBIT
              KBDS(6)  = KBDS(6) + 1
              IF (KBIT.NE.0) THEN
C                 PRINT *,'          SOB ON'
C                                  GET NEXT FIRST ORDER PACKED VALUE
                  CALL GBYTEC (MSGA,IFOVAL,KBDS(7),KBDS(13))
                  KBDS(7)  = KBDS(7) + KBDS(13)
C                 PRINT *,'FOVAL =',IFOVAL
C                                   GET SECOND ORDER BIT WIDTH
                  CALL GBYTEC (MSGA,KBDS(15),KBDS(5),8)
                  KBDS(5)  = KBDS(5) + 8
C                PRINT *,KBDS(7)-KBDS(13),' FOVAL =',IFOVAL,' KBDS(5)=',
C    *                           ,KBDS(5), 'ISOWID =',KBDS(15)
              ELSE
C                 PRINT *,'          SOB NOT ON'
              END IF
              ISOVAL  = 0
              IF (KBDS(15).EQ.0) THEN
C                        IF SECOND ORDER BIT WIDTH = 0
C                             THEN SECOND ORDER VALUE IS 0
C                            SO CALCULATE DATA VALUE FOR THIS POINT
C                 DATA(I) = (REFNCE + (FLOAT(IFOVAL) * SCALE2)) / SCAL10
              ELSE
                  CALL GBYTEC (MSGA,ISOVAL,KBDS(8),KBDS(15))
                  KBDS(8)  = KBDS(8) + KBDS(15)
              END IF
              DATA(I) = (REFNCE + (FLOAT(IFOVAL + ISOVAL) *
     *                         SCALE2)) / SCAL10
C             PRINT *,I,DATA(I),REFNCE,IFOVAL,ISOVAL,SCALE2,SCAL10
          ELSE
C             WRITE(6,901) I,KBMS(I)
C 901         FORMAT (1X,I4,3X,15HMAIN BIT NOT ON,3X,L4)
              DATA(I)  = 0.0
          END IF
C         PRINT *,I,DATA(I),IFOVAL,ISOVAL,KBDS(5),KBDS(15)
 1000 CONTINUE
C  **************************************************************
C     PRINT *,'EXIT FI636'
      RETURN
      END

C> @brief Grib grid/size test.
C> @author Bill Cavanaugh @date 1991-09-13

C> To test when gds is available to see if size mismatch
C> on existing grids (by center) is indicated.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-09-13
C> - Mark Iredell 1995-10-31 Removed saves and prints
C> - M. Bostelman 1997-02-12 Corrects ecmwf us grid 2 processing
C> - Mark Iredell 1998-06-17 Removed alternate return
C> - M. Baldwin 1999-01-20 Modify to handle grid 237
C> - Boi Vuong 1909-05-21 Modify to handle grid 45
C>
C> @param[inout] J Size for indicated grid modified for ecmwf-us 2
C> @param[in] KPDS
C> @param[in] KGDS
C> @param[out] KRET Error return (a mismatch was detected if kret is not zero)
C>
C> @note
C> - KRET:
C>  - 9 - Gds indicates size mismatch with std grid
C>
C> @author Bill Cavanaugh @date 1991-09-13
      SUBROUTINE FI637(J,KPDS,KGDS,KRET)

      INTEGER       KPDS(*)
      INTEGER       KGDS(*)
      INTEGER       J
      INTEGER       I
C  ---------------------------------------
C  ---------------------------------------
C           IF GDS NOT INDICATED, RETURN
C  ----------------------------------------
      KRET=0
      IF (IAND(KPDS(4),128).EQ.0) RETURN
C  ---------------------------------------
C            GDS IS INDICATED, PROCEED WITH TESTING
C  ---------------------------------------
      IF (KGDS(2).EQ.65535) THEN
          RETURN
      END IF
      KRET=1
      I     = KGDS(2) * KGDS(3)
C  ---------------------------------------
C            INTERNATIONAL SET
C  ---------------------------------------
      IF (KPDS(3).GE.21.AND.KPDS(3).LE.26) THEN
          IF (I.NE.J) THEN
               RETURN
          END IF
      ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.44) THEN
          IF (I.NE.J) THEN
              RETURN
          END IF
      ELSE IF (KPDS(3).EQ.50) THEN
          IF (I.NE.J) THEN
              RETURN
          END IF
      ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
          IF (I.NE.J) THEN
              RETURN
          END IF
C  ---------------------------------------
C            TEST ECMWF CONTENT
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.98) THEN
          KRET  = 9
          IF (KPDS(3).GE.1.AND.KPDS(3).LE.16) THEN
              IF (I.NE.J) THEN
                IF (KPDS(3) .NE. 2) THEN
                  RETURN
                ELSEIF (I .NE. 10512) THEN ! Test for US Grid 2
                  RETURN
                END IF
                J  = I   ! Set to US Grid 2, 2.5 Global
              END IF
          ELSE
              KRET  = 5
              RETURN
          END IF
C  ---------------------------------------
C           U.K. MET OFFICE, BRACKNELL
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.74) THEN
          KRET  = 9
          IF (KPDS(3).GE.25.AND.KPDS(3).LE.26) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE
              KRET  = 5
              RETURN
          END IF
C  ---------------------------------------
C           CANADA
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.54) THEN
C         PRINT *,' NO CURRENT LISTING OF CANADIAN GRIDS'
          RETURN
C  ---------------------------------------
C           JAPAN METEOROLOGICAL AGENCY
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.34) THEN
C         PRINT *,' NO CURRENT LISTING OF JMA GRIDS'
          RETURN
C  ---------------------------------------
C           NAVY - FNOC
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.58) THEN
          IF (KPDS(3).GE.37.AND.KPDS(3).LE.44) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.220.AND.KPDS(3).LE.221) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.223) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE
              KRET = 5
              RETURN
          END IF
C  ---------------------------------------
C                 U.S. GRIDS
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.7) THEN
          KRET  = 9
          IF (KPDS(3).GE.1.AND.KPDS(3).LE.6) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.8) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.10) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.11.AND.KPDS(3).LE.18) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.27.AND.KPDS(3).LE.30) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.33.AND.KPDS(3).LE.34) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.37.AND.KPDS(3).LE.45) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.53) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.55.AND.KPDS(3).LE.56) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.67.AND.KPDS(3).LE.77) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.85.AND.KPDS(3).LE.88) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.90.AND.KPDS(3).LE.99) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.100.OR.KPDS(3).EQ.101) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.103.AND.KPDS(3).LE.107) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.110) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.120) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.122.AND.KPDS(3).LE.130) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.132) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.138) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.139) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.140) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.145.AND.KPDS(3).LE.148) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.150.OR.KPDS(3).EQ.151) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.160.OR.KPDS(3).EQ.161) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.163) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.170.AND.KPDS(3).LE.176) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.179.AND.KPDS(3).LE.184) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.187) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.188) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.189) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).EQ.190.OR.KPDS(3).EQ.192) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.193.AND.KPDS(3).LE.199) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE IF (KPDS(3).GE.200.AND.KPDS(3).LE.254) THEN
              IF (I.NE.J) THEN
                  RETURN
              END IF
          ELSE
              KRET  = 5
              RETURN
          END IF
      ELSE
          KRET  = 10
          RETURN
      END IF
C  ------------------------------------
C                    NORMAL EXIT
C  ------------------------------------
      KRET  = 0
      RETURN
      END
