C> @file
C> @brief Unpack grib field to grib grid.
C> @author Bill Cavanaugh @date 1988-01-20

C> Unpack a grib field to the exact grid specified in the
C> message, isolate the bit map and make the values of the product
C> description sec (pds) and the grid description sec (gds)
C> available in return arrays.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-01-20
C> - Bill Cavanaugh 1990-05-11  To assure that all u.s. grids in the grib decoder
C> comply with size changes in the december 1989 revisions.
C> - Bill Cavanaugh 1990-05-24 Corrects searching an improper location for grib
c> version number in grib messages.
C> - William Bostelman 1990-07-15 Modiifed sub. ai084 so that it will test
C> the grib bds byte size to determine what ecmwf grid array size is
C> to be specified.
C> - Ralph Jones 1990-09-14 Change's for ansi fortran, and pds version 1.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C> - Boi Vuong 2002-10-15 Replaced function ichar with mova2i.
C>
C> @param[in] msga grib field - "grib" thru "7777"   char*1
C> @param[out] data array containing data elements
C> @note (version 0):
C> - 1: id of center
C> - 2: model identification
C> - 3: grid identification
C> - 4: gds/bms flag
C> - 5: indicator of parameter
C> - 6: type of level
C> - 7: height/pressure , etc of level
C> - 8: year including century
C> - 9: month of year
C> - 10: day of month
C> - 11: hour of day
C> - 12: minute of hour
C> - 13: indicator of forecast time unit
C> - 14: time range 1
C> - 15: time range 2
C> - 16: time range flag
C> - 17: number included in average
C> - 18: grib specification edition number
C> @param[out] kpds array containing pds elements.  (version 1)
C> - 1: id of center
C> - 2: model identification
C> - 3: grid identification
C> - 4: gds/bms flag
C> - 5: indicator of parameter
C> - 6: type of level
C> - 7: height/pressure , etc of level
C> - 8: year including century
C> - 9: month of year
C> - 10: day of month
C> - 11: hour of day
C> - 12: minute of hour
C> - 13: indicator of forecast time unit
C> - 14: time range 1
C> - 15: time range 2
C> - 16: time range flag
C> - 17: number included in average
C> - 18: version nr of grib specification
C> - 19: version nr of parameter table
C> - 20: total length of grib message (including section 0)
C> @param[out] kgds array containing gds elements.
C> - 1: data representation type
C> - Latitude/longitude grids
C>  - 2: n(i) nr points on latitude circle
C>  - 3: n(j) nr points on longitude meridian
C>  - 4: la(1) latitude of origin
C>  - 5: lo(1) longitude of origin
C>  - 6: resolution flag
C>  - 7: la(2) latitude of extreme point
C>  - 8: lo(2) longitude of extreme point
C>  - 9: di longitudinal direction of increment
C>  - 10: dj latitundinal direction of increment
C>  - 11: scanning mode flag
C> - Polar stereographic grids
C>  - 2: n(i) nr points along lat circle
C>  - 3: n(j) nr points along lon circle
C>  - 4: la(1) latitude of origin
C>  - 5: lo(1) longitude of origin
C>  - 6: reserved
C>  - 7: lov grid orientation
C>  - 8: dx - x direction increment
C>  - 9: dy - y direction increment
C>  - 10: projection center flag
C>  - 11: scanning mode
C> - Spherical harmonic coefficients
C>  - 2: j pentagonal resolution parameter
C>  - 3: k pentagonal resolution parameter
C>  - 4: m pentagonal resolution parameter
C>  - 5: representation type
C>  - 6: coefficient storage mode
C> - Mercator grids
C>  - 2: n(i) nr points on latitude circle
C>  - 3: n(j) nr points on longitude meridian
C>  - 4: la(1) latitude of origin
C>  - 5: lo(1) longitude of origin
C>  - 6: resolution flag
C>  - 7: la(2) latitude of last grid point
C>  - 8: lo(2) longitude of last grid point
C>  - 9: longit dir increment
C>  - 10: latit dir increment
C>  - 11: scanning mode flag
C>  - 12: latitude intersection
C> - Lambert conformal grids
C>  - 2: nx nr points along x-axis
C>  - 3: ny nr points along y-axis
C>  - 4: la1 lat of origin (lower left)
C>  - 5: lo1 lon of origin (lower left)
C>  - 6: reserved
C>  - 7: lov - orientation of grid
C>  - 8: dx - x-dir increment
C>  - 9: dy - y-dir increment
C>  - 10: projection center flag
C>  - 11: scanning mode flag
C>  - 12: latin 1 - first lat from pole of secant cone inter
C>  - 13: latin 2 - second lat from pole of secant cone inter
C> @param[out] kbms       - bitmap describing location of output elements.
C> @param[out] kptr       - array containing storage for following parameters
C> - 1: unused
C> - 2: unused
C> - 3: length of pds
C> - 4: length of gds
C> - 5: length of bms
C> - 6: length of bds
C> - 7: value of current byte
C> - 8: unused
C> - 9: grib start byte nr
C> - 10: grib/grid element count
C> @param[out] kret flag indicating quality of completion
C>
C> @note values for return flag (kret)
C> - kret = 0 - normal return, no errors
C>  - = 1 - 'grib' not found in first 100 chars
C>  - = 2 - '7777' not in correct location
C>  - = 3 - unpacked field is larger than 32768
C>  - = 4 - gds/ grid not one of currently accepted values
C>  - = 5 - grid not currently avail for center indicated
C>  - = 8 - temp gds indicated, but gds flag is off
C>  - = 9 - gds indicates size mismatch with std grid
C>  - = 10 - incorrect center indicator
C>
C> @author Bill Cavanaugh @date 1988-01-20
      SUBROUTINE W3AI08(MSGA,KPDS,KGDS,KBMS,DATA,KPTR,KRET)
C                                                         4 AUG 1988
C                               W3AI08
C
C
C                       GRIB UNPACKING ROUTINE
C
C
C       THIS ROUTINE WILL UNPACK A 'GRIB' FIELD TO THE EXACT GRID
C  TYPE SPECIFIED IN THE MESSAGE, RETURN A BIT MAP AND MAKE THE
C  VALUES OF THE PRODUCT DEFINITION SEC   (PDS) AND THE GRID
C  DESCRIPTION SEC   (GDS) AVAILABLE IN RETURN ARRAYS.
C  SEE "GRIB - THE WMO FORMAT FOR THE STORAGE OF WEATHER PRODUCT
C  INFORMATION AND THE EXCHANGE OF WEATHER PRODUCT MESSAGES IN
C  GRIDDED BINARY FORM" DATED JULY 1, 1988 BY JOHN D. STACKPOLE
C  DOC, NOAA, NWS, NATIONAL METEOROLOGICAL CENTER.
C
C       THE CALL TO THE GRIB UNPACKING ROUTINE IS AS FOLLOWS:
C
C            CALL W3AI08(MSGA,KPDS,KGDS,LBMS,DATA,KPTR,KRET)
C
C  INPUT:
C
C       MSGA  = CONTAINS THE GRIB MESSAGE TO BE UNPACKED. CHARACTERS
C               "GRIB" MAY BEGIN ANYWHERE WITHIN FIRST 100 BYTES.
C
C  OUTPUT:
C
C       KPDS(100)      INTEGER
C               ARRAY TO CONTAIN THE ELEMENTS OF THE PRODUCT
C               DEFINITION SEC  .
C          (VERSION 0)
C            KPDS(1)  - ID OF CENTER
C            KPDS(2)  - MODEL IDENTIFICATION (SEE "GRIB" TABLE 1)
C            KPDS(3)  - GRID IDENTIFICATION (SEE "GRIB" TABLE 2)
C            KPDS(4)  - GDS/BMS FLAG
C                           BIT       DEFINITION
C                            25        0 - GDS OMITTED
C                                      1 - GDS INCLUDED
C                            26        0 - BMS OMITTED
C                                      1 - BMS INCLUDED
C                        NOTE:- LEFTMOST BIT = 1,
C                               RIGHTMOST BIT = 32
C            KPDS(5)  - INDICATOR OF PARAMETER (SEE "GRIB" TABLE 5)
C            KPDS(6)  - TYPE OF LEVEL (SEE "GRIB" TABLES 6 & 7)
C            KPDS(7)  - HEIGHT,PRESSURE,ETC  OF LEVEL
C            KPDS(8)  - YEAR OF CENTURY
C            KPDS(9)  - MONTH OF YEAR
C            KPDS(10) - DAY OF MONTH
C            KPDS(11) - HOUR OF DAY
C            KPDS(12) - MINUTE OF HOUR
C            KPDS(13) - INDICATOR OF FORECAST TIME UNIT (SEE "GRIB"
C                       TABLE 8)
C            KPDS(14) - TIME 1               (SEE "GRIB" TABLE 8A)
C            KPDS(15) - TIME 2               (SEE "GRIB" TABLE 8A)
C            KPDS(16) - TIME RANGE INDICATOR (SEE "GRIB" TABLE 8A)
C            KPDS(17) - NUMBER INCLUDED IN AVERAGE
C            KPDS(18) - VERSION NR OF GRIB SPECIFICATION
C
C         (VERSION 1)
C            KPDS(1)  - ID OF CENTER
C            KPDS(2)  - MODEL IDENTIFICATION (SEE "GRIB" TABLE 1)
C            KPDS(3)  - GRID IDENTIFICATION (SEE "GRIB" TABLE 2)
C            KPDS(4)  - GDS/BMS FLAG
C                           BIT       DEFINITION
C                            25        0 - GDS OMITTED
C                                      1 - GDS INCLUDED
C                            26        0 - BMS OMITTED
C                                      1 - BMS INCLUDED
C                        NOTE:- LEFTMOST BIT = 1,
C                               RIGHTMOST BIT = 32
C            KPDS(5)  - INDICATOR OF PARAMETER (SEE "GRIB" TABLE 5)
C            KPDS(6)  - TYPE OF LEVEL (SEE "GRIB" TABLES 6 & 7)
C            KPDS(7)  - HEIGHT,PRESSURE,ETC  OF LEVEL
C            KPDS(8)  - YEAR INCLUDING CENTURY
C            KPDS(9)  - MONTH OF YEAR
C            KPDS(10) - DAY OF MONTH
C            KPDS(11) - HOUR OF DAY
C            KPDS(12) - MINUTE OF HOUR
C            KPDS(13) - INDICATOR OF FORECAST TIME UNIT (SEE "GRIB"
C                       TABLE 8)
C            KPDS(14) - TIME 1               (SEE "GRIB" TABLE 8A)
C            KPDS(15) - TIME 2               (SEE "GRIB" TABLE 8A)
C            KPDS(16) - TIME RANGE INDICATOR (SEE "GRIB" TABLE 8A)
C            KPDS(17) - NUMBER INCLUDED IN AVERAGE
C            KPDS(18) - VERSION NR OF GRIB SPECIFICATION
C            KPDS(19) - VERSION NR OF PARAMETER TABLE
C            KPDS(20) - TOTAL LENGTH 0F GRIB MESSAGE
C                       (INCLUDING SECTION 0)
C       KGDS(13)       INTEGER
C             ARRAY CONTAINING GDS ELEMENTS.
C
C            KGDS(1)  - DATA REPRESENTATION TYPE
C
C         LATITUDE/LONGITUDE GRIDS (SEE "GRIB" TABLE 10)
C            KGDS(2)  - N(I) NUMBER OF POINTS ON LATITUDE
C                       CIRCLE
C            KGDS(3)  - N(J) NUMBER OF POINTS ON LONGITUDE
C                       CIRCLE
C            KGDS(4)  - LA(1) LATITUDE OF ORIGIN
C            KGDS(5)  - LO(1) LONGITUDE OF ORIGIN
C            KGDS(6)  - RESOLUTION FLAG
C                           BIT       MEANING
C                            25       0 - DIRECTION INCREMENTS NOT
C                                         GIVEN
C                                     1 - DIRECTION INCREMENTS GIVEN
C            KGDS(7)  - LA(2) LATITUDE OF EXTREME POINT
C            KGDS(8)  - LO(2) LONGITUDE OF EXTREME POINT
C            KGDS(9)  - DI LONGITUDINAL DIRECTION INCREMENT
C            KGDS(10) - REGULAR LAT/LON GRID
C                           DJ - LATITUDINAL DIRECTION
C                                INCREMENT
C                       GAUSSIAN GRID
C                           N  - NUMBER OF LATITUDE CIRCLES
C                                BETWEEN A POLE AND THE EQUATOR
C            KGDS(11) - SCANNING MODE FLAG
C                           BIT       MEANING
C                            25       0 - POINTS ALONG A LATITUDE
C                                         SCAN FROM WEST TO EAST
C                                     1 - POINTS ALONG A LATITUDE
C                                         SCAN FROM EAST TO WEST
C                            26       0 - POINTS ALONG A MERIDIAN
C                                         SCAN FROM NORTH TO SOUTH
C                                     1 - POINTS ALONG A MERIDIAN
C                                         SCAN FROM SOUTH TO NORTH
C                            27       0 - POINTS SCAN FIRST ALONG
C                                         CIRCLES OF LATITUDE, THEN
C                                         ALONG MERIDIANS
C                                         (FORTRAN: (I,J))
C                                     1 - POINTS SCAN FIRST ALONG
C                                         MERIDIANS THEN ALONG
C                                         CIRCLES OF LATITUDE
C                                         (FORTRAN: (J,I))
C
C         POLAR STEREOGRAPHIC GRIDS  (SEE GRIB TABLE 12)
C            KGDS(2)  - N(I) NR POINTS ALONG LAT CIRCLE
C            KGDS(3)  - N(J) NR POINTS ALONG LON CIRCLE
C            KGDS(4)  - LA(1) LATITUDE OF ORIGIN
C            KGDS(5)  - LO(1) LONGITUDE OF ORIGIN
C            KGDS(6)  - RESERVED
C            KGDS(7)  - LOV GRID ORIENTATION
C            KGDS(8)  - DX - X DIRECTION INCREMENT
C            KGDS(9)  - DY - Y DIRECTION INCREMENT
C            KGDS(10) - PROJECTION CENTER FLAG
C            KGDS(11) - SCANNING MODE
C
C         SPHERICAL HARMONIC COEFFICIENTS (SEE "GRIB" TABLE 14)
C            KGDS(2)  - J PENTAGONAL RESOLUTION PARAMETER
C            KGDS(3)  - K PENTAGONAL RESOLUTION PARAMETER
C            KGDS(4)  - M PENTAGONAL RESOLUTION PARAMETER
C            KGDS(5)  - REPRESENTATION TYPE
C            KGDS(6)  - COEFFICIENT STORAGE MODE
C
C       MERCATOR GRIDS
C            KGDS(2)   - N(I) NR POINTS ON LATITUDE CIRCLE
C            KGDS(3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
C            KGDS(4)   - LA(1) LATITUDE OF ORIGIN
C            KGDS(5)   - LO(1) LONGITUDE OF ORIGIN
C            KGDS(6)   - RESOLUTION FLAG
C            KGDS(7)   - LA(2) LATITUDE OF LAST GRID POINT
C            KGDS(8)   - LO(2) LONGITUDE OF LAST GRID POINT
C            KGDS(9)   - LONGIT DIR INCREMENT
C            KGDS(10)  - LATIT DIR INCREMENT
C            KGDS(11)  - SCANNING MODE FLAG
C            KGDS(12)  - LATITUDE INTERSECTION
C       LAMBERT CONFORMAL GRIDS
C            KGDS(2)   - NX NR POINTS ALONG X-AXIS
C            KGDS(3)   - NY NR POINTS ALONG Y-AXIS
C            KGDS(4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
C            KGDS(5)   - LO1 LON OF ORIGIN (LOWER LEFT)
C            KGDS(6)   - RESERVED
C            KGDS(7)   - LOV - ORIENTATION OF GRID
C            KGDS(8)   - DX - X-DIR INCREMENT
C            KGDS(9)   - DY - Y-DIR INCREMENT
C            KGDS(10)  - PROJECTION CENTER FLAG
C            KGDS(11)  - SCANNING MODE FLAG
C            KGDS(12)  - LATIN 1 - FIRST LAT FROM POLE OF
C                        SECANT CONE INTERSECTION
C            KGDS(13)  - LATIN 2 - SECOND LAT FROM POLE OF
C                        SECANT CONE INTERSECTION
C
C       LBMS(32768)    LOGICAL
C               ARRAY TO CONTAIN THE BIT MAP DESCRIBING THE
C               PLACEMENT OF DATA IN THE OUTPUT ARRAY.  IF A
C               BIT MAP IS NOT INCLUDED IN THE SOURCE MESSAGE,
C               ONE WILL BE GENERATED AUTOMATICALLY BY THE
C               UNPACKING ROUTINE.
C
C
C       DATA(32768)    REAL
C               THIS ARRAY WILL CONTAIN THE UNPACKED DATA POINTS.
C
C                      NOTE:- 32768 IS MAXIMUN FIELD SIZE ALLOWABLE
C
C       KPTR(10)       INTEGER
C               ARRAY CONTAINING STORAGE FOR THE FOLLOWING
C               PARAMETERS.
C
C                 (1)  -    UNUSED
C                 (2)  -    UNUSED
C                 (3)  -    LENGTH OF PDS (IN BYTES)
C                 (4)  -    LENGTH OF GDS (IN BYTES)
C                 (5)  -    LENGTH OF BMS (IN BYTES)
C                 (6)  -    LENGTH OF BDS (IN BYTES)
C                 (7)  -    USED BY UNPACKING ROUTINE
C                 (8)  -    NUMBER OF DATA POINTS FOR GRID
C                 (9)  -    "GRIB" CHARACTERS START IN BYTE NUMBER
C                 (10) -    USED BY UNPACKING ROUTINE
C
C
C       KRET      INTEGER
C                 THIS VARIABLE WILL CONTAIN THE RETURN INDICATOR.
C
C                 0    -    NO ERRORS DETECTED.
C
C                 1    -    'GRIB' NOT FOUND IN FIRST 100
C                           CHARACTERS.
C
C                 2    -    '7777' NOT FOUND, EITHER MISSING OR
C                           TOTAL OF SEC   COUNTS OF INDIVIDUAL
C                           SEC'S  IS INCORRECT.
C
C                 3    -    UNPACKED FIELD IS LARGER THAN 32768.
C
C                 4    -    IN GDS, DATA REPRESENTATION TYPE
C                           NOT ONE OF THE CURRENTLY ACCEPTABLE
C                           VALUES. SEE "GRIB" TABLE 9. VALUE
C                           OF INCORRECT TYPE RETURNED IN KGDS(1).
C
C                 5    -    GRID INDICATED IN KPDS(3) IS NOT
C                           AVAILABLE FOR THE CENTER INDICATED IN
C                           KPDS(1) AND NO GDS SENT.
C
C                 7    -    VERSION INDICATED IN KPDS(18) HAS NOT
C                           YET BEEN INCLUDED IN THE DECODER.
C
C                 8    -    GRID IDENTIFICATION = 255 (NOT STANDARD
C                           GRID) BUT FLAG INDICATING PRESENCE OF
C                           GDS IS TURNED OFF. NO METHOD OF
C                           GENERATING PROPER GRID.
C
C                 9    -    PRODUCT OF KGDS(2) AND KGDS(3) DOES NOT
C                           MATCH STANDARD NUMBER OF POINTS FOR THIS
C                           GRID (FOR OTHER THAN SPECTRALS). THIS
C                           WILL OCCUR ONLY IF THE GRID.
C                           IDENTIFICATION, KPDS(3), AND A
C                           TRANSMITTED GDS ARE INCONSISTENT.
C
C                10    -    CENTER INDICATOR WAS NOT ONE INDICATED
C                           IN "GRIB" TABLE 1.  PLEASE CONTACT AD
C                           PRODUCTION MANAGEMENT BRANCH (W/NMC42)
C                                     IF THIS ERROR IS ENCOUNTERED.
C
C
C
C  LIST OF TEXT MESSAGES FROM CODE
C
C
C  W3AI08/AI082
C
C            'HAVE ENCOUNTERED A NEW GRID FOR NMC, PLEASE NOTIFY
C            AUTOMATION DIVISION, PRODUCTION MANAGEMENT BRANCH
C            (W/NMC42)'
C
C            'HAVE ENCOUNTERED A NEW GRID FOR ECMWF, PLEASE NOTIFY
C            AUTOMATION DIVISION, PRODUCTION MANAGEMENT BRANCH
C            (W/NMC42)'
C
C            'HAVE ENCOUNTERED A NEW GRID FOR U.K. METEOROLOGICAL
C            OFFICE, BRACKNELL.  PLEASE NOTIFY AUTOMATION DIVISION,
C            PRODUCTION MANAGEMENT BRANCH (W/NMC42)'
C
C            'HAVE ENCOUNTERED A NEW GRID FOR FNOC, PLEASE NOTIFY
C            AUTOMATION DIVISION, PRODUCTION MANAGEMENT BRANCH
C            (W/NMC42)'
C
C
C  W3AI08/AI083
C
C            'POLAR STEREO PROCESSING NOT AVAILABLE'  *
C
C  W3AI08/AI084
C
C            'WARNING - BIT MAP MAY NOT BE ASSOCIATED WITH SPHERICAL
C            COEFFICIENTS'
C
C
C  W3AI08/AI087
C
C            'NO CURRENT LISTING OF FNOC GRIDS'      *
C
C
C  * WILL BE AVAILABLE IN NEXT UPDATE
C  ***************************************************************
C
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C                       BIT MAP
      LOGICAL       KBMS(*)
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
      CALL AI081(MSGA,KPTR,KPDS,KRET)
           IF (KRET.NE.0) GO TO 900
C
C                  PARSE PARAMETERS FROM PRODUCT DESCRIPTION SECTION
C
      IF (KPDS(18).EQ.0) THEN
          CALL AI082(MSGA,KPTR,KPDS,KRET)
      ELSE IF (KPDS(18).EQ.1) THEN
          CALL AI082A(MSGA,KPTR,KPDS,KRET)
      ELSE
          PRINT *,'GRIB EDITION',KPDS(18),' NOT PROGRAMMED FOR'
          KRET    = 7
          GO TO 900
      END IF
         IF (KRET.NE.0) GO TO 900
C
C                  EXTRACT NEW GRID DESCRIPTION
C
      CALL AI083(MSGA,KPTR,KPDS,KGDS,KRET)
         IF (KRET.NE.0) GO TO 900
C
C                  EXTRACT OR GENERATE BIT MAP
C
      CALL AI084(MSGA,KPTR,KPDS,KGDS,KBMS,KRET)
         IF (KRET.NE.0) GO TO 900
C
C                  USING INFORMATION FROM PDS, BMS AND BIT DATA SEC  ,
C                  EXTRACT AND SAVE IN GRIB GRID, ALL DATA ENTRIES.
C
      IF (KPDS(18).EQ.0) THEN
          CALL AI085(MSGA,KPTR,KPDS,KBMS,DATA,KRET)
      ELSE IF (KPDS(18).EQ.1) THEN
          CALL AI085A(MSGA,KPTR,KPDS,KBMS,DATA,KRET)
      ELSE
          PRINT *,'AI085 NOT PROGRAMMED FOR VERSION NR',KPDS(18)
          KRET   = 7
      END IF
C
  900 RETURN
      END

C>Find 'grib; characters and set pointers to the next
C>byte following 'grib'. If they exist extract counts from gds and
C>bms. Extract count from bds. determine if sum of counts actually
C>places terminator '7777' at the correct location.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-01-20
C> - Ralph Jones 1990-09-01 Change's for ansi fortran.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C>
C> @param[in] msga grib field - "grib" thru "7777".``
C> @param[inout] kptr array containing storage for following parameters.
C> - 1: Unused.
C> - 2: Unused.
C> - 3: Length of pds.
C> - 4: Length of gds.
C> - 5: Length of bms.
C> - 6: Length of bds.
C> - 7: Value of current byte.
C> - 8: Unused.
C> - 9: Grib start byte.
C> - 10: Grib/grid element count.
C> @param[out] kpds     - array containing pds elements..
C> - 1: Id of center.
C> - 2: Model identification.
C> - 3: Grid identification.
C> - 4: Gds/bms flag.
C> - 5: Indicator of parameter.
C> - 6: Type of level.
C> - 7: Height/pressure , etc of level.
C> - 8: Year of century.
C> - 9: Month of year.
C> - 10: Day of month.
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version nr of grib specification.
C> @param[out] kret Error return.
C>
C> @note Error returns.
C> - kret = 1: No 'grib'.
C> - kret = 2: No '7777' or mislocated (by counts).
C>
C> @author Bill Cavanaugh @date 1988-01-20
      SUBROUTINE AI081(MSGA,KPTR,KPDS,KRET)

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
C     DATA  MASK40/Z00000040/
C     DATA  MASK80/Z00000080/
C
      DATA  MASK40/64/
      DATA  MASK80/128/
C
C  ******************************************************************
      KRET = 0
C  -------------------  FIND 'GRIB' KEY
      DO 100 I = 1, 105
          IF (MOVA2I(MSGA(I  )).NE.71) GO TO 100
          IF (MOVA2I(MSGA(I+1)).NE.82) GO TO 100
          IF (MOVA2I(MSGA(I+2)).NE.73) GO TO 100
          IF (MOVA2I(MSGA(I+3)).NE.66) GO TO 100
          KPTR(9)   = I
          GO TO 200
  100 CONTINUE
        KRET  = 1
        RETURN
C
  200 CONTINUE
      IS  = KPTR(9)
C  -------------------  HAVE 'GRIB' KEY
      KCNT     = 0
C  ---------------  EXTRACT COUNT FROM PDS OR GRIB
      ISS      = IS + 4
      DO 300 I = 0, 2
          KCNT = KCNT * 256 + MOVA2I(MSGA(I+ISS))
  300 CONTINUE
C
C     TEST FOR VERSION NUMBER OF PDS  0 OR 1
C
      IF (KCNT.EQ.24) THEN
        KPTR(3)  = KCNT
        IGRIBL   = 4
C
C  ---------------  EDITION NR OF GRIB SPECIFICATION, VERSION 0
C
        KPDS(18) = MOVA2I(MSGA(ISS + 3))
      ELSE
        IGRIBL   = 8
        ISS      = IS + IGRIBL
C  ---------------  EDITION NR OF GRIB SPECIFICATION, VERSION 1
        KPDS(18) = MOVA2I(MSGA(IS + 7))
C
C  ---------------  PARAMETER TABLE VERSION NUMBER FOR INTERNATIONAL
C                   EXCHANGE (CURRENTLY NO. 1)
C
        KPDS(19) = MOVA2I(MSGA(ISS + 3))
C
C  ---------------- SAVE TOTAL LENGTH OF MESSAGE (INCLUDING SECTION 0)
C
        KPDS(20) = KCNT
C
C  ---------------  EXTRACT COUNT FROM PDS VERSION 1
C
        KCNT     = 0
        DO 400 I = 0, 2
          KCNT = KCNT * 256 + MOVA2I(MSGA(I+ISS))
  400   CONTINUE
        KPTR(3)  = KCNT
      ENDIF
C
C  ---------------  GET GDS, BMS INDICATOR
C
      KPDS(4)  = MOVA2I(MSGA(ISS+7))
C
C                   READY FOR NEXT SECTION
C
      KPTR(4)  = 0
      KPTR(5)  = 0
      IF (IAND(KPDS(4),MASK80).EQ.0) GO TO 600
C
C  ---------------  EXTRACT COUNT FROM GDS
C
      ISS      = KPTR(3) + IS + IGRIBL
      KCNT     = 0
      DO 500 I = 0, 2
          KCNT = KCNT * 256 + MOVA2I(MSGA(I+ISS))
  500 CONTINUE
      KPTR(4)  = KCNT
  600 CONTINUE
      IF (IAND(KPDS(4),MASK40).EQ.0) GO TO 800
C
C  ---------------- EXTRACT COUNT FROM BMS
C
      ISS      = KPTR(3) + KPTR(4) + IS + IGRIBL
      KCNT     = 0
      DO 700 I = 0, 2
          KCNT = KCNT * 256 + MOVA2I(MSGA(I+ISS))
  700 CONTINUE
      KPTR(5) = KCNT
C
C  ---------------  EXTRACT COUNT FROM BDS
C
  800 CONTINUE
      KCNT     = 0
      ISS      = KPTR(3) + KPTR(4) + KPTR(5) + IS + IGRIBL
      DO 900 I = 0, 2
          KCNT = KCNT * 256  + MOVA2I(MSGA(I+ISS))
  900 CONTINUE
      KPTR(6) = KCNT
C
C  ---------------  TEST FOR '7777'
C
      ISS      = KPTR(3) + KPTR(4) + KPTR(5) + KPTR(6) + IS + IGRIBL
      KRET     = 0
      DO 1000 I = 0, 3
          IF (MOVA2I(MSGA(I+ISS)).EQ.55) THEN
              GO TO 1000
          ELSE
              KRET  = 2
              RETURN
          END IF
 1000 CONTINUE
      RETURN
      END

C> Extract information from the product description
C> sec, and generate label information to permit storage
C> in office note 84 format.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-01-20
C> - Ralph Jones 1990-09-01 Change's for ansi fortran.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C>
C> @param[in] msga Array containing grib message.
C> @param[inout] kptr Array containing storage for following parameters.
C> - 1: Unused.
C> - 2: Unused.
C> - 3: Length of pds.
C> - 4: Length of gds.
C> - 5: Length of bms.
C> - 6: Length of pds.
C> - 7: Value of current byte.
C> - 8: Unused.
C> - 9: Grib start byte nr.
C> - 10: Grib/grid element count.
C> @param[out] kpds Array containing pds elements.
C> - 1: Id of center.
C> - 2: Model identification.
C> - 3: Grid identification.
C> - 4: Gds/bms flag.
C> - 5: Indicator of parameter.
C> - 6: Type of level.
C> - 7: Height/pressure, etc of level.
C> - 8: Year of century.
C> - 9: Month of year.
C> - 10: Day of month.
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version number of grib spefication.
C> - 19: Version nr of parameter table.
C> - 20: Total length of grib message (including section 0).
C> @param[out] kret error return.
C>
C> @note error return:
C> - = 0 - no errors
C> - = 8 - temp gds indicated, but no gds
C>
C> @author Bill Cavanaugh @date 1988-01-20
      SUBROUTINE AI082(MSGA,KPTR,KPDS,KRET)
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
C
C  -------------------- COLLECT PDS VALUES
C              KPDS(1)  -  ID OF CENTER
C              KPDS(2)  -  MODEL IDENTIFICATION
C              KPDS(3)  -  GRID IDENTIFICATION
C              KPDS(4)  -  GDS/BMS FLAG
C              KPDS(5)  -  INDICATOR OF PARAMETER
C  ----------- KPDS(6)  -  TYPE OF LEVEL
      IS       = KPTR(9)
      ISS      = IS + 8
      DO 200 I = 0, 5
          KPDS(I+1)  = MOVA2I(MSGA(I+ISS))
  200 CONTINUE
      IF (KPDS(3).NE.255) GO TO 250
      IF (IAND(KPDS(4),128).NE.0) GO TO 250
      KRET  = 8
      RETURN
  250 CONTINUE
      ISS      = IS + 14
      KPDS(7)  = 0
      DO 300 I = 0, 1
          KPDS(7)  = KPDS(7) * 256 + MOVA2I(MSGA(I+ISS))
  300 CONTINUE
C  ----------- KPDS(8)  -  YEAR OF CENTURY
C              KPDS(9)  -  MONTH OF YEAR
C              KPDS(10) -  DAY OF MONTH
C              KPDS(11) -  HOUR OF DAY
C              KPDS(12) -  MINUTE OF HOUR
C              KPDS(13) -  INDICATOR OF FORECAST TIME UNIT
C              KPDS(14) -  TIME RANGE 1
C              KPDS(15) -  TIME RANGE 2
C  ----------- KPDS(16) -  TIME RANGE FLAG
C
      ISS      = IS + 16
      DO 400 I = 0, 7
          KPDS(I+8)  = MOVA2I(MSGA(I+ISS))
  400 CONTINUE
C  ----------- KPDS(17) -  NUMBER INCLUDED IN AVERAGE
      ISS       = IS + 25
      KPDS(17)  = 0
      DO 500 I = 0, 1
          KPDS(17) = KPDS(17) * 256 + MOVA2I(MSGA(I+ISS))
  500 CONTINUE
C  -----------SKIP OVER SOURCE BYTE 24
C  ----------- TEST FOR NEW GRID
      IF (IAND(KPDS(4),128).NE.0) THEN
          IF (IAND(KPDS(4),64).NE.0) THEN
              IF (KPDS(3).NE.255) THEN
                  IF (KPDS(1).EQ.7) THEN
                      IF (KPDS(3).GE.21.AND.KPDS(3).LE.26) THEN
                      ELSE IF (KPDS(3).EQ.50) THEN
                      ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
                      ELSE IF (KPDS(3).EQ.70) THEN
                      ELSE IF (KPDS(3).GE.85.AND.KPDS(3).LE.86) THEN
                      ELSE IF (KPDS(3).GE.100.AND.KPDS(3).LE.103) THEN
                      ELSE IF (KPDS(3).GE.201.AND.KPDS(3).LE.214) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                    ' NMC'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.98) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.16) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                            ' ECMWF'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.74) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
                      ELSE IF (KPDS(3).GE.21.AND.KPDS(3).LE.26)THEN
                      ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
                      ELSE IF (KPDS(3).EQ.70) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                            ' U.K. MET OFFICE, BRACKNELL'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.58) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                            ' FNOC,'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  END IF
              END IF
          END IF
      END IF
      RETURN
      END

C> Extract information from the product description section (version 1).
C>
C> Program history log:
C> - Bill Cavanaugh 1989-11-20
C> - Ralph Jones 1990-09-01 Change's for ansi fortran.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C>
C> @param[in] MSGA Array containing grib message.
C> @param[inout] KPTR Array containing storage for following parameters.
C> - 1: Unused.
C> - 2: Unused.
C> - 3: Length of pds.
C> - 4: Length of gds.
C> - 5: Length of bms.
C> - 6: Length of pds.
C> - 7: Value of current byte.
C> - 8: Unused.
C> - 9: Grib start byte nr.
C> - 10: Grib/grid element count.
C>
C> @param[out] KPDS Array containing pds elements.
C> - 1: Id of center
C> - 2: Model identi.fication
C> - 3: Grid identification.
C> - 4: Gds/bms flag.
C> - 5: Indicator of. parameter
C> - 6: Type of level.
C> - 7: Height/pressu.re , etc of level
C> - 8: Year (including century).
C> - 9: Month of year.
C> - 10: Day of month..
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version nr of grib specification.
C> - 19: Version nr of parameter table.
C> - 20: Total byte count for source message.
C> @param[out] KRET Error return.
C>
C> @note Source pds structure (version 1).
C> - 1-3: Length of pds section in bytes.
C> - 4: Parameter table version no. for international exchange (crrently no. 1).
C> - 5: Center id.
C> - 6: Model id.
C> - 7: Grid id.
C> - 8: Flag for gds/bms.
C> - 9: Indicator for parameter.
C> - 10: Indicator for type of level.
C> - 11-12: Height, pressure of level.
C> - 13: Year of century.
C> - 14: Month.
C> - 15: Day.
C> - 16: Hour.
C> - 17: Minute.
C> - 18: Forecast time unit.
C> - 19: P1 - pd of time.
C> - 20: P2 - pd of time.
C> - 21: Time range indicator.
C> - 22-23: Number in average.
C> - 24: Number misg from averages.
C> - 25: Century.
C> - 26: Indicator of parameter in locally re-defined parameter table..
C> - 27-28: Units decimal scale factor (d).
C> - 29-40: Reserved: need not be present.
C> - 41-NN: National use.
C> - Error return:
C>  - = 0 - No errors.
C>  - = 8 - Temp gds indicated, but no gds.
C>
C> @author Bill Cavanaugh @date 1988-01-20
      SUBROUTINE AI082A(MSGA,KPTR,KPDS,KRET)
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
C
      IS       = KPTR(9)
      IGRIBL   = 8
C  -------------------- COLLECT PDS VALUES
C              KPDS(1)  -  ID OF CENTER
C              KPDS(2)  -  MODEL IDENTIFICATION
C              KPDS(3)  -  GRID IDENTIFICATION
C              KPDS(4)  -  GDS/BMS FLAG
C              KPDS(5)  -  INDICATOR OF PARAMETER
C  ----------- KPDS(6)  -  TYPE OF LEVEL
      ISS      = IS + IGRIBL + 4
      DO 200 I = 0, 5
          KPDS(I+1)  = MOVA2I(MSGA(I+ISS))
  200 CONTINUE
      IF (KPDS(3).NE.255) GO TO 250
      IF (IAND(KPDS(4),128).NE.0) GO TO 250
      KRET  = 8
      RETURN
  250 CONTINUE
C                         HEIGHT, PRESS OF LEVEL
      ISS      = IS + IGRIBL + 10
      KPDS(7)  = 0
      DO 300 I = 0, 1
          KPDS(7)  = KPDS(7) * 256 + MOVA2I(MSGA(I+ISS))
  300 CONTINUE
C
C  ----------- KPDS(8)  -  YEAR (INCLUDING CENTURY)
C
      ISS      = IS + IGRIBL + 12
      ICEN     = IS + IGRIBL + 24
C
      KPDS(8)  = MOVA2I(MSGA(ICEN)) * 100 + MOVA2I(MSGA(ISS))
C
C              KPDS(9)  -  MONTH OF YEAR
C              KPDS(10) -  DAY OF MONTH
C              KPDS(11) -  HOUR OF DAY
C              KPDS(12) -  MINUTE OF HOUR
C              KPDS(13) -  INDICATOR OF FORECAST TIME UNIT
C              KPDS(14) -  TIME RANGE 1
C              KPDS(15) -  TIME RANGE 2
C  ----------- KPDS(16) -  TIME RANGE FLAG
C
      ISS      = IS + IGRIBL + 13
      DO 400 I = 0, 7
          KPDS(I+9)  = MOVA2I(MSGA(I+ISS))
  400 CONTINUE
C  ----------- KPDS(17) -  NUMBER INCLUDED IN AVERAGE
      ISS       = IS + IGRIBL + 21
      KPDS(17)  = 0
      DO 500 I = 0, 1
          KPDS(17) = KPDS(17) * 256 + MOVA2I(MSGA(I+ISS))
  500 CONTINUE
C  -----------SKIP OVER SOURCE BYTE 28
C  ----------- TEST FOR NEW GRID
      IF (IAND(KPDS(4),128).NE.0) THEN
          IF (IAND(KPDS(4),64).NE.0) THEN
              IF (KPDS(3).NE.255) THEN
                  IF (KPDS(1).EQ.7) THEN
                      IF (KPDS(3).GE.21.AND.KPDS(3).LE.26)THEN
                      ELSE IF (KPDS(3).EQ.50) THEN
                      ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
                      ELSE IF (KPDS(3).EQ.70) THEN
                      ELSE IF (KPDS(3).GE.85.AND.KPDS(3).LE.86) THEN
                      ELSE IF (KPDS(3).GE.100.AND.KPDS(3).LE.103) THEN
                      ELSE IF (KPDS(3).GE.201.AND.KPDS(3).LE.214) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                    ' NMC'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.98) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.16) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                            ' ECMWF'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.74) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
                      ELSE IF (KPDS(3).GE.21.AND.KPDS(3).LE.26)THEN
                      ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
                      ELSE IF (KPDS(3).EQ.70) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                            ' U.K. MET OFFICE, BRACKNELL'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  ELSE IF (KPDS(1).EQ.58) THEN
                      IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
                      ELSE
                          PRINT *,' HAVE ENCOUNTERED A NEW GRID FOR',
     *                            ' FNOC,'
                          PRINT *,' PLEASE NOTIFY AUTOMATION DIVISION'
                          PRINT *,' PRODUCTION MANAGEMENT BRANCH'
                          PRINT *,' W/NMC42)'
                      END IF
                  END IF
              END IF
          END IF
      END IF
      RETURN
      END

C> Extract information on unlisted grid to allow conversion to office note 84 format.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-01-20
C> - Bill Cavanaugh 1989-03-16 Added mercator & lambert conformal processing.
C> - Bill Cavanaugh 1989-07-12 Corrected change entered 89-03-16 reordering
C> processing for lambert conformal and mercator grids.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C>
C> @param[in] MSGA Array containing grib message.
C> @param[inout] KPTR Array containing storage for following parameters.
C> - 1): Unused.
C> - 2): Unused.
C> - 3): Length of pds.
C> - 4): Length of gds.
C> - 5): Length of bms.
C> - 6): Length of bds.
C> - 7): Value of current byte.
C> - 8): Unused.
C> - 9): Grib start byte nr.
C> - 0): Grib/grid element count.
C> @param[in] KPDS Array containing pds elements.
C> - 1): Id of center.
C> - 2): Model identification.
C> - 3): Grid identification.
C> - 4): Gds/bms flag.
C> - 5): Indicator of parameter.
C> - 6): Type of level.
C> - 7): Height/pressure , etc of level.
C> - 8): Year of century.
C> - 9): Month of year.
C> - 10: Day of month.
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version nr of grib specification.
C> @param[out] KGDS Array containing gds elements..
C> - 1): Data representation type.
C> - Latitude/Longitude grids
C>  - 2): N(i) nr points on latitude circle.
C>  - 3): N(j) nr points on longitude meridian.
C>  - 4): La(1) latitude of origin.
C>  - 5): Lo(1) longitude of origin.
C>  - 6): Resolution flag.
C>  - 7): La(2) latitude of extreme point.
C>  - 8): Lo(2) longitude of extreme point.
C>  - 9): Di longitudinal direction of increment.
C>  - 10: Dj latitudinal direction of increment.
C>  - 11: Scanning mode flag.
C> - Polar stereographic grids.
C>  - 2): N(i) nr points along lat circle.
C>  - 3): N(j) nr points along lon circle.
C>  - 4): La(1) latitude of origin.
C>  - 5): Lo(1) longitude of origin.
C>  - 6): Reserved.
C>  - 7): Lov grid orientation.
C>  - 8): Dx - x direction increment.
C>  - 9): Dy - y direction increment.
C>  - 10: Projection center flag.
C>  - 11: Scanning mode.
C> - Spherical harmonic coefficients.
C>  - 2): J pentagonal resolution parameter.
C>  - 3): K pentagonal resolution parameter.
C>  - 4): M pentagonal resolution parameter.
C>  - 5): Representation type.
C>  - 6): Coefficient storage mode.
C> - Mercator grids.
C>  - 2): N(i) nr points on latitude circle.
C>  - 3): N(j) nr points on longitude meridian.
C>  - 4): La(1) latitude of origin.
C>  - 5): Lo(1) longitude of origin.
C>  - 6): Resolution flag.
C>  - 7): La(2) latitude of last grid point.
C>  - 8): Lo(2) longitude of last grid point.
C>  - 9): Longit dir increment.
C>  - 10: Latit dir increment.
C>  - 11: Scanning mode flag.
C>  - 12: Latitude intersection.
C> - Lambert conformal grids.
C>  - 2): Nx nr points along x-axis.
C>  - 3): Ny nr points along y-axis.
C>  - 4): La1 lat of origin (lower left).
C>  - 5): Lo1 lon of origin (lower left).
C>  - 6): Reserved.
C>  - 7): Lov - orientation of grid.
C>  - 8): Dx - x-dir increment.
C>  - 9): Dy - y-dir increment.
C>  - 10: Projection center flag.
C>  - 11: Scanning mode flag.
C>  - 12: Latin 1 - first lat from pole of secant cone inter.
C>  - 13: Latin 2 - second lat from pole of secant cone inter.
C> @param[out] KRET Error return.
C>
C> @note KRET
C> - = 0
C> - = 4   - DATA REPRESENTATION TYPE NOT CURRENTLY ACCEPTABLE
C>
C> @author Bill Cavanaugh @date 1988-01-20

      SUBROUTINE AI083(MSGA,KPTR,KPDS,KGDS,KRET)
C  ************************************************************
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C
C                       ARRAY GDS ELEMENTS
      INTEGER       KGDS(*)
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(*)
C                       ARRAY OF PDS ELEMENTS
      INTEGER       KPDS(*)
C
      INTEGER       KRET
C
C     DATA  MSK80 /Z00000080/
C
      DATA  MSK80 /128/
C  ********************************************************
C      IF FLAG IN PDS INDICATE THAT THERE IS NO GDS ,
C         RETURN IMMEDIATELY
C ************************************************************
      IF (IAND(KPDS(4),MSK80).EQ.0) GO TO 900
C  ------------------- BYTE 1-3   COUNT
      IS       = KPTR(9)
      IF (KPDS(18).EQ.0) THEN
        IGRIBL = 4
      ELSE
        IGRIBL = 8
      ENDIF
      ISS      = IS + KPTR(3) + IGRIBL
C  ------------------- BYTE 4     NUMBER OF UNUSED BITS AT END OF SEC
C  ------------------- BYTE 5     RESERVED
C  ------------------- BYTE 6     DATA REPRESENTATION TYPE
      KGDS(1) = MOVA2I(MSGA(ISS+5))
C  ------------------- DIVERT TO PROCESS CORRECT TYPE
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
      ELSE IF (KGDS(1).EQ.50) THEN
          GO TO 3000
      ELSE
C                      MARK AS GDS/ UNKNOWN DATA REPRESENTATION TYPE
          KRET     = 4
          GO TO 900
      END IF
C
C  ------------------- LATITUDE/LONGITUDE GRIDS
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG LATITUDE CIRCLE
 1000 KGDS(2)   = 0
      DO 1005 I = 0, 1
          KGDS(2) = KGDS(2) * 256 + MOVA2I(MSGA(I+ISS+6))
 1005 CONTINUE
C  ------------------- BYTE 9-10    NR OF POINTS ALONG LONG MERIDIAN
      KGDS(3)   = 0
      DO 1010 I = 0, 1
          KGDS(3) = KGDS(3) * 256 + MOVA2I(MSGA(I+ISS+8))
 1010 CONTINUE
C  ------------------- BYTE 11-13   LATITUE OF ORIGIN
      KGDS(4)   = 0
      DO 1020 I = 0, 2
          KGDS(4) = KGDS(4) * 256 + MOVA2I(MSGA(I+ISS+10))
 1020 CONTINUE
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  IAND(KGDS(4),8388607) * (-1)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN
      KGDS(5)   = 0
      DO 1030 I = 0, 2
          KGDS(5) = KGDS(5) * 256 + MOVA2I(MSGA(I+ISS+13))
 1030 CONTINUE
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  =  - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESOLUTION FLAG
      KGDS(6) = MOVA2I(MSGA(ISS+16))
C  ------------------- BYTE 18-20   LATITUDE OF LAST GRID POINT
      KGDS(7)   = 0
      DO 1040 I = 0, 2
          KGDS(7) = KGDS(7) * 256 + MOVA2I(MSGA(I+ISS+17))
 1040 CONTINUE
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  =  - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   LONGITUDE OF LAST GRID POINT
      KGDS(8)   = 0
      DO 1050 I = 0, 2
          KGDS(8) = KGDS(8) * 256 + MOVA2I(MSGA(I+ISS+20))
 1050 CONTINUE
      IF (IAND(KGDS(8),8388608).NE.0) THEN
          KGDS(8)  =  - IAND(KGDS(8),8388607)
      END IF
C  ------------------- BYTE 24-25   LATITUDINAL DIR INCREMENT
      KGDS(9)   = 0
      DO 1060 I = 0, 1
          KGDS(9) = KGDS(9) * 256 + MOVA2I(MSGA(I+ISS+23))
 1060 CONTINUE
C  ------------------- BYTE 26-27   IF REGULAR LAT/LON GRID
C                                       HAVE LONGIT DIR INCREMENT
C                                   ELSE IF GAUSSIAN GRID
C                                       HAVE NR OF LAT CIRCLES
C                                       BETWEEN POLE AND EQUATOR
      KGDS(10)   = 0
      DO 1070 I = 0, 1
          KGDS(10) = KGDS(10) * 256 + MOVA2I(MSGA(I+ISS+25))
 1070 CONTINUE
C  ------------------- BYTE 28      SCANNING MODE FLAGS
      KGDS(11) = MOVA2I(MSGA(ISS+27))
C  ------------------- BYTE 29-32   RESERVED
C  -------------------
      GO TO 900
C  -------------------
C            ' POLAR STEREO PROCESSING '
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG X=AXIS
 2000 KGDS(2)   = 0
      DO 2005 I = 0, 1
          KGDS(2) = KGDS(2) * 256 + MOVA2I(MSGA(I+ISS+6))
 2005 CONTINUE
C  ------------------- BYTE 9-10    NR OF POINTS ALONG Y-AXIS
      KGDS(3)   = 0
      DO 2010 I = 0, 1
          KGDS(3) = KGDS(3) * 256 + MOVA2I(MSGA(I+ISS+8))
 2010 CONTINUE
C  ------------------- BYTE 11-13   LATITUDE OF ORIGIN
      KGDS(4)   = 0
      DO 2020 I = 0, 2
          KGDS(4) = KGDS(4) * 256 + MOVA2I(MSGA(I+ISS+10))
 2020 CONTINUE
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  - IAND(KGDS(4),8388607)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN
      KGDS(5)   = 0
      DO 2030 I = 0, 2
          KGDS(5) = KGDS(5) * 256 + MOVA2I(MSGA(I+ISS+13))
 2030 CONTINUE
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  =   - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESERVED
      KGDS(6) = MOVA2I(MSGA(ISS+16))
C  ------------------- BYTE 18-20   LOV ORIENTATION OF THE GRID
      KGDS(7)   = 0
      DO 2040 I = 0, 2
          KGDS(7) = KGDS(7) * 256 + MOVA2I(MSGA(I+ISS+17))
 2040 CONTINUE
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  =  - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   DX - THE X DIRECTION INCREMENT
      KGDS(8)   = 0
      DO 2050 I = 0, 2
          KGDS(8) = KGDS(8) * 256 + MOVA2I(MSGA(I+ISS+20))
 2050 CONTINUE
      IF (IAND(KGDS(8),8388608).NE.0) THEN
          KGDS(8)  =  - IAND(KGDS(8),8388607)
      END IF
C  ------------------- BYTE 24-26   DY - THE Y DIRECTION INCREMENT
      KGDS(9)   = 0
      DO 2060 I = 0, 2
          KGDS(9) = KGDS(9) * 256 + MOVA2I(MSGA(I+ISS+23))
 2060 CONTINUE
      IF (IAND(KGDS(9),8388608).NE.0) THEN
          KGDS(9)  =  - IAND(KGDS(9),8388607)
      END IF
C  ------------------- BYTE 27      PROJECTION CENTER FLAG
      KGDS(10) = MOVA2I(MSGA(ISS+26))
C  ------------------- BYTE 28      SCANNING MODE
      KGDS(11) = MOVA2I(MSGA(ISS+27))
C  ------------------- BYTE 29-32   RESERVED
C  -------------------
      GO TO 900
C
C  ------------------- GRID DESCRIPTION FOR SPHERICAL HARMONIC COEFF.
C
C  ------------------- BYTE 7-8     J PENTAGONAL RESOLUTION PARAMETER
 3000 KGDS(2) = 0
      DO 3010 I = 0, 1
          KGDS(2) = KGDS(2) * 256 + MOVA2I(MSGA(I+ISS+6))
 3010 CONTINUE
C  ------------------- BYTE 9-10    K PENTAGONAL RESOLUTION PARAMETER
      KGDS(3) = 0
      DO 3020 I = 0, 1
          KGDS(3) = KGDS(3) * 256 + MOVA2I(MSGA(I+ISS+8))
 3020 CONTINUE
C  ------------------- BYTE 11-12   M PENTAGONAL RESOLUTION PARAMETER
      KGDS(4) = 0
      DO 3030 I = 0, 1
          KGDS(4) = KGDS(4) * 256 + MOVA2I(MSGA(I+ISS+10))
 3030 CONTINUE
C  ------------------- BYTE 13 REPRESENTATION TYPE
      KGDS(5) = MOVA2I(MSGA(ISS+12))
C  ------------------- BYTE 14 COEFFICIENT STORAGE MODE
      KGDS(6) = MOVA2I(MSGA(ISS+13))
C  -------------------        EMPTY FIELDS - BYTES 15 - 32
      KRET   = 0
      GO TO 900
C  ------------------- PROCESS MERCATOR GRIDS
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG LATITUDE CIRCLE
 4000 KGDS(2)   = 0
      DO 4005 I = 0, 1
          KGDS(2) = KGDS(2) * 256 + MOVA2I(MSGA(I+ISS+6))
 4005 CONTINUE
C  ------------------- BYTE 9-10    NR OF POINTS ALONG LONG MERIDIAN
      KGDS(3)   = 0
      DO 4010 I = 0, 1
          KGDS(3) = KGDS(3) * 256 + MOVA2I(MSGA(I+ISS+8))
 4010 CONTINUE
C  ------------------- BYTE 11-13   LATITUE OF ORIGIN
      KGDS(4)   = 0
      DO 4020 I = 0, 2
          KGDS(4) = KGDS(4) * 256 + MOVA2I(MSGA(I+ISS+10))
 4020 CONTINUE
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  - IAND(KGDS(4),8388607)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN
      KGDS(5)   = 0
      DO 4030 I = 0, 2
          KGDS(5) = KGDS(5) * 256 + MOVA2I(MSGA(I+ISS+13))
 4030 CONTINUE
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  =  - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESOLUTION FLAG
      KGDS(6) = MOVA2I(MSGA(ISS+16))
C  ------------------- BYTE 18-20   LATITUDE OF EXTREME POINT
      KGDS(7)   = 0
      DO 4040 I = 0, 2
          KGDS(7) = KGDS(7) * 256 + MOVA2I(MSGA(I+ISS+17))
 4040 CONTINUE
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  =  - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   LONGITUDE OF EXTREME POINT
      KGDS(8)   = 0
      DO 4050 I = 0, 2
          KGDS(8) = KGDS(8) * 256 + MOVA2I(MSGA(I+ISS+20))
 4050 CONTINUE
      IF (IAND(KGDS(8),8388608).NE.0) THEN
          KGDS(8)  =  - IAND(KGDS(8),8388607)
      END IF
C  ------------------- BYTE 24-25   LONGITUDE DIR INCREMENT
      KGDS(9)   = 0
      DO 4070 I = 0, 1
          KGDS(9) = KGDS(9) * 256 + MOVA2I(MSGA(I+ISS+23))
 4070 CONTINUE
      IF (IAND(KGDS(9),8388608).NE.0) THEN
          KGDS(9)  =  - IAND(KGDS(9),32768)
      END IF
C  ------------------- BYTE 26-27   LATIT DIR INCREMENT
      KGDS(10)   = 0
      DO 4080 I = 0, 1
          KGDS(10) = KGDS(10) * 256 + MOVA2I(MSGA(I+ISS+25))
 4080 CONTINUE
      IF (IAND(KGDS(10),8388608).NE.0) THEN
          KGDS(10)  =  - IAND(KGDS(10),32768)
      END IF
C  ------------------- BYTE 28      SCANNING MODE FLAGS
      KGDS(11) = MOVA2I(MSGA(ISS+27))
C  ------------------- BYTE 29-31   INTERSECTION LATITUDE
      KGDS(12)  = 0
      DO 4060 I = 0, 2
          KGDS(12)= KGDS(12) * 256 + MOVA2I(MSGA(I+ISS+28))
 4060 CONTINUE
C  ------------------- BYTE 32   RESERVED
C  -------------------
      GO TO 900
C  ------------------- PROCESS LAMBERT CONFORMAL
C
C  ------------------- BYTE 7-8     NR OF POINTS ALONG X-AXIS
 5000 KGDS(2)   = 0
      DO 5005 I = 0, 1
          KGDS(2) = KGDS(2) * 256 + MOVA2I(MSGA(I+ISS+6))
 5005 CONTINUE
C  ------------------- BYTE 9-10    NR OF POINTS ALONG Y-AXIS
      KGDS(3)   = 0
      DO 5010 I = 0, 1
          KGDS(3) = KGDS(3) * 256 + MOVA2I(MSGA(I+ISS+8))
 5010 CONTINUE
C  ------------------- BYTE 11-13   LATITUDE OF ORIGIN
      KGDS(4)   = 0
      DO 5020 I = 0, 2
          KGDS(4) = KGDS(4) * 256 + MOVA2I(MSGA(I+ISS+10))
 5020 CONTINUE
      IF (IAND(KGDS(4),8388608).NE.0) THEN
          KGDS(4)  =  - IAND(KGDS(4),8388607)
      END IF
C  ------------------- BYTE 14-16   LONGITUDE OF ORIGIN (LOWER LEFT)
      KGDS(5)   = 0
      DO 5030 I = 0, 2
          KGDS(5) = KGDS(5) * 256 + MOVA2I(MSGA(I+ISS+13))
 5030 CONTINUE
      IF (IAND(KGDS(5),8388608).NE.0) THEN
          KGDS(5)  = - IAND(KGDS(5),8388607)
      END IF
C  ------------------- BYTE 17      RESERVED
C     KGDS(6) =
C  ------------------- BYTE 18-20   LOV -ORIENTATION OF GRID
      KGDS(7)   = 0
      DO 5040 I = 0, 2
          KGDS(7) = KGDS(7) * 256 + MOVA2I(MSGA(I+ISS+17))
 5040 CONTINUE
      IF (IAND(KGDS(7),8388608).NE.0) THEN
          KGDS(7)  = - IAND(KGDS(7),8388607)
      END IF
C  ------------------- BYTE 21-23   DX - X-DIR INCREMENT
      KGDS(8)   = 0
      DO 5060 I = 0, 2
          KGDS(8) = KGDS(8) * 256 + MOVA2I(MSGA(I+ISS+20))
 5060 CONTINUE
C  ------------------- BYTE 24-26   DY - Y-DIR INCREMENT
      KGDS(9)   = 0
      DO 5070 I = 0, 2
          KGDS(9) = KGDS(9) * 256 + MOVA2I(MSGA(I+ISS+23))
 5070 CONTINUE
C  ------------------- BYTE 27       PROJECTION CENTER FLAG
      KGDS(10) = MOVA2I(MSGA(ISS+26))
C  ------------------- BYTE 28      SCANNING MODE
      KGDS(11) = MOVA2I(MSGA(ISS+27))
C  ------------------- BYTE 29-31   LATIN1 - 1ST LAT FROM POLE
      KGDS(12)  = 0
      DO 5050 I = 0, 2
          KGDS(12)= KGDS(12)* 256 + MOVA2I(MSGA(I+ISS+28))
 5050 CONTINUE
      IF (IAND(KGDS(12),8388608).NE.0) THEN
          KGDS(12)  =  - IAND(KGDS(12),8388607)
      END IF
C  ------------------- BYTE 32-34   LATIN2 - 2ND LAT FROM POLE
      KGDS(13)  = 0
      DO 5055 I = 0, 2
          KGDS(13)= KGDS(13)* 256 + MOVA2I(MSGA(I+ISS+31))
 5055 CONTINUE
      IF (IAND(KGDS(13),8388608).NE.0) THEN
          KGDS(13)  =  - IAND(KGDS(13),8388607)
      END IF
C  -------------------
  900 CONTINUE
      RETURN
      END

C> If bit map sec is available in grib message,extract
C> for program use, otherwise generate an appropriate bit map.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-01-20
C> - Bill Cavanaugh 1989-02-24 Increment of position in bit map when bit map was included was handled improperly. corrected this data.
C> - Bill Cavanaugh 1989-07-12 Altered method of calculating nr of bits in a bit map contained in grib message.
C> - Bill Cavanaugh 1990-05-07 Brings all u.s. grids to revised values as of dec 89.
C> - William Bostelman 1990-07-15 Modiifed to test the grib bds byte size to determine what ecmwf grid array size is to be specified.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C>
C> @param[in] MSGA BUFR message.
C> @param[inout] KPTR Array containing storage for following parameters.
C> - 1: Unused.
C> - 2: Unused.
C> - 3: Length of pds.
C> - 4: Length of gds.
C> - 5: Length of bms.
C> - 6: Length of bds.
C> - 7: Value of current byte.
C> - 8: Unused.
C> - 9: Grib start byte nr.
C> - 10: Grib/grid element count.
C> @param[in] KPDS ARRAY CONTAINING PDS ELEMENTS.
C> - 1: Id of center.
C> - 2: Model identification.
C> - 3: Grid identification.
C> - 4: Gds/bms flag.
C> - 5: Indicator of parameter.
C> - 6: Type of level.
C> - 7: Height/pressure , etc of level.
C> - 8: Year of century.
C> - 9: Month of year.
C> - 10: Day of month.
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version nr of grib specification.
C> @param[out] kgds array containing gds elements.
C> - 1: data representation type
C> - Latitude/longitude grids
C>  - 2: n(i) nr points on latitude circle
C>  - 3: n(j) nr points on longitude meridian
C>  - 4: la(1) latitude of origin
C>  - 5: lo(1) longitude of origin
C>  - 6: resolution flag
C>  - 7: la(2) latitude of extreme point
C>  - 8: lo(2) longitude of extreme point
C>  - 9: di longitudinal direction of increment
C>  - 10: dj latitundinal direction of increment
C>  - 11: scanning mode flag
C> - Polar stereographic grids
C>  - 2: n(i) nr points along lat circle
C>  - 3: n(j) nr points along lon circle
C>  - 4: la(1) latitude of origin
C>  - 5: lo(1) longitude of origin
C>  - 6: reserved
C>  - 7: lov grid orientation
C>  - 8: dx - x direction increment
C>  - 9: dy - y direction increment
C>  - 10: projection center flag
C>  - 11: scanning mode
C> - Spherical harmonic coefficients
C>  - 2: j pentagonal resolution parameter
C>  - 3: k pentagonal resolution parameter
C>  - 4: m pentagonal resolution parameter
C>  - 5: representation type
C>  - 6: coefficient storage mode
C> - Mercator grids
C>  - 2: n(i) nr points on latitude circle
C>  - 3: n(j) nr points on longitude meridian
C>  - 4: la(1) latitude of origin
C>  - 5: lo(1) longitude of origin
C>  - 6: resolution flag
C>  - 7: la(2) latitude of last grid point
C>  - 8: lo(2) longitude of last grid point
C>  - 9: longit dir increment
C>  - 10: latit dir increment
C>  - 11: scanning mode flag
C>  - 12: latitude intersection
C> - Lambert conformal grids
C>  - 2: nx nr points along x-axis
C>  - 3: ny nr points along y-axis
C>  - 4: la1 lat of origin (lower left)
C>  - 5: lo1 lon of origin (lower left)
C>  - 6: reserved
C>  - 7: lov - orientation of grid
C>  - 8: dx - x-dir increment
C>  - 9: dy - y-dir increment
C>  - 10: projection center flag
C>  - 11: scanning mode flag
C>  - 12: latin 1 - first lat from pole of secant cone inter
C>  - 13: latin 2 - second lat from pole of secant cone inter
C> @param[out] KBMS Bitmap describing location of output elements..
C> @param[out] KRET Error return.
C>
C> @note KRET
C> - = 0 - No error.
C> - = 5 - Grid not avail for center indicated.
C> - = 10 - Incorrect center indicator.
C>
C> @author Bill Cavanaugh @date 1988-01-20
      SUBROUTINE AI084(MSGA,KPTR,KPDS,KGDS,KBMS,KRET)
C
C                       INCOMING MESSAGE HOLDER
      CHARACTER*1   MSGA(*)
C
C                       BIT MAP
      LOGICAL       KBMS(*)
C
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPTR(10)
C                       ARRAY OF POINTERS AND COUNTERS
      INTEGER       KPDS(20)
      INTEGER       KGDS(13)
C
      INTEGER       KRET
      INTEGER       MASK(8)
C  ----------------------GRID 21 AND GRID 22 ARE THE SAME
      LOGICAL       GRD21( 1369)
C  ----------------------GRID 23 AND GRID 24 ARE THE SAME
      LOGICAL       GRD23( 1369)
      LOGICAL       GRD25( 1368)
      LOGICAL       GRD26( 1368)
C  ----------------------GRID 27 AND GRID 28 ARE THE SAME
C  ----------------------GRID 29 AND GRID 30 ARE THE SAME
C  ----------------------GRID 33 AND GRID 34 ARE THE SAME
      LOGICAL       GRD50(1188)
C  -----------------------GRID 61 AND GRID 62 ARE THE SAME
      LOGICAL       GRD61( 4186)
C  -----------------------GRID 63 AND GRID 64 ARE THE SAME
      LOGICAL       GRD63( 4186)
C
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
C     DATA  MSK40 /Z00000040/
      DATA  MSK40 /64/
C
      IS      = KPTR(9)
      IF (KPDS(18).EQ.0) THEN
        IGRIBL = 4
      ELSE
        IGRIBL = 8
      ENDIF
      ISS     = IS + KPTR(3) + KPTR(4) + IGRIBL
C  **********************************************************
C     IF THE FLAG IN PDS INDICATES THAT THERE IS NO BMS,
C        SET BIT MAP WITH ALL BITS ON
C      ELSE
C        RECOVER BIT MAP
C      THEN RETURN
C  **********************************************************
C  ---------------- NON-STANDARD GRID
      IF (KPDS(3).EQ.255) THEN
          J      = KGDS(2) * KGDS(3)
          KPTR(10) = J
          DO 600 I = 1, J
              KBMS(I) = .TRUE.
  600     CONTINUE
      END IF
      IF (IAND(KPDS(4),MSK40).EQ.0)THEN
C         PRINT *,' NO BIT MAP',MSK40,KPDS(4)
          GO TO 400
      ELSE
          PRINT *,' HAVE A BIT MAP'
      END IF
C  ---------------- FLAG INDICATING PRESENCE OF BIT MAP IS ON
      IF (KGDS(1).EQ.50) THEN
          PRINT *,'  W3AI08/AI084  WARNING - BIT MAP MAY NOT BE',
     *           '  ASSOCIATED WITH SPHERICAL COEFFICIENTS'
          RETURN
      ENDIF
C                        GET NUMBER OF UNUSED BITS
      IUBITS   = MOVA2I(MSGA(ISS+3))
C                        SEE IF BIT MAP IS CONTAINED
      KFLAG  = 0
      DO 150 I = 0, 1
          KFLAG  = KFLAG * 256 + MOVA2I(MSGA(I+ISS+4))
  150 CONTINUE
      PRINT *,'KFLAG=',KFLAG
C  ----------------- IF KFLAG = 0 PICK UP NEW BIT MAP
C                        ELSE
C  ------------------      USE PREDEFINED BIT MAP
      MAXBYT  = KPTR(5) - 6
      IF (KFLAG.EQ.0) THEN
C  ------------------ UTILIZE BIT MAP FROM MESSAGE
          II      = 1
          DO 300 I = 1, MAXBYT
              KCNT   = MOVA2I(MSGA(I+ISS+6))
              DO 200 K = 1, 8
                  IF (IAND(KCNT,MASK(K)).NE.0) THEN
                      KBMS(II) = .TRUE.
                  ELSE
                      KBMS(II) = .FALSE.
                  END IF
                  II       = II + 1
  200         CONTINUE
  300     CONTINUE
          KPTR(10)  = 8 * (KPTR(5) - 6) - IUBITS
          GO TO 900
      ELSE
          PRINT *,'KFLAG SAYS USE STD BIT MAP',KFLAG
      END IF
C  ---------------------- PREDEFINED BIT MAP IS INDICATED
C                         IF GRID NUMBER DOES NOT MATCH AN
C                         EXISTING GRID, SET KRET TO 5 AND
C  ---------------------- RETURN.
  400 CONTINUE
      KRET = 0
C  ---------------------- ECMWF MAP GRIDS
      IF (KPDS(1).EQ.98) THEN
          IF (KPDS(3).GE.1.AND.KPDS(3).LE.12) THEN
              J   = 1073
C*** TEST FOR FULL HEMISPHERIC GRID ****
              IF (KPTR(6) .GT. 2158) J= 1369
C*** ***       ****       ***       ***
              KPTR(10)  = J
              CALL AI087(*900,J,KPDS,KGDS,KRET)
              DO 1000 I = 1, J
                  KBMS(I) = .TRUE.
 1000         CONTINUE
          ELSE IF (KPDS(3).GE.13.AND.KPDS(3).LE.16) THEN
              J   = 361
              KPTR(10)  = J
              CALL AI087(*900,J,KPDS,KGDS,KRET)
              DO 1013 I = 1, J
                  KBMS(I) = .TRUE.
 1013         CONTINUE
          ELSE
              KRET  = 5
              RETURN
          END IF
C  ---------------------- U.K. MET OFFICE BRACKNELL
      ELSE IF (KPDS(1).EQ.74) THEN
            IF (KPDS(3).EQ.21.OR.KPDS(3).EQ.22) THEN
C                   ----- INT'L GRIDS 21, 22 - MAP SIZE 1369
                J   = 1369
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3021 I = 1, 1369
                    KBMS(I) = GRD21(I)
 3021           CONTINUE
            ELSE IF (KPDS(3).EQ.23.OR.KPDS(3).EQ.24) THEN
C                   ----- INT'L GRIDS 23, 24 - MAP SIZE 1369
                J   = 1369
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3023 I = 1, 1369
                    KBMS(I) = GRD23(I)
 3023           CONTINUE
            ELSE IF (KPDS(3).EQ.25) THEN
C                   ----- INT'L GRID 25 - MAP SIZE 1368
                J   = 1368
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3025 I = 1, 1368
                      KBMS(I) = GRD25(I)
 3025           CONTINUE
            ELSE IF (KPDS(3).EQ.26) THEN
C                   ----- INT'L GRID  26 - MAP SIZE 1368
                J   = 1368
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3026 I = 1, 1368
                    KBMS(I) = GRD26(I)
 3026           CONTINUE
            ELSE IF (KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
C                   ----- INT'L GRIDS 61, 62 - MAP SIZE 4186
                J     = 4186
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3061 I = 1, 4186
                    KBMS(I) = GRD61(I)
 3061           CONTINUE
            ELSE IF (KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
C                   ----- INT'L GRIDS 63, 64 - MAP SIZE 4186
                J     = 4186
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3063 I = 1, 4186
                    KBMS(I) = GRD63(I)
 3063           CONTINUE
            ELSE IF (KPDS(3).EQ.70) THEN
C                   ----- U.S. GRID 70 - MAP SIZE 16380
                J     = 16380
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 3070 I = 1, J
                    KBMS(I)  = .TRUE.
 3070           CONTINUE
            ELSE
               KRET  = 5
               RETURN
            END IF
C  ---------------------- FNOC NAVY
        ELSE IF (KPDS(1).EQ.58) THEN
          PRINT *,' NO STANDARD FNOC GRID AT THIS TIME'
          RETURN
C  ---------------------- U.S. GRIDS
        ELSE IF (KPDS(1).EQ.7) THEN
            IF (KPDS(3).EQ.5) THEN
C                   ----- U.S. GRID 5 - MAP SIZE 3021
                J   = 3021
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2005 I = 1, J
                    KBMS(I)  = .TRUE.
 2005           CONTINUE
            ELSE IF (KPDS(3).EQ.6) THEN
C                   ----- U.S. GRID 6 - MAP SIZE 2385
                J   = 2385
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2006 I = 1, J
                    KBMS(I)  = .TRUE.
 2006           CONTINUE
            ELSE IF (KPDS(3).EQ.21.OR.KPDS(3).EQ.22) THEN
C                   ----- U.S. GRIDS 21, 22 - MAP SIZE 1369
                J   = 1369
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2021 I = 1, 1369
                    KBMS(I) = GRD21(I)
 2021           CONTINUE
            ELSE IF (KPDS(3).EQ.23.OR.KPDS(3).EQ.24) THEN
C                   ----- U.S GRIDS 23, 24 - MAP SIZE 1369
                J   = 1369
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2023 I = 1, 1369
                    KBMS(I) = GRD23(I)
 2023           CONTINUE
            ELSE IF (KPDS(3).EQ.25) THEN
C                   ----- U.S. GRID 25 - MAP SIZE 1368
                J   = 1368
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2025 I = 1, 1368
                      KBMS(I) = GRD25(I)
 2025           CONTINUE
            ELSE IF (KPDS(3).EQ.26) THEN
C                   ----- U.S.GRID 26 - MAP SIZE 1368
                J   = 1368
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2026 I = 1, 1368
                    KBMS(I) = GRD26(I)
 2026           CONTINUE
            ELSE IF (KPDS(3).EQ.27.OR.KPDS(3).EQ.28) THEN
C                   ----- U.S. GRIDS 27, 28 - MAP SIZE 4225
                J     = 4225
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2027 I = 1, J
                    KBMS(I)  = .TRUE.
 2027           CONTINUE
            ELSE IF (KPDS(3).EQ.29.OR.KPDS(3).EQ.30)THEN
C                   ----- U.S. GRIDS 29,30 - MAP SIZE 5365
                J     = 5365
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2029 I = 1, J
                    KBMS(I)  = .TRUE.
 2029           CONTINUE
            ELSE IF (KPDS(3).EQ.33.OR.KPDS(3).EQ.34) THEN
C                   ----- U.S GRID 33, 34 - MAP SIZE 8326 (181 X 46)
                J     = 8326
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2033 I = 1, J
                    KBMS(I)  = .TRUE.
 2033           CONTINUE
            ELSE IF (KPDS(3).EQ.50) THEN
C                   ----- U.S. GRID 50 - MAP SIZE 964
                J     = 1188
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2050 I = 1, 1188
                    KBMS(I) = GRD50(I)
 2050           CONTINUE
            ELSE IF (KPDS(3).EQ.61.OR.KPDS(3).EQ.62) THEN
C                   ----- U.S. GRIDS 61, 62 - MAP SIZE 4186
                J     = 4186
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2061 I = 1, 4186
                    KBMS(I) = GRD61(I)
 2061           CONTINUE
            ELSE IF (KPDS(3).EQ.63.OR.KPDS(3).EQ.64) THEN
C                   ----- U.S. GRIDS 63, 64 - MAP SIZE 4186
                J     = 4186
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2063 I = 1, 4186
                    KBMS(I) = GRD63(I)
 2063           CONTINUE
            ELSE IF (KPDS(3).EQ.70) THEN
C                   ----- U.S. GRID 70 - MAP SIZE 16380
                J     = 16380
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2070 I = 1, J
                    KBMS(I)  = .TRUE.
 2070           CONTINUE
            ELSE IF (KPDS(3).EQ.85.OR.KPDS(3).EQ.86) THEN
C                   ----- U.S. GRIDS 85, 86 - MAP SIZE 32400 (360 X 90)
                J     = 32400
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2085 I = 1, J
                    KBMS(I) = .TRUE.
 2085           CONTINUE
            ELSE IF (KPDS(3).EQ.100) THEN
C                   ----- U.S. GRID 100 - MAP SIZE 6889  (83 X 83)
                J     = 6889
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 1100 I = 1, J
                    KBMS(I)  = .TRUE.
 1100           CONTINUE
            ELSE IF (KPDS(3).EQ.101) THEN
C                   ----- U.S. GRID 101 - MAP SIZE 10283 (113 X 91)
                J     = 10283
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2101 I = 1, J
                    KBMS(I) = .TRUE.
 2101           CONTINUE
            ELSE IF (KPDS(3).EQ.102) THEN
C                   ----- U.S. GRID 102 - MAP SIZE 14375  (115 X 125)
                J = 14375
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2102 I = 1, J
                    KBMS(I) = .TRUE.
 2102           CONTINUE
            ELSE IF (KPDS(3).EQ.103) THEN
C                   ----- U.S. GRID 103 - MAP SIZE 3640  (65 X 56)
                J = 3640
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2103 I = 1, J
                    KBMS(I) = .TRUE.
 2103           CONTINUE
            ELSE IF (KPDS(3).GE.201.AND.KPDS(3).LE.214) THEN
                IF (KPDS(3).EQ.201) J = 4225
                IF (KPDS(3).EQ.202) J = 2795
                IF (KPDS(3).EQ.203) J = 1755
                IF (KPDS(3).EQ.204) J = 5609
                IF (KPDS(3).EQ.205) J = 1755
                IF (KPDS(3).EQ.206) J = 2091
                IF (KPDS(3).EQ.207) J = 1715
                IF (KPDS(3).EQ.208) J = 625
                IF (KPDS(3).EQ.209) J = 8181
                IF (KPDS(3).EQ.210) J = 625
                IF (KPDS(3).EQ.211) J = 2915
                IF (KPDS(3).EQ.212) J = 4225
                IF (KPDS(3).EQ.213) J = 10965
                IF (KPDS(3).EQ.214) J = 6693
                KPTR(10)  = J
                CALL AI087(*900,J,KPDS,KGDS,KRET)
                DO 2201 I = 1, J
                    KBMS(I) = .TRUE.
 2201           CONTINUE
            ELSE
               KRET  = 5
               RETURN
            END IF
        ELSE
          KRET  = 10
          RETURN
      END IF
  900 CONTINUE
      RETURN
      END

C> Extract grib data and place into output arry in proper position.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-01-20
C> - Ralph Jones 1990-09-01 Change's for ansi fortran.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C>
C> @param[in] MSGA Array containing grib message.
C> @param[inout] KPTR Array containing storage for following parameters.
C> - 1: Unused.
C> - 2: Unused.
C> - 3: Length of pds.
C> - 4: Length of gds.
C> - 5: Length of bms.
C> - 6: Length of bds.
C> - 7: Value of current byte.
C> - 8: Unused.
C> - 9: Grib start byte nr.
C> - 10: Grib/grid element count.
C> @param[in] KPDS Array containing pds elements.
C> - 1: Id of center.
C> - 2: Model identification.
C> - 3: Grid identification.
C> - 4: Gds/bms flag.
C> - 5: Indicator of parameter.
C> - 6: Type of level.
C> - 7: Height/pressure , etc of level.
C> - 8: Year of century.
C> - 9: Month of year.
C> - 10: Day of month.
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version nr of grib specification.
C> @param[in] KBMS Bitmap describing location of output elements.
C> @param[out] DATA Real array of gridded elements in grib message.
C> @param[out] KRET Error return.
C>
C> @note Error return.
C> - 3 = Unpacked field is larger than 32768.
C> - 6 = Does not match nr of entries for this grib/grid.
C> - 7 = Number of bits in fill too large.
C>
C> @author Bill Cavanaugh @date 1988-01-20
      SUBROUTINE AI085(MSGA,KPTR,KPDS,KBMS,DATA,KRET)
C  *************************************************************
      CHARACTER*1   MSGA(*)
      CHARACTER*1   KREF(8)
      CHARACTER*1   KK(8)
C
      LOGICAL       KBMS(*)
C
      INTEGER       KPDS(*)
      INTEGER       KPTR(*)
      INTEGER       NRBITS
      INTEGER       KSAVE(105000)
      INTEGER       KSCALE
C
      REAL          DATA(*)
      REAL          REFNCE
      REAL          SCALE
      REAL          REALKK
C
      LOGICAL       IBM370
C
      EQUIVALENCE   (REFNCE,KREF(1),IREF)
      EQUIVALENCE   (KK(1),REALKK,IKK)
C
C     DATA  MSK0F /Z0000000F/
C     DATA  MSK80 /Z00000080/
C     DATA  MSK40 /Z00000040/
C
      DATA  MSK0F /15/
      DATA  MSK80 /128/
      DATA  MSK40 /64/
C
C  *************************************************************
      KRET   = 0
      IS     = KPTR(9)
      ISS    = IS + KPTR(3) + KPTR(4) + KPTR(5) + 4
C                   BYTE 4
      KSPL    = MOVA2I(MSGA(ISS+3))
C     POINT TO BYTE 5 OF BDS
C
C  ------------- GET SCALE FACTOR
C
      KSCALE  = 0
      DO 100 I = 0, 1
          KSCALE   = KSCALE * 256 + MOVA2I(MSGA(I+ISS+4))
  100 CONTINUE
      IF (IAND(KSCALE,32768).NE.0) THEN
          KSCALE = - IAND(KSCALE,32767)
      END IF
      SCALE = 2.0**KSCALE
C
C  ------------ GET REFERENCE VALUE
C
      IREF = 0
      DO 200 I = 0, 3
          KREF(I+1)  = MSGA(I+ISS+6)
  200 CONTINUE
C
C     THE FLOATING POINT NUMBER IN THE REFERENCE VALUE IS AN IBM370
C     32 BIT NUMBER, IF YOUR COMPUTER IS NOT AN IBM370 OR CLONE
C     SET IBM370 TO .FALSE. SO THE NUMBER IS CONVERTED TO A F.P.
C     NUMBER OF YOUR MACHINE TYPE.
C
      IBM370 = .FALSE.
C
      IF (.NOT.IBM370) THEN
        KOFF = 0
C  GET 1 BIT SIGN
        CALL GBYTE(IREF,ISGN,0,1)
C  GET 7 BIT EXPONENT
        CALL GBYTE(IREF,IEXP,1,7)
C  GET 24 BIT FRACTION
        CALL GBYTE(IREF,IFR,8,24)
        IF (IFR.EQ.0.OR.IEXP.EQ.0) THEN
          REFNCE = 0.0
        ELSE
          REFNCE = FLOAT(IFR) * 16.0 ** (IEXP-64-6)
          IF (ISGN.NE.0) REFNCE = - REFNCE
        ENDIF
      ENDIF
C
C  ------------- NUMBER OF BITS SPECIFIED FOR EACH ENTRY
C
      KBITS   = MOVA2I(MSGA(ISS+10))
      KENTRY  = KPTR(10)
C
C  ------------- MAX SIZE CHECK
C
      IF (KENTRY.GT.105000) THEN
          KRET   = 3
          RETURN
      END IF
      IF (KBITS.EQ.0) THEN
C
C  -------------------- HAVE NO BDS ENTRIES, ALL ENTRIES = REFNCE
C
          DO 210 I = 1, KENTRY
              DATA(I) = 0.0
              IF (KBMS(I)) THEN
                  DATA(I) = REFNCE
              END IF
  210     CONTINUE
          GO TO 900
      END IF
C
C  --------------------
C       CYCLE THRU BDS UNTIL HAVE USED ALL (SPECIFIED NUMBER)
C       ENTRIES.
C
C  ------------- UNUSED BITS IN DATA AREA
C
      LESSBT  = IAND(KSPL,MSK0F)
C
C  ------------- NUMBER OF BYTES IN DATA AREA
C
      NRBYTE  = KPTR(6) - 11
C
C  ------------- TOTAL NR OF USABLE BITS
C
      NRBITS  = NRBYTE * 8  - LESSBT
C
C  ------------- TOTAL NR OF ENTRIES
C
      KENTRY  = NRBITS / KBITS
C
C  -------------  MAX SIZE CHECK
C
      IF (KENTRY.GT.105000) THEN
          KRET   = 3
          RETURN
      END IF
C
      IBMS = IAND(KPDS(4),MSK40)
C
C  -------------- CHECK TO SEE IF PROCESSING COEFFICIENTS
C                   IF YES,
C                      GO AND PROCESS AS SUCH
C                    ELSE
C                      CONTINUE PROCESSING
C
      IF (IAND(KSPL,MSK80).EQ.0) THEN
C
C  ------------- SET POINTERS
C
C     XMOVEX MOVES THE DATA TO MAKE SURE IT IS ON A INTEGER WORD
C     BOUNDARY, ON SOME COMPUTERS THIS DOES NOT HAVE TO BE DONE.
C     (IBM PC, VAX)
C
C         CALL XMOVEX(MSGB,MSGA(ISS+11),NRBYTE)
C  ------------- UNPACK ALL FIELDS
          KOFF   = 0
C
C     THE BIT UNPACKER W3AI41 WILL CONSUME MOST OF THE CPU TIME
C     CONVERTING THE GRIB DATA. FOR THE IBM370 WE HAVE AN
C     ASSEMBLER AND FORTRAN VERSION. THE ASSMBLER VERSION WILL
C     RUN TWO TO THREE TIMES FASTER. THE FORTRAN VERSION IS TO
C     MAKE THE CODE MORE PORTABLE. FOR A VAX OR IBM PC WE HAVE
C     ANOTHER VERSION, IT REVERSED THE ORDER OF THE BYTES IN
C     AN INTEGER WORD.  W3AI41 CAN BE REPLACED BY NCAR GBYTES
C     BIT UNPACKER. NCAR HAS A LARGE NUMBER OF VERSIONS OF GBYTES
C     IN FORTRAN AN ASSEMBLER FOR A NUMBER OF DIFFERENT BRANDS OF
C     COMPUTERS. THEY ALSO HAVE A C VERSION.
C
C         CALL W3AI41(MSGB,KSAVE,KBITS,KENTRY,KOFF)
C
C     ALIGN CHARACTER ARRAY MSGA STARTING ADDRESS ON CRAY
C     INTEGER WORD BOUNDARY
C
          LLL  = MOD(ISS+10,8)
          NNN  = 11 - LLL
          KOFF = LLL * 8
          CALL GBYTES(MSGA(ISS+NNN),KSAVE,KOFF,KBITS,0,KENTRY)
C
C  ------------- CORRECTLY PLACE ALL ENTRIES
C
          II    = 1
          KENTRY = KPTR(10)
          DO 500 I = 1, KENTRY
              IF (KBMS(I)) THEN
                  DATA(I) = REFNCE + FLOAT(KSAVE(II)) * SCALE
                  II  = II + 1
              ELSE
                  DATA(I) = 0.0
              END IF
  500     CONTINUE
          GO TO 900
      END IF
C
C  ------------- PROCESS SPHERICAL HARMONIC COEFFICIENTS
C
      IKK = 0
      DO 5500 I =  0, 3
          KK(I+1)    = MSGA(I+ISS+11)
 5500 CONTINUE
C
      IF (.NOT.IBM370) THEN
        KOFF = 0
C  GET 1 BIT SIGN
        CALL GBYTE(IKK,ISGN,0,1)
C  GET 7 BIT EXPONENT
        CALL GBYTE(IKK,IEXP,1,7)
C  GET 24 BIT FRACTION
        CALL GBYTE(IKK,IFR,8,24)
        IF (IFR.EQ.0.OR.IEXP.EQ.0) THEN
          REALKK = 0.0
        ELSE
          REALKK = FLOAT(IFR) * 16.0 ** (IEXP-64-6)
          IF (ISGN.NE.0) REALKK = - REALKK
        ENDIF
      ENDIF
C
      DATA(1) = REALKK
      KOFF    = 0
C     CALL XMOVEX(MSGB,MSGA(ISS+15),NRBYTE)
C  ------------- UNPACK ALL FIELDS
C
C     CALL W3AI41(MSGB,KSAVE,KBITS,KENTRY,KOFF)
C
C     ALIGN CHARACTER ARRAY MSGA STARTING ADDRESS ON CRAY
C     INTEGER WORD BOUNDARY
C
          LLL  = MOD(ISS+14,8)
          NNN  = 15 - LLL
          KOFF = LLL * 8
C
          CALL GBYTES(MSGA(ISS+NNN),KSAVE,KOFF,KBITS,0,KENTRY)
C
C  --------------
      DO 6000 I = 1, KENTRY
          DATA(I+1)  = REFNCE + FLOAT(KSAVE(I)) * SCALE
 6000 CONTINUE
  900 CONTINUE
      RETURN
      END


C> Extract grib data (version 1) and place into proper position in output array.
C>
C> Program history log:
C> - Bill Cavanaugh 1989-11-20
C> - Ralph Jones 1990-09-01 Change's for ansi fortran.
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C>
C> @param[in] MSGA Array containing grib message.
C> @param[inout] KPTR Array containing storage for following parameters.
C> - 1:Unused.
C> - 2:Unused.
C> - 3:Length of pds.
C> - 4:Length of gds.
C> - 5:Length of bms.
C> - 6:Length of bds.
C> - 7:Value of current byte.
C> - 8:Unused.
C> - 9:Grib start byte nr.
C> - 10:Grib/grid element count.
C> @param[in] KPDS Array containing pds elements.  (version 1)
C> - 1: Id of center.
C> - 2: Model identification.
C> - 3: Grid identification.
C> - 4: Gds/bms flag.
C> - 5: Indicator of parameter.
C> - 6: Type of level.
C> - 7: Height/pressure , etc of level.
C> - 8: Year including century.
C> - 9: Month of year.
C> - 10: Day of month.
C> - 11: Hour of day.
C> - 12: Minute of hour.
C> - 13: Indicator of forecast time unit.
C> - 14: Time range 1.
C> - 15: Time range 2.
C> - 16: Time range flag.
C> - 17: Number included in average.
C> - 18: Version nr of grib specification.
C> - 19: Version nr of parameter table.
C> - 20: Total length of grib message (including section 0).
C> @param[in] KBMS Bitmap describing location of output elements.
C> @param[out] DATA Real array of gridded elements in grib message.
C> @param[out] KRET Error return.
C>
C> @note Structure of binary data section (version 1)
C> - 1-3: LENGTH OF SECTION
C> - 4: PACKING FLAGS
C> - 5-6: SCALE FACTOR
C> - 7-10: REFERENCE VALUE
C> - 11: NUMBER OF BIT FOR EACH VALUE
C> - 12s-N: DATA
C>
C> @note Error return:
C> - 3 = Unpacked field is larger than 32768.
C> - 6 = Does not match nr of entries for this grib/grid.
C> - 7 = Number of bits in fill too large.
C>
C> @author Bill Cavanaugh @date 1989-11-20
      SUBROUTINE AI085A(MSGA,KPTR,KPDS,KBMS,DATA,KRET)
C  *************************************************************
      CHARACTER*1   MSGA(*)
      CHARACTER*1   KREF(8)
      CHARACTER*1   KK(8)
C
      LOGICAL       KBMS(*)
C
      INTEGER       KPDS(*)
      INTEGER       KPTR(*)
      INTEGER       NRBITS
      INTEGER       KSAVE(105000)
      INTEGER       KSCALE
C
      REAL          DATA(*)
      REAL          REFNCE
      REAL          SCALE
      REAL          REALKK
C
      LOGICAL       IBM370
C
      EQUIVALENCE   (REFNCE,KREF(1),IREF)
      EQUIVALENCE   (KK(1),REALKK,IKK)
C
C     DATA  MSK0F /Z0000000F/
C     DATA  MSK40 /Z00000040/
C     DATA  MSK80 /Z00000080/
C
      DATA  MSK0F /15/
      DATA  MSK40 /64/
      DATA  MSK80 /128/
C
C  *************************************************************
C
      KRET   = 0
      IS     = KPTR(9)
      IGRIBL = 8
      ISS    = IS + KPTR(3) + KPTR(4) + KPTR(5) + IGRIBL
C                   BYTE 4
      KSPL    = MOVA2I(MSGA(ISS+3))
C
C  ------------- POINT TO BYTE 5 OF BDS
C
C  ------------- GET SCALE FACTOR
C
      KSCALE  = 0
      DO 100 I = 0, 1
          KSCALE   = KSCALE * 256 + MOVA2I(MSGA(I+ISS+4))
  100 CONTINUE
      IF (IAND(KSCALE,32768).NE.0) THEN
          KSCALE = - IAND(KSCALE,32767)
      END IF
      SCALE = 2.0**KSCALE
C
C  -------------------- DECIMAL SCALE EXPONENT
C
      IDEC   = IS + IGRIBL + 26
      JSCALE = 0
      DO 150 I = 0, 1
          JSCALE   = JSCALE * 256 + MOVA2I(MSGA(I+IDEC))
  150 CONTINUE
C                      IF HIGH ORDER BIT IS ON, HAVE NEGATIVE EXPONENT
      IF (IAND(JSCALE,32768).NE.0) THEN
          JSCALE = - IAND(JSCALE,32767)
      END IF
      ASCALE   = 10.0 ** JSCALE
C
C  ------------ GET REFERENCE VALUE
C
      IREF = 0
      DO 200 I = 0, 3
          KREF(I+1)  = MSGA(I+ISS+6)
  200 CONTINUE
C
C     THE FLOATING POINT NUMBER IN THE REFERENCE VALUE IS AN IBM370
C     32 BIT NUMBER, IF YOUR COMPUTER IS NOT AN IBM370 OR CLONE
C     SET IBM370 TO .FALSE. SO THE NUMBER IS CONVERTED TO A F.P.
C     NUMBER OF YOUR MACHINE TYPE.
C
      IBM370 = .FALSE.
C
      IF (.NOT.IBM370) THEN
        KOFF = 0
C  GET 1 BIT SIGN
        CALL GBYTE(IREF,ISGN,0,1)
C  GET 7 BIT EXPONENT
        CALL GBYTE(IREF,IEXP,1,7)
C  GET 24 BIT FRACTION
        CALL GBYTE(IREF,IFR,8,24)
        IF (IFR.EQ.0.OR.IEXP.EQ.0) THEN
          REFNCE = 0.0
        ELSE
          REFNCE = FLOAT(IFR) * 16.0 ** (IEXP-64-6)
          IF (ISGN.NE.0) REFNCE = - REFNCE
        ENDIF
      ENDIF
C
C  ------------- NUMBER OF BITS SPECIFIED FOR EACH ENTRY
C
      KBITS   = MOVA2I(MSGA(ISS+10))
      KENTRY  = KPTR(10)
C
C  ------------- MAX SIZE CHECK
C
      IF (KENTRY.GT.105000) THEN
          KRET   = 3
          RETURN
      END IF
C
      IF (KBITS.EQ.0) THEN
C
C  -------------------- HAVE NO BDS ENTRIES, ALL ENTRIES = REFNCE
C
          DO 210 I = 1, KENTRY
              DATA(I) = 0.0
              IF (KBMS(I)) THEN
                  DATA(I) = REFNCE
              END IF
  210     CONTINUE
          GO TO 900
      END IF
C
C  --------------------
C       CYCLE THRU BDS UNTIL HAVE USED ALL (SPECIFIED NUMBER)
C       ENTRIES.
C
C  ------------- UNUSED BITS IN DATA AREA
C
      LESSBT  = IAND(KSPL,MSK0F)
C
C  ------------- NUMBER OF BYTES IN DATA AREA
C
      NRBYTE  = KPTR(6) - 11
C
C  ------------- TOTAL NR OF USABLE BITS
C
      NRBITS  = NRBYTE * 8  - LESSBT
C
C  ------------- TOTAL NR OF ENTRIES
C
      KENTRY  = NRBITS / KBITS
C
C  ------------- MAX SIZE CHECK
C
      IF (KENTRY.GT.105000) THEN
          KRET   = 3
          RETURN
      END IF
      IBMS = IAND(KPDS(4),MSK40)
C
C  -------------- CHECK TO SEE IF PROCESSING COEFFICIENTS
C                   IF YES,
C                      GO AND PROCESS AS SUCH
C                    ELSE
C                      CONTINUE PROCESSING
      IF (IAND(KSPL,MSK80).EQ.0) THEN
C
C  ------------- SET POINTERS
C
C         REPLACE XMOVEX AND W3AI41 WITH GBYTES
C         CALL XMOVEX(MSGB,MSGA(ISS+11),NRBYTE)
C
C  ------------- UNPACK ALL FIELDS
C
          KOFF   = 0
C         CALL W3AI41(MSGB,KSAVE,KBITS,KENTRY,KOFF)
C
C     THE BIT UNPACKER W3AI41 WILL CONSUME MOST OF THE CPU TIME
C     CONVERTING THE GRIB DATA. FOR THE IBM370 WE HAVE AN
C     ASSEMBLER AND FORTRAN VERSION. THE ASSMBLER VERSION WILL
C     RUN TWO TO THREE TIMES FASTER. THE FORTRAN VERSION IS TO
C     MAKE THE CODE MORE PORTABLE. FOR A VAX OR IBM PC WE HAVE
C     ANOTHER VERSION, IT REVERSED THE ORDER OF THE BYTES IN
C     AN INTEGER WORD.  W3AI41 CAN BE REPLACED BY NCAR GBYTES
C     BIT UNPACKER. NCAR HAS A LARGE NUMBER OF VERSIONS OF GBYTES
C     IN FORTRAN AND ASSEMBLER FOR A NUMBER OF DIFFERENT BRANDS OF
C     COMPUTERS. THEY ALSO HAVE A C VERSION.
C
C     ALIGN CHARACTER ARRAY MSGA STARTING ADDRESS ON CRAY
C     INTEGER WORD BOUNDARY
C
          LLL  = MOD(ISS+10,8)
          NNN  = 11 - LLL
          KOFF = LLL * 8
C
          CALL GBYTES(MSGA(ISS+NNN),KSAVE,KOFF,KBITS,0,KENTRY)
C
C  ------------- CORRECTLY PLACE ALL ENTRIES
C
          II    = 1
          KENTRY = KPTR(10)
          DO 500 I = 1, KENTRY
            IF (KBMS(I)) THEN
C                                         MUST INCLUDE DECIMAL SCALE
                DATA(I) = (REFNCE + FLOAT(KSAVE(II)) * SCALE) / ASCALE
                II  = II + 1
            ELSE
                DATA(I) = 0.0
            END IF
  500     CONTINUE
          GO TO 900
      END IF
C
C  ------------- PROCESS SPHERICAL HARMONIC COEFFICIENTS
C
      IKK = 0
      DO 5500 I =  0, 3
          KK(I+1) = MSGA(I+ISS+11)
 5500 CONTINUE
C
      IF (.NOT.IBM370) THEN
        KOFF = 0
C  GET 1 BIT SIGN
        CALL GBYTE(IKK,ISGN,0,1)
C  GET 7 BIT EXPONENT
        CALL GBYTE(IKK,IEXP,1,7)
C  GET 24 BIT FRACTION
        CALL GBYTE(IKK,IFR,8,24)
        IF (IFR.EQ.0.OR.IEXP.EQ.0) THEN
          REALKK = 0.0
        ELSE
          REALKK = FLOAT(IFR) * 16.0 ** (IEXP-64-6)
          IF (ISGN.NE.0) REALKK = - REALKK
        ENDIF
      ENDIF
C
      DATA(1)  = REALKK
      KOFF     = 0
C     CALL XMOVEX(MSGB,MSGA(ISS+15),NRBYTE)
C
C  ------------- UNPACK ALL FIELDS
C
C     CALL W3AI41(MSGB,KSAVE,KBITS,KENTRY,KOFF)
C  --------------
C
C     ALIGN CHARACTER ARRAY MSGA STARTING ADDRESS ON CRAY
C     INTEGER WORD BOUNDARY
C
          LLL  = MOD(ISS+14,8)
          NNN  = 15 - LLL
          KOFF = LLL * 8
C
          CALL GBYTES(MSGA(ISS+NNN),KSAVE,KOFF,KBITS,0,KENTRY)
C
      DO 6000 I = 1, KENTRY
          DATA(I+1)  = REFNCE + FLOAT(KSAVE(I)) * SCALE
 6000 CONTINUE
  900 CONTINUE
      RETURN
      END

C> To test when gds is available to see if size mismatch
C> on existing grids (by center) is indicated.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-02-08
C> - Ralph Jones 1990-09-23 Change's for cray cft77 fortran.
C> - Ralph Jones 1990-12-05 Change's for grib nov. 21,1990.
C>
C> @param[in] J Size for indicated grid.
C> @param[in] KPDS
C> @param[in] KGDS
C> @param[out] KRET Error return.
C>
C> @note KRET = 9 - GDS indicates size mismatch with std grid.
C>
C> @author Bill Cavanaugh @date 1988-02-08
C$$$
      SUBROUTINE AI087(*,J,KPDS,KGDS,KRET)
      INTEGER       KPDS(20)
      INTEGER       KGDS(13)
      INTEGER       J
      INTEGER       I
C  ---------------------------------------
C  ---------------------------------------
C           IF GDS NOT INDICATED, RETURN
C  ----------------------------------------
      IF (IAND(KPDS(4),128).EQ.0) RETURN
C  ---------------------------------------
C            GDS IS INDICATED, PROCEED WITH TESTING
C  ---------------------------------------
      I     = KGDS(2) * KGDS(3)
C  ---------------------------------------
C            TEST ECMWF CONTENT
C  ---------------------------------------
      IF (KPDS(1).EQ.98) THEN
          KRET  = 9
          IF (KPDS(3).GE.1.AND.KPDS(3).LE.16) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE
              KRET  = 5
              RETURN 1
          END IF
C  ---------------------------------------
C           U.K. MET OFFICE, BRACKNELL
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.74) THEN
          KRET  = 9
          IF (KPDS(3).GE.21.AND.KPDS(3).LE.24) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE  IF (KPDS(3).EQ.25.OR.KPDS(3).EQ.26) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.70) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE
              KRET  = 5
              RETURN 1
          END IF
C  ---------------------------------------
C           NAVY - FNOC
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.58) THEN
          PRINT *,' NO CURRENT LISTING OF NAVY GRIDS'
          RETURN 1
C  ---------------------------------------
C                 U.S. GRIDS
C  ---------------------------------------
      ELSE IF (KPDS(1).EQ.7) THEN
          KRET  = 9
          IF (KPDS(3).EQ.5) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.6) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).GE.21.AND.KPDS(3).LE.24) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE  IF (KPDS(3).EQ.25.OR.KPDS(3).EQ.26) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.27.OR.KPDS(3).EQ.28) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.29.OR.KPDS(3).EQ.30) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.33.OR.KPDS(3).EQ.34) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.50) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).GE.61.AND.KPDS(3).LE.64) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.70) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.85.OR.KPDS(3).EQ.86) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.100) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.101) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.102) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).EQ.103) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE IF (KPDS(3).GE.201.AND.KPDS(3).LE.214) THEN
              IF (I.NE.J) THEN
                  RETURN 1
              END IF
          ELSE
              KRET  = 5
              RETURN 1
          END IF
      ELSE
          KRET  = 10
          RETURN 1
      END IF
C  ------------------------------------
C                    NORMAL EXIT
C  ------------------------------------
      KRET  = 0
      RETURN
      END
