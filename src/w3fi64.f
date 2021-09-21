C> @file
C> @brief NMC office note 29 report unpacker.
C> @author L. Marx @date 1990-01

C> Unpacks an array of upper-air reports that are packed in
C> the format described by NMC office note 29, or unpacks an array
C> of surface reports that are packed in the format described by NMC
C> office note 124. Input character data are converted to integer,
C> real or character type as specified in the category tables below.
C> Missing integer data are replaced with 99999, missing real data
C> are replaced with 99999.0 and missing character data are replaced
C> with blanks. This library is similar to w3ai02() except w3ai02()
C> was written in assembler and could not handle internal read errors
C> (program calling w3ai02() would fail in this case w/o explanation).
C>
C> Program history log:
C> - L. Marx 1990-01 Converted code from assembler
C> to vs fortran; Expanded error return codes in 'NEXT'
C> - Dennis Keyser 1991-07-22 Use same arguments as w3ai02() ;
C> Streamlined code; Docblocked and commented; Diag-
C> nostic print for errors; Attempts to skip to NEXT
C> report in same record rather than exiting record.
C> - Dennis Keyser 1991-08-12 Slight changes to make sub-
C> program more portable; Test for absence of end-
C> of-record indicator, will gracefully exit record.
C> - Dennis Keyser 1992-06-29 Convert to cray cft77 fortran
C> - Dennis Keyser 1992-08-06 Corrected error which could
C> lead to the length for a concatenation operator
C> being less than 1 when an input parameter spans
C> across two 10-character words.
C>
C> @param[in] COCBUF Character*10 array containing a block of packed
C> reports in nmc office note 29/124 format.
C> @param[in] NEXT Marker indicating relative location (in bytes) of
C> end of last report in COCBUF. Exception: NEXT must
C> be set to zero prior to unpacking the first report of
C> a new block of reports.  subsequently, the value of
C> NEXT returned by the previous call to w3fi64 should
C> be used as input.  (see output argument list below.)
C> if NEXT is negative, w3fi64 will return immediately
C> without action.
C> @param[out] LOCRPT Array containing one unpacked report with pointers
C> and counters to direct the user. Locrpt() must begin
C> on a fullword boundary. Format is mixed, user must
C> equivalence real and character arrays to this array
C> (see below and remarks for content).
C>
C>   ***************************************************************
C>
C> |word   |  content                  | unit                 | format |
C> | :---- | :----------------------   | :------------------- | :----- |
C> |  1    | latitude                  | 0.01 degrees         | real   |
C> |  2    | longitude                 | 0.01 degrees west    | real   |
C> |  3    | unused                    |                      |        |
C> |  4    | observation time          | 0.01 hours (utc)     | real   |
C> |  5    | reserved (3rd byte is     | 4-characters         | char*8 |
C> |       | on29 "25'th char.; 4th    | left-justified       |        |
C> |       |byte is on29 "26'th        |                      |        |
C> |       |char." (see on29)          |                      |        |
C> |  6    |reserved (3rd byte is      | 3-characters         | char*8 |
C> |       |on29 "27'th char. (see     |left-justified        |        |
C> |       |on29)                      |                      |        |
C> |  7    |station elevation          |meters                | real   |
C> |  8    |instrument type            |on29 table r.2        | integer|
C> |  9    |report type                |on29 table r.1 or     | integer|
C> |       |on124 table s.3            |                      |        |
C> |  10   |ununsed                    |                      |        |
C> |  11   |stn. id. (first 4 char.)   | 4-characters         | char*8 |
C> |       |left-justified             |                      |        |
C> |  12   |stn. id. (last  2 char.)   | 2-characters         | char*8 |
C> |       |left-justified             |                      |        |
C> |  13   |category  1, no. levels    | count                | integer|
C> |  14   |category  1, data index    | count                | integer|
C> |  15   |category  2, no. levels    | count                | integer|
C> |  16   |category  2, data index    | count                | integer|
C> |  17   |category  3, no. levels    | count                | integer|
C> |  18   |category  3, data index    | count                | integer|
C> |  19   |category  4, no. levels    | count                | integer|
C> |  20   |category  4, data index    | count                | integer|
C> |  21   |category  5, no. levels    | count                | integer|
C> |  22   |category  5, data index    | count                | integer|
C> |  23   |category  6, no. levels    | count                | integer|
C> |  24   |category  6, data index    | count                | integer|
C> |  25   |category  7, no. levels    | count                | integer|
C> |  26   |category  7, data index    | count                | integer|
C> |  27   |category  8, no. levels    | count                | integer|
C> |  28   |category  8, data index    | count                | integer|
C> |  29   |category 51, no. levels    | count                | integer|
C> |  30   |category 51, data index    | count                | integer|
C> |  31   |category 52, no. levels    | count                | integer|
C> |  32   |category 52, data index    | count                | integer|
C> |  33   |category  9, no. levels    | count                | integer|
C> |  34   |category  9, data index    | count                | integer|
C> | 35-42 |  zeroed out - not used    |                      | integer|
C> | 43-end| unpacked data groups     |(see remarks)         | mixed|
C>
C> ***************************************************************
C>
C> NEXT: Marker indicating relative location (in bytes)
C> of end of current report in COCBUF. NEXT will be
C> set to -1 if w3fi64() encounters string 'end record'
C> in place of the NEXT report. This is the end of the
C> block. No unpacking takes place. NEXT is set to-2
C> when internal (logic) errors have been detected.
C> NEXT is set to -3 when data count check fails. In
C> both of the latter cases some data (e.g., header
C> information) may be unpacked into LOCRPT.
C>
C> @note After first reading and processing the office note 85
C> (first) date record, the user's fortran program begins a read
C> loop as follows. For each iteration a blocked input report is
C> read into array COCBUF. Now test the first ten characters in
C> COCBUF for the string 'endof file' (sic). This string signals
C> the end of input. Otherwise, set the marker 'NEXT' to zero and
C> begin the unpacking loop.
C>
C> Each iteration of the unpacking loop consists of a call to
C> w3fi64() with the current value of 'NEXT'. If 'NEXT' is -1 upon
C> returning from w3fi64(), it has reached the end of the input
C> record, and the user's program should read the next record as
C> above. If 'NEXT' is -2 or -3 upon returning, there is a grievous
C> error in the current packed input record, and the user's program
C> should print it for examination by automation division personnel.
C> If 'NEXT' is positive, the output structure locrpt contains
C> an unpacked report, and the user's program should process it at
C> this point, subsequently repeating the unpacking loop.
C>
C>   EXAMPLE:
C> @code{.F}
C>         CHARACTER*10 COCBUF(644)
C>         CHARACTER*8  COCRPT(1608)
C>         CHARACTER*3  CQUMAN(20)
C>         INTEGER    LOCRPT(1608)
C>         REAL    ROCRPT(1608),GEOMAN(20),TMPMAN(20),DPDMAN(20),
C>        $ WDRMAN(20),WSPMAN(20)
C>         EQUIVALENCE (COCRPT,LOCRPT,ROCRPT)
C>
C>   C READ AND PROCESS THE OFFICE NOTE 85 DATE RECORD
C>         ..........
C>   C --- BEGIN READ LOOP
C>      10 CONTINUE
C>         READ (UNIT=INP, IOSTAT=IOS, NUM=NBUF) COCBUF
C>         IF(IOS .LT. 0)  GO TO (END OF INPUT)
C>         IF(IOS .GT. 0)  GO TO (INPUT ERROR)
C>         IF(NBUF .GT. 6432)  GO TO (BUFFER OVERFLOW)
C>         IF(COCBUF(1).EQ.'ENDOF FILE')  GO TO (END OF INPUT)
C>         NEXT = 0
C>   C ------ BEGIN UNPACKING LOOP
C>      20    CONTINUE
C>            CALL W3FI64(COCBUF, LOCRPT, NEXT)
C>            IF(NEXT .EQ. -1)  GO TO 10
C>            IF(NEXT .LT. -1)  GO TO (OFFICE NOTE 29/124 ERROR)
C>            RLAT = 0.01 * ROCRPT(1)    (LATITUDE)
C>            ..... ETC .....
C>   C --- BEGIN CATEGORY 1 FETCH -- MANDATORY LEVEL DATA
C>            IF(LOCRPT(13) .GT. 0)  THEN
C>               NLVLS = MIN(20,LOCRPT(13))
C>               INDX = LOCRPT(14)
C>               DO 66  I = 1,NLVLS
C>                  GEOMAN(I) = ROCRPT(INDX)
C>                  TMPMAN(I) = 0.1 * ROCRPT(INDX+1)
C>                  DPDMAN(I) = 0.1 * ROCRPT(INDX+2)
C>                  WDRMAN(I) = ROCRPT(INDX+3)
C>                  WSPMAN(I) = ROCRPT(INDX+4)
C>                  CQUMAN(I) = COCRPT(INDX+5)
C>                  INDX = INDX + 6
C>      66       CONTINUE
C>            END IF
C>            ..... ETC .....
C>            GO TO 20
C>            ...............
C> @endcode
C>
C> Data from the on29/124 record is unpacked into fixed locations
C> in words 1-12 and into indexed locations in word 43 and
C> following. Study on29 appendix c/on124 appendix s.2 carefully.
C> Each category (or group of fields) in the packed report has a
C> corresponding layout in locations in array LOCRPT that may be
C> found by using the corresponding index amount from words 14, 16,
C> ..., 34, in array LOCRPT.  For instance, if a report contains
C> one or more packed category 3 data groups (wind data at variable
C> pressure levels) that data will be unpacked into binary and
C> and character fields in one or more unpacked category 3 data
C> groups as described below. The number of levels will be stored
C> in word 17 and the index in fullwords of the first level of
C> unpacked data in the output array will be stored in word 18.
C> The second level, if any, will be stored beginning four words
C> further on, and so forth until the count in word 17 is
C> exhausted. The field layout in each category is given below...
C>
C> ***************************************************************
C> - CATEGORY 1 - MANDATORY LEVEL DATA
C>     |WORD   |PARAMETER            |UNITS               |FORMAT
C>     |:----   |:---------            |:-----------------   |:-------------|
C>     |  1    |GEOPOTENTIAL         |METERS              |REAL|
C>     |  2    |TEMPERATURE          |0.1 DEGREES C       |REAL|
C>     |  3    |DEWPOINT DEPRESSION  |0.1 DEGREES C       |REAL|
C>     |  4    |WIND DIRECTION       |DEGREES             |REAL|
C>     |  5    |WIND SPEED           |KNOTS               |REAL|
C>     |  6    |QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     GEOPOTENTIAL    |ON29 TABLE Q.A| |
C>     |       |     TEMPERATURE     |ON29 TABLE Q.A| |
C>     |       |     DEWPOINT DEPR.  |ON29 TABLE Q.C| |
C>     |       |     WIND            |ON29 TABLE Q.A| |
C>
C> ***************************************************************
C> - CATEGORY 2 - TEMPERATURE AT VARIABLE PRESSURE
C>     |WORD   |PARAMETER            |UNITS               | FORMAT|
C>     |----   |---------            |-----------------   | -------------|
C>     |  1    |PRESSURE             |0.1 MILLIBARS       | REAL|
C>     |  2    |TEMPERATURE          |0.1 DEGREES C       | REAL|
C>     |  3    |DEWPOINT DEPRESSION  |0.1 DEGREES C       | REAL|
C>     |  4    |QUALITY MARKERS:     |EACH 1-CHARACTER    | CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     PRESSURE        |ON29 TABLE Q.B| |
C>     |       |     TEMPERATURE     |ON29 TABLE Q.A| |
C>     |       |     DEWPOINT DEPR.  |ON29 TABLE Q.C| |
C>     |       |     NOT USED        |BLANK| |
C>
C> ***************************************************************
C> - CATEGORY 3 - WINDS AT VARIABLE PRESSURE
C>     |WORD   |PARAMETER           | UNITS               | FORMAT|
C>     |----   |---------           | -----------------   | -------------|
C>     |  1    |PRESSURE            | 0.1 MILLIBARS       | REAL|
C>     |  2    |WIND DIRECTION      | DEGREES             | REAL|
C>     |  3    |WIND SPEED          | KNOTS               | REAL|
C>     |  4    |QUALITY MARKERS:    | EACH 1-CHARACTER    | CHAR*8|
C>     |       |                    | LEFT-JUSTIFIED| |
C>     |       |     PRESSURE       | ON29 TABLE Q.B| |
C>     |       |     WIND           | ON29 TABLE Q.A| |
C>     |       |     NOT USED       | BLANK| |
C>     |       |     NOT USED       | BLANK| |
C>
C> ***************************************************************
C> - CATEGORY 4 - WINDS AT VARIABLE HEIGHTS
C>     |WORD   |PARAMETER            |UNITS               |FORMAT|
C>     |----   |---------            |-----------------   |-------------|
C>     |  1    |GEOPOTENTIAL         |METERS              |REAL|
C>     |  2    |WIND DIRECTION       |DEGREES             |REAL|
C>     |  3    |WIND SPEED           |KNOTS               |REAL|
C>     |  4    |QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     GEOPOTENTIAL    |ON29 TABLE Q.B| |
C>     |       |     WIND            |ON29 TABLE Q.A| |
C>     |       |     NOT USED        |BLANK| |
C>     |       |     NOT USED        |BLANK| |
C>
C> ***************************************************************
C> - CATEGORY 5 - TROPOPAUSE DATA
C>     |WORD   |PARAMETER            |UNITS               |FORMAT|
C>     |----   |---------            |-----------------   |-------------|
C>     |  1    |GEOPOTENTIAL         |METERS              |REAL|
C>     |  2    |TEMPERATURE          |0.1 DEGREES C       |REAL|
C>     |  3    |DEWPOINT DEPRESSION  |0.1 DEGREES C       |REAL|
C>     |  4    |WIND DIRECTION       |DEGREES             |REAL|
C>     |  5    |WIND SPEED           |KNOTS               |REAL|
C>     |  6    |QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     PRESSURE        |ON29 TABLE Q.B| |
C>     |       |     TEMPERATURE     |ON29 TABLE Q.A| |
C>     |       |     DEWPOINT DEPR.  |ON29 TABLE Q.C| |
C>     |       |     WIND            |ON29 TABLE Q.A| |
C>
C> ***************************************************************
C> - CATEGORY 6 - CONSTANT-LEVEL DATA (AIRCRAFT, SAT. CLOUD-DRIFT)
C>     |WORD  | PARAMETER            |UNITS               |FORMAT|
C>     |----  | ---------            |-----------------   |-------------|
C>     |  1   | PRESSURE ALTITUDE    |METERS              |REAL|
C>     |  2   | TEMPERATURE          |0.1 DEGREES C       |REAL|
C>     |  3   | DEWPOINT DEPRESSION  |0.1 DEGREES C       |REAL|
C>     |  4   | WIND DIRECTION       |DEGREES             |REAL|
C>     |  5   | WIND SPEED           |KNOTS               |REAL|
C>     |  6   | QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |      |                      |LEFT-JUSTIFIED| |
C>     |      |      PRESSURE        |ON29 TABLE Q.6| |
C>     |      |      TEMPERATURE     |ON29 TABLE Q.6| |
C>     |      |      DEWPOINT DEPR.  |ON29 TABLE Q.6| |
C>     |      |      WIND            |ON29 TABLE Q.6C | |
C>
C> ***************************************************************
C> - CATEGORY 7 - CLOUD COVER
C>     |WORD   |PARAMETER            |UNITS               |FORMAT|
C>     |----   |---------            |-----------------   |-------------|
C>     |  1    |PRESSURE             |0.1 MILLIBARS       |REAL|
C>     |  2    |AMOUNT OF CLOUDS     |PER CENT            |REAL|
C>     |  3    |QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     PRESSURE        |ON29 TABLE Q.7| |
C>     |       |     CLOUD AMOUNT    |ON29 TABLE Q.7| |
C>     |       |     NOT USED        |BLANK| |
C>     |       |     NOT USED        |BLANK| |
C>
C> ***************************************************************
C> - CATEGORY 8 - ADDITIONAL DATA
C>     |WORD   |PARAMETER           | UNITS                |FORMAT|
C>     |----   |---------           | -----------------    |-------------|
C>     |  1    |SPECIFIED IN ON29   | VARIABLE             |REAL|
C>     |       |TABLE 101.1 OR      | | |
C>     |       |ON124 TABLE SM.8A.1 | | |
C>     |  2    |FORM OF ADD'L DATA   |CODE FIGURE FROM     |REAL|
C>     |       |                     |ON29 TABLE 101 OR | |
C>     |       |                     |ON124 TABLE SM.8A | |
C>     |  3    |QUALITY MARKERS:     |EACH 1-CHARACTER     |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED | |
C>     |       |     VALUE 1         |ON29 TABLE Q.8 OR | |
C>     |       |                     |ON124 TABLE SM.8B | |
C>     |       |     VALUE 2         |ON29 TABLE Q.8A OR | |
C>     |       |                     |ON124 TABLE SM.8C | |
C>     |       |     NOT USED        |BLANK | |
C>     |       |     NOT USED        |BLANK | |
C>
C> ***************************************************************
C> - CATEGORY 51 - SURFACE DATA
C>     |WORD   |PARAMETER            |UNITS               |FORMAT|
C>     |----   |---------            |-----------------   |-------------|
C>     |  1    |SEA-LEVEL PRESSURE   |0.1 MILLIBARS       |REAL|
C>     |  2    |STATION PRESSURE     |0.1 MILLIBARS       |REAL|
C>     |  3    |WIND DIRECTION       |DEGREES             |REAL|
C>     |  4    |WIND SPEED           |KNOTS               |REAL|
C>     |  5    |AIR TEMPERATURE      |0.1 DEGREES C       |REAL|
C>     |  6    |DEWPOINT DEPRESSION  |0.1 DEGREES C       |REAL|
C>     |  7    |MAXIMUM TEMPERATURE  |0.1 DEGREES C       |REAL|
C>     |  8    |MINIMUM TEMPERATURE  |0.1 DEGREES C       |REAL|
C>     |  9    |QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     S-LEVEL PRESS.  |ON124 TABLE SM.51| |
C>     |       |     STATION PRESS.  |ON124 TABLE SM.51| |
C>     |       |     WIND            |ON124 TABLE SM.51| |
C>     |       |     AIR TEMPERATURE |ON124 TABLE SM.51| |
C>     | 10    |QUALITY MARKERS:     |EACH 1-CHARACTER    |CHAR*8|
C>     |       |                     |LEFT-JUSTIFIED| |
C>     |       |     DEWPOINT DEPR.  |ON124 TABLE SM.51| |
C>     |       |     NOT USED        |BLANK| |
C>     |       |     NOT USED        |BLANK| |
C>     |       |     NOT USED        |BLANK| |
C>     | 11    |HORIZ. VISIBILITY    |WMO CODE TABLE 4300  |INTEGER|
C>     | 12    |PRESENT WEATHER      |WMO CODE TABLE 4677  |INTEGER|
C>     | 13    |PAST WEATHER         |WMO CODE TABLE 4561  |INTEGER|
C>     | 14    |TOTAL CLOUD COVER N  |WMO CODE TABLE 2700  |INTEGER|
C>     | 15    |CLOUD COVER OF C/LN  |WMO CODE TABLE 2700  |INTEGER|
C>     | 16    |CLOUD TYPE OF C/L    |WMO CODE TABLE 0513  |INTEGER|
C>     | 17    |CLOUD HEIGHT OF C/L  |WMO CODE TABLE 1600  |INTEGER|
C>     | 18    |CLOUD TYPE OF C/M    |WMO CODE TABLE 0515  |INTEGER|
C>     | 19    |CLOUD TYPE OF C/H    |WMO CODE TABLE 0509  |INTEGER|
C>     | 20    |CHARACTERISTIC OF    |WMO CODE TABLE 0200  |INTEGER|
C>     |       |3-HR PRESS TENDENCY | | |
C>     | 21    |AMT. PRESS TENDENCY  |0.1 MILLIBARS |      REAL|
C>     |       |(50.0 WILL BE ADDED TO INDICATE 24-HR TENDENCY)| | |
C>
C> ***************************************************************
C> - CATEGORY 52 - ADDITIONAL SURFACE DATA
C>     |WORD  | PARAMETER            |UNITS               |FORMAT|
C>     |----  | ---------            |-----------------   |-------------|
C>     |  1   | 6-HR PRECIPITATION   |0.01 INCH           |INTEGER|
C>     |  2   | SNOW DEPTH           |INCH                |INTEGER|
C>     |  3   | 24-HR PRECIPITATION  |0.01 INCH           |INTEGER|
C>     |  4   | DURATION OF PRECIP.  |NO. 6-HR PERIODS    |INTEGER|
C>     |  5   | PERIOD OF WAVES      |SECONDS             |INTEGER|
C>     |  6   | HEIGHT OF WAVES      |0.5 METERS          |INTEGER|
C>     |  7   | SWELL DIRECTION      |WMO CODE TABLE 0877 |INTEGER|
C>     |  8   | SWELL PERIOD         |SECONDS             |INTEGER|
C>     |  9   | SWELL HEIGHT         |0.5 METERS          |INTEGER|
C>     | 10   | SEA SFC TEMPERATURE  |0.1 DEGREES C       |INTEGER|
C>     | 11   | SPECIAL PHEN, GEN'L   |                   |INTEGER|
C>     | 12   | SPECIAL PHEN, DET'L   |                   |INTEGER|
C>     | 13   | SHIP'S COURSE        |WMO CODE TABLE 0700 |INTEGER|
C>     | 14   | SHIP'S AVERAGE SPEED |WMO CODE TABLE 4451 |INTEGER|
C>     | 15   | WATER EQUIVALENT OF  0.01 INCH |          |INTEGER|
C>     |      | SNOW AND/OR ICE| | |
C>
C> ***************************************************************
C> - CATEGORY 9 - PLAIN LANGUAGE DATA (ALPHANUMERIC TEXT)
C>     |WORD   |BYTES  |PARAMETER                                  |FORMAT |
C>     |----   |-----  |---------------------------------------    |-------- |
C>     |  1    |   1   |INDICATOR OF CONTENT (ON124 TABLE SM.9)    |CHAR*8 |
C>     |       |       |  (1 CHARACTER)| |
C>     |       | 2-4   |PLAIN LANGUAGE DATA, TEXT CHARACTERS 1-3| |
C>     |       | 4-8   |NOT USED (BLANK) | |
C>     |  2    | 1-4   |PLAIN LANGUAGE DATA, TEXT CHARACTERS 4-7   |CHAR*8 |
C>     |       | 4-8   |NOT USED (BLANK)| |
C>     |  3    | 1-4   |PLAIN LANGUAGE DATA, TEXT CHARACTERS 8-11  |CHAR*8 |
C>     |       | 4-8   |NOT USED (BLANK)| |
C>
C> @note One report may unpack into more than one category having
C> multiple levels. The unused portion of LOCRPT is not cleared.
C>
C> @note Entry w3ai02() duplicates processing in w3fi64() since no
C> assembly language code in cray w3lib.
C>
C> @author L. Marx @date 1990-01
      SUBROUTINE W3FI64(COCBUF,LOCRPT,NEXT)
C
      CHARACTER*12  HOLD
      CHARACTER*10  COCBUF(*)
      CHARACTER*7   CNINES
      CHARACTER*4   COCRPT(10000),BLANK
      CHARACTER*2   KAT(11)
C
      INTEGER  LOCRPT(*),KATGC(20,11),KATGL(20,11),KATL(11),KATO(11),
     $ MOCRPT(5000)
C
      REAL  ROCRPT(5000)
C
      EQUIVALENCE (ROCRPT,MOCRPT,COCRPT)
C
      SAVE
C
      DATA  BLANK/'    '/,CNINES/'9999999'/,IMSG/99999/,XMSG/99999./
      DATA  KATL/6,4,4,4,6,6,3,3,1,20,15/,KATO/13,15,17,19,21,23,25,27,
     $ 33,29,31/,IREC/2/
      DATA  KAT/'01','02','03','04','05','06','07','08','09','51','52'/
      DATA KATGC/    5*2,4,14*0, 3*2,4,16*0, 3*2,4,16*0, 3*2,4,16*0,
     $ 5*2,4,14*0,   5*2,4,14*0, 2*2,4,17*0, 2*2,4,17*0, 4,19*0,
     $ 8*2,4,10*1,2, 15*1,5*0/
      DATA KATGL/    5,4,3*3,4,14*0, 5,4,2*3,16*0,   5,2*3,2,16*0,
     $ 5,2*3,2,16*0, 5,4,3*3,4,14*0, 5,4,3*3,4,14*0, 5,3,2,17*0,
     $ 5,3,2,17*0,   12,19*0,
     $ 2*5,2*3,4,3,2*4,5,2*3,7*2,1,3, 4,3,4,1,5*2,4,2*2,1,2,7,5*0/
      DATA LWFLAG/0/
C
           ENTRY      W3AI02(COCBUF,LOCRPT,NEXT)
C
      IF (LWFLAG.EQ.0)  THEN
C FIRST TIME CALLED, DETERMINE MACHINE WORD LG IN BYTES (=8 FOR CRAY)
C     DEPENDING ON WORD SIZE LW2*I-LW1 INDEXES THRU COCRPT
C     EITHER AS 1,2,3...I    FOR LW = 4 OR
C            AS 1,3,5..2*I-1 FOR LW = 8 <------ HERE
C     NECESSITATED BY LEFT JUSTIFICATION OF EQUIVALENCE
         CALL W3FI01(LW)
         LW2 = LW/4
         LW1 = LW/8
         LWFLAG = 1
      END IF
 7000 CONTINUE
      IF(NEXT.LT.0)  RETURN
      NEXTO = NEXT/10
      N = NEXT/10 + 1
C
      IF(COCBUF(N).EQ.'END RECORD'.OR.COCBUF(N).EQ.'XXXXXXXXXX')  THEN
C HIT END-OF-RECORD; RETURN WITH NEXT = -1
         IF(COCBUF(N).EQ.'XXXXXXXXXX')  PRINT 109, IREC
         IREC = IREC + 1
         NEXT = -1
         RETURN
      END IF
C INITIALIZE REPORT ID AS MISSING OR 0 FOR RESERVED WORDS
      ROCRPT(1)  = XMSG
      ROCRPT(2)  = XMSG
      ROCRPT(3)  = 0.
      ROCRPT(4)  = XMSG
      COCRPT(LW2*5-LW1) = '    '
      COCRPT(LW2*6-LW1) = '    '
      ROCRPT(7)  = XMSG
      MOCRPT(8)  = 99
      MOCRPT(9)  = IMSG
      MOCRPT(10) = 0.
      COCRPT(LW2*11-LW1) = '    '
      COCRPT(LW2*12-LW1) = '    '
C INITIALIZE CATEGORY WORD PAIRS AS ZEROES
      DO 100  MB = 13,42
         MOCRPT(MB) = 0
  100 CONTINUE
C WRITE OUT LATITUDE INTO WORD 1 (REAL)
      M = 1
      IF(COCBUF(N)(1:5).NE.'99999')  READ(COCBUF(N)(1:5),51)  ROCRPT(M)
C WRITE OUT LONGITUDE INTO WORD 2 (REAL)
      M = 2
      IF(COCBUF(N)(6:10).NE.'99999')  READ(COCBUF(N)(6:10),51) ROCRPT(M)
C WORD 3 IS RESERVED (KEEP AS A REAL NUMBER OF 0.)
C WRITE OUT STATION ID TO WORDS 11 AND 12 (CHAR*8)
C (CHAR. 1-4 OF ID IN WORD 11, CHAR. 5-6 OF ID IN WORD 12, LEFT-JUSTIF.)
      M = 11
      N = N + 1
      COCRPT(LW2*M-LW1) = COCBUF(N)(1:4)
      M = 12
      COCRPT(LW2*M-LW1) = COCBUF(N)(5:6)//'  '
C WRITE OUT OBSERVATION TIME INTO WORD 4 (REAL)
      M = 4
      IF(COCBUF(N)(7:10).NE.'9999')  READ(COCBUF(N)(7:10),41)  ROCRPT(M)
C WORD 5 IS RESERVED (CHAR*8) (4 CHARACTERS, LEFT-JUSTIF.)
      M = 5
      N = N + 1
      COCRPT(LW2*M-LW1) = COCBUF(N)(3:6)
C WORD 6 IS RESERVED (CHAR*8) (3 CHARACTERS, LEFT-JUSTIF.)
      M = 6
      COCRPT(LW2*M-LW1) = COCBUF(N)(1:2)//COCBUF(N)(7:7)//' '
C WRITE OUT REPORT TYPE INTO WORD 9 (INTEGER)
      M = 9
      READ(COCBUF(N)(8:10),30)  MOCRPT(M)
C WRITE OUT STATION ELEVATION INTO WORD 7 (REAL)
      N = N + 1
      M = 7
      IF(COCBUF(N)(1:5).NE.'99999')  READ(COCBUF(N)(1:5),51)  ROCRPT(M)
C WRITE OUT INSTRUMENT TYPE INTO WORD 8 (INTEGER)
      M = 8
      IF(COCBUF(N)(6:7).NE.'99')  READ(COCBUF(N)(6:7),20)  MOCRPT(M)
C READ IN NWDS, THE TOTAL NO. OF 10-CHARACTER WORDS IN ENTIRE REPORT
      READ(COCBUF(N)(8:10),30)  NWDS
C 'MO' WILL BE STARTING LOCATION IN MOCRPT FOR THE DATA
      MO = 43
      N = N + 1
  700 CONTINUE
      IF(COCBUF(N).EQ.'END REPORT')  THEN
C-----------------------------------------------------------------------
C HAVE HIT THE END OF THE REPORT
         IF(N-NEXTO.EQ.NWDS)  THEN
C EVERYTHING LOOKS GOOD, RETURN WITH NEXT SET TO LAST BYTE IN REPORT
            NEXT = N * 10
         ELSE
C PROBLEM, MAY EXIT WITH NEXT = -3
            NEXTX = -3
            PRINT 101,
     &      COCRPT(LW2*11-LW1),COCRPT(LW2*12-LW1)(1:2),N-NEXTO,NWDS
            GO TO 99
         END IF
         MWORDS = MO - 1
         DO 1001  I =1, MWORDS
            LOCRPT(I) = MOCRPT(I)
 1001    CONTINUE
         RETURN
C-----------------------------------------------------------------------
      END IF
C READ IN NWDSC, THE RELATIVE POSITION IN RPT OF THE NEXT CATEGORY
      READ(COCBUF(N)(3:5),30)  NWDSC
C READ IN LVLS, THE NUMBER OF LEVELS IN THE CURRENT CATEGORY
      READ(COCBUF(N)(6:7),20)  LVLS
C DETERMINE THE CATEGORY NUMBER OF THE CURRENT CATEGORY
      DO 800  NCAT = 1,11
         IF(COCBUF(N)(1:2).EQ.KAT(NCAT))  GO TO 1000
  800 CONTINUE
C-----------------------------------------------------------------------
C PROBLEM, CAT. CODE IN INPUT NOT VALID; MAY EXIT WITH NEXT = -2
      NEXTX = -2
      PRINT 102,
     $ COCRPT(LW2*11-LW1),COCRPT(LW2*12-LW1)(1:2),COCBUF(N)(1:2)
      GO TO 99
C-----------------------------------------------------------------------
 1000 CONTINUE
C 'M' IS THE WORD IN MOCRPT WHERE THE NO. OF LEVELS WILL BE WRITTEN
      M = KATO(NCAT)
C WRITE THIS CATEGORY WORD PAIR OUT
      MOCRPT(M) = LVLS
      MOCRPT(M+1) = MO
      N = N + 1
      I = 1
C***********************************************************************
C       LOOP THROUGH ALL THE LEVELS IN THE CURRENT CATEGORY
C***********************************************************************
      DO 2000  L = 1,LVLS
C NDG IS NO. OF OUTPUT PARAMETERS PER LEVEL IN THIS CATEGORY
         NDG = KATL(NCAT)
C-----------------------------------------------------------------------
C          LOOP THROUGH ALL THE PARAMETERS IN THE CURRENT LEVEL
C-----------------------------------------------------------------------
         DO 1800  K = 1,NDG
C 'LL' IS THE NUMBER OF INPUT CHARACTERS PER PARAMETER FOR THIS CATEGORY
            LL = KATGL(K,NCAT)
C 'I' IS POINTER FOR BEGINNING BYTE IN C*10 WORD FOR NEXT PARAMETER
C 'J' IS POINTER FOR ENDING BYTE IN C*10 WORD FOR NEXT PARAMETER
            J = I + LL - 1
            IF(J.GT.10)  THEN
C COME HERE IF INPUT PARAMETER SPANS ACROSS TWO C*10 WORDS
               HOLD(1:LL) = COCBUF(N)(I:10)//COCBUF(N+1)(1:J-10)
               N = N + 1
               I = J - 9
               IF(I.GE.11)  THEN
                  N = N + 1
                  I = 1
               END IF
            ELSE
               HOLD(1:LL) = COCBUF(N)(I:J)
               I = J + 1
               IF(I.GE.11)  THEN
                  N = N + 1
                  I = 1
               END IF
            END IF
C KATGC IS AN INDICATOR FOR THE OUTPUT FORMAT OF EACH INPUT PARAMETER
C  (=2 - REAL, =1 - INTEGER, =4 - CHARACTER*8)
            IF(KATGC(K,NCAT).EQ.4)   GO TO 1500
            IF(KATGC(K,NCAT).NE.1.AND.KATGC(K,NCAT).NE.2)  THEN
C.......................................................................
C PROBLEM IN INTERNAL READ; MAY EXIT WITH NEXT = -2
               NEXTX = -2
               PRINT 104, COCRPT(LW2*11-LW1),COCRPT(LW2*12)(1:2)
               GO TO 99
C.......................................................................
            END IF
            IF(HOLD(1:LL).EQ.CNINES(1:LL))  THEN
C INPUT PARAMETER IS MISSING OR NOT APPLICABLE -- OUTPUT IT AS SUCH
               IF(KATGC(K,NCAT).EQ.1)  MOCRPT(MO) = IMSG
               IF(KATGC(K,NCAT).EQ.2)  ROCRPT(MO) = XMSG
               GO TO 1750
            END IF
            IF(LL.EQ.1)  THEN
C INPUT PARAMETER CONSISTS OF ONE CHARACTER
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),10)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),11)  ROCRPT(MO)
            ELSE  IF(LL.EQ.2)  THEN
C INPUT PARAMETER CONSISTS OF TWO CHARACTERS
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),20)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),21)  ROCRPT(MO)
            ELSE  IF(LL.EQ.3)  THEN
C INPUT PARAMETER CONSISTS OF THREE CHARACTERS
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),30)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),31)  ROCRPT(MO)
            ELSE  IF(LL.EQ.4)  THEN
C INPUT PARAMETER CONSISTS OF FOUR CHARACTERS
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),40)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),41)  ROCRPT(MO)
            ELSE  IF(LL.EQ.5)  THEN
C INPUT PARAMETER CONSISTS OF FIVE CHARACTERS
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),50)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),51)  ROCRPT(MO)
            ELSE  IF(LL.EQ.6)  THEN
C INPUT PARAMETER CONSISTS OF SIX CHARACTERS
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),60)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),61)  ROCRPT(MO)
            ELSE  IF(LL.EQ.7)  THEN
C INPUT PARAMETER CONSISTS OF SEVEN CHARACTERS
               IF(KATGC(K,NCAT).EQ.1)  READ(HOLD(1:LL),70)  MOCRPT(MO)
               IF(KATGC(K,NCAT).EQ.2)  READ(HOLD(1:LL),71)  ROCRPT(MO)
            ELSE
C.......................................................................
C INPUT PARAMETER CONSISTS OF MORE THAN SEVEN CHARACTERS (NOT PERMITTED)
               NEXTX = -2
               PRINT 108, COCRPT(LW2*11-LW1),COCRPT(LW2*12-LW1)(1:2)
               GO TO 99
C.......................................................................
            END IF
            GO TO 1750
 1500       CONTINUE
C.......................................................................
C OUTPUT CHARACTER (MARKER) PROCESSING COMES HERE
            IF(LL.LT.4)  THEN
C THERE ARE ONE, TWO OR THREE MARKERS IN THE INPUT WORD
               COCRPT(LW2*MO-LW1)(1:4)=HOLD(1:LL)//BLANK(1:4-LL)
            ELSE  IF(LL.EQ.4)  THEN
C THERE ARE FOUR MARKERS IN THE INPUT WORD
               COCRPT(LW2*MO-LW1)(1:4) = HOLD(1:LL)
            ELSE
C THERE ARE MORE THAN FOUR MARKERS IN THE INPUT WORD
               IP = 1
 1610          CONTINUE
               JP = IP + 3
               IF(JP.LT.LL)  THEN
C FILL FIRST FOUR MARKERS TO OUTPUT WORD
                  COCRPT(LW2*MO-LW1)(1:4) = HOLD(IP:JP)
                  MO = MO + 1
                  IP = JP + 1
                  GO TO 1610
               ELSE  IF(JP.EQ.LL)  THEN
C FILL FOUR REMAINING MARKERS TO NEXT OUTPUT WORD
                  COCRPT(LW2*MO-LW1)(1:4) = HOLD(IP:JP)
               ELSE
C FILL ONE, TWO, OR THREE REMAINING MARKERS TO NEXT OUTPUT WORD
                  COCRPT(LW2*MO-LW1)(1:4) = HOLD(IP:LL)//BLANK(1:JP-LL)
               END IF
            END IF
C.......................................................................
 1750       CONTINUE
            MO = MO + 1
 1800    CONTINUE
C-----------------------------------------------------------------------
 2000 CONTINUE
C***********************************************************************
      IF(I.GT.1)  N = N + 1
      IF(N-NEXTO.NE.NWDSC)  THEN
C-----------------------------------------------------------------------
C PROBLEM, REL. LOCATION OF NEXT CAT. NOT WHAT'S EXPECTED; MAY EXIT
C  WITH NEXT = -3
C ERROR - RELATIVE LOCATION OF NEXT CATEGORY NOT WHAT'S EXPECTED
         NEXTX = -3
         PRINT 105, COCRPT(LW2*11-LW1),COCRPT(LW2*12-LW1)(1:2),
     $              KAT(NCAT),N-NEXTO-1,
     $    NWDSC-1
         GO TO 99
C-----------------------------------------------------------------------
      END IF
C GO ON TO NEXT CATEGORY
      GO TO 700
C-----------------------------------------------------------------------
C ALL OF THE PROBLEM REPORTS END UP HERE -- ATTEMPT TO MOVE AHEAD TO
C  NEXT REPORT, IF NOT POSSIBLE THEN EXIT WITH NEXT = -2 OR -3 MEANING
C  THE REST OF THE RECORD IS BAD, GO ON TO NEXT RECORD
   99 CONTINUE
      DO 98  I = 1,644
         N = N + 1
         IF(N.GT.644)  GO TO 97
         IF(COCBUF(N).EQ.'END RECORD')  GO TO 97
         IF(COCBUF(N).EQ.'END REPORT')  THEN
C WE'VE MADE IT TO THE END OF THIS PROBLEM REPORT - START OVER WITH
C  NEXT ONE
            PRINT 106
            NEXT = N * 10
            GO TO 7000
         END IF
   98 CONTINUE
   97 CONTINUE
C COULDN'T GET TO THE END OF THIS PROBLEM REPORT - RETURN WITH ORIGINAL
C  NEXT VALUE (-2 OR -3) MEANING USER MUST GO ON TO NEXT RECORD
      NEXT = NEXTX
      PRINT 107, NEXT
      MWORDS = MO - 1
      DO 1002  I =1, MWORDS
         LOCRPT(I) = MOCRPT(I)
 1002 CONTINUE
      RETURN
C-----------------------------------------------------------------------
   10 FORMAT(I1)
   11 FORMAT(F1.0)
   20 FORMAT(I2)
   21 FORMAT(F2.0)
   30 FORMAT(I3)
   31 FORMAT(F3.0)
   40 FORMAT(I4)
   41 FORMAT(F4.0)
   50 FORMAT(I5)
   51 FORMAT(F5.0)
   60 FORMAT(I6)
   61 FORMAT(F6.0)
   70 FORMAT(I7)
   71 FORMAT(F7.0)
  101 FORMAT(/' *** W3FI64 ERROR- REPORT: ',A4,A2,'; ACTUAL NO. 10-CHAR'
     $,' WORDS:',I10,' NOT EQUAL TO VALUE READ IN WITH REPORT:',I10/6X,
     $ '- MAY BE DUE TO INTERNAL READ PROBLEM ASSOC. W/ EITHER ORIG. ',
     $ 'PACKING OR TRANSFER OF FILE RESULTING IN UNPROCESSABLE INFO.'/6X
     $,'- WILL ATTEMPT TO MOVE AHEAD TO NEXT REPORT, IF NOT POSSIBLE ',
     $ 'WILL EXIT RECORD WITH NEXT = -3'/)
  102 FORMAT(/' *** W3FI64 ERROR- REPORT: ',A4,A2,'; PACKED CATEGORY '
     $,'CODE: ',A2,' IS NOT A VALID O.N. 29 CATEGORY'/6X,
     $ '- MAY BE DUE TO INTERNAL READ PROBLEM ASSOC. W/ EITHER ORIG. ',
     $ 'PACKING OR TRANSFER OF FILE RESULTING IN UNPROCESSABLE INFO.'/6X
     $,'- WILL ATTEMPT TO MOVE AHEAD TO NEXT REPORT, IF NOT POSSIBLE ',
     $ 'WILL EXIT RECORD WITH NEXT = -2'/)
  104 FORMAT(/' *** W3FI64 ERROR- REPORT: ',A4,A2,'; INTERNAL READ ',
     $ 'PROBLEM'/6X,'- EITHER ORIGINAL PACKING OF FILE OR TRANSFER ',
     $ 'OF FILE HAS RESULTED IN UNPROCESSABLE INFORMATION'/6X,
     $ '- WILL ATTEMPT TO MOVE AHEAD TO NEXT REPORT, IF NOT POSSIBLE ',
     $ 'WILL EXIT RECORD WITH NEXT = -2'/)
  105 FORMAT(/' *** W3FI64 ERROR- REPORT: ',A4,A2,'; ACTUAL NO. 10-CHAR'
     $,' WORDS IN CAT. ',A2,',',I10,' .NE. TO VALUE READ IN WITH ',
     $ 'REPORT:',I10/6X,
     $ '- MAY BE DUE TO INTERNAL READ PROBLEM ASSOC. W/ EITHER ORIG. ',
     $ 'PACKING OR TRANSFER OF FILE RESULTING IN UNPROCESSABLE INFO.'/6X
     $,'- WILL ATTEMPT TO MOVE AHEAD TO NEXT REPORT, IF NOT POSSIBLE ',
     $ 'WILL EXIT RECORD WITH NEXT = -3'/)
  106 FORMAT(/' +++ IT WAS POSSIBLE TO MOVE TO NEXT REPORT IN THIS ',
     $ 'RECORD -- CONTINUE WITH THE UNPACKING OF THIS NEW REPORT'/)
  107 FORMAT(/' *** IT WAS NOT POSSIBLE TO MOVE TO NEXT REPORT IN THIS',
     $ ' RECORD -- MUST EXIT THIS RECORD WITH NEXT =',I3/)
  108 FORMAT(/' *** W3FI64 ERROR- REPORT: ',A4,A2,'; AN INPUT ',
     $ 'PARAMETER CONSISTS OF MORE THAN SEVEN CHARACTERS'/6X,
     $ '- MAY BE DUE TO INTERNAL READ PROBLEM ASSOC. W/ EITHER ORIG. ',
     $ 'PACKING OR TRANSFER OF FILE RESULTING IN UNPROCESSABLE INFO.'/6X
     $,'- WILL ATTEMPT TO MOVE AHEAD TO NEXT REPORT, IF NOT POSSIBLE ',
     $ 'WILL EXIT RECORD WITH NEXT = -2'/)
  109 FORMAT(/' *** W3FI64 ERROR- RECORD ',I4,' DOES NOT END WITH ',
     $ '"END RECORD" BUT INSTEAD CONTAINS "X" FILLERS AFTER LAST ',
     $ 'REPORT IN RECORD'/6X,'- WILL EXIT RECORD WITH NEXT = -1, NO ',
     $ 'REPORTS SHOULD BE LOST'/)
      END
