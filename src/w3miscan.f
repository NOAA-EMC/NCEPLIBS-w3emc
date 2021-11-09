C> @file
C> @brief Reads 1 ssm/i scan line from bufr d-set
C> @author Dennis Keyser @date 1996-07-30

C> Reads one ssm/i scan line (64 retrievals) from the NCEP
C> bufr ssm/i dump file. Each scan is time checked against the
C> user-requested time window and satellite id combinations. When a
C> valid scan is read the program returns to the calling program.
C> the user must pass in the type of the input ssm/i dump file,
C> either derived products (regardless of source) or brightness
C> temperatures (7-channels). If the latter is chosen, the user
C> has the further option of processing, in addition to the
C> brightness temperatures, in-line calculation of wind speed
C> product via the goodberlet algorithm, and/or in-line calculation
C> of both wind speed and total column precipitable water (tpw)
C> products using the neural net 3 algorithm. If the wind speed
C> or tpw is calculated here (either algorithm), this subroutine
C> will check for brightness temperatures outside of a preset range
C> and will return a missing wind speed/tpw if any b. temp is
C> unreasonable. Also, for calculated wind speeds and tpw, this
C> program will check to see if the b. temps are over land or ice,
C> and if they are it will also return missing values since these
C> data are valid only over ocean.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1996-07-30 | Dennis Keyser | Original author - subroutine is a modified version of w3lib w3fi86 which read one scan line from the 30-orbit shared processing data sets
C> 1997-05-22 | Dennis Keyser | Crisis fix to account for clon now returned from bufr as -180 to 0 (west) or 0 to 180 (east), used to return as 0 to 360 east which was not the bufr standard
C> 1998-01-28 | Dennis Keyser | Replaced neural net 2 algorithm which calculated only wind speed product with neural net 3 algorithm which calculates both wind speed and total precipitable water products (among others) but, unlike nn2, does not return a rain flag value (it does set all retrievals to missing that fail rain flag and ice contamination tests)
C> 1998-03-30 | Dennis Keyser | Modified to handle neural net 3 ssm/i products input in a products bufr data dump file; now prints out number of scans processed by satellite number in final summary
C> 1998-10-23 | Dennis Keyser | Subroutine now y2k and fortran 90 compliant
C> 1999-02-18 | Dennis Keyser | Modified to compile and run properly on ibm-sp
C> 2000-06-08 | Dennis Keyser | Corrected mnemonic for rain rate to "reqv" (was "prer" for some unknown reason)
C> 2001-01-03 | Dennis Keyser | Changed units of returned rain rate from whole mm/hr to 10**6 mm/sec, changed units of returned surface temp from whole kelvin to 10**2 kelvin (to incr. precision to that orig. in input bufr file)
C> 2004-09-12 | Dennis Keyser | Now decodes sea-surface temperature if valid into same location as surface temperature, quantity is surface temperature if surface tag is not 5, otherwise quantity is sea-surface temperature (ncep products data dump file now contains sst); checks to see if old or new version of mnemonic table bufrtab.012 is being used here (old version had "ph2o" instead of "tpwt", "sndp" instead of "tosd", "wsos" instead of "wspd" and "ch2o" instead of the sequence "metfet vilwc metfet"), and decodes using whichever mnemonics are found {note: a further requirement for "vilwc" is that the first "metfet" (meteorological feature) in the sequence must be 12 (=cloud), else cloud water set to missing, regardless of "vilwc" value}
C> 2011-08-04 | Dennis Keyser | Add ibdate (input bufr message date) to output argument list (now used by calling program prepobs_prepssmi)
C>
C> @param[in] INDTA Unit number of ncep bufr ssm/i dump data set
C> @param[in] INLSF Unit number of direct access nesdis land/sea file
C> (valid only if lbrit and either nnalg or gbalg true).
C> @param[in] INGBI Unit number of grib index file for grib file
C> Containing global 1-degree sea-surface temp field.
C> (valid only if lbrit and either nnalg or gbalg true).
C> @param[in] INGBD Unit number of grib file containing global 1-degree
C> Sea-surface temp field (valid only if lbrit and either.
C> Nnalg or gbalg true).
C> @param[in] LSAT 10-word logical array (240:249) indicating which
C> Satellite ids should be processed (see remarks)
C> @param[in] LPROD Logical indicating if the input bufr file contains
C> Products (regardless of source) - in this case one or.
C> More available products can be processed and returned.
C> @param[in] LBRIT Logical indicating if the input bufr file contains
C> Brightness temperatures - in this case b. temps are.
C> Processed and returned along with, if requested, in-.
C> Line generated products from one or both algorithms.
C> (see next two switches).
C> - The following two switches apply only if lbrit is true -----
C> @param[in] NNALG Indicating if the subroutine should
C> calculate and return ssm/i wind speed and tpw
C> via the neural net 3 algorithm (note: b o t h
C> wind speed and tpw are returned here)
C> @param[in] GBALG Indicating if the subroutine should
C> calculate and return ssm/i wind speed via the
C> goodberlet algorithm
C> @param[in] KDATE Requested earliest year(yyyy), month, day, hour,
C> Min for accepting scans.
C> @param[in] LDATE Requested latest   year(yyyy), month, day, hour,
C> Min for accepting scans.
C> @param[in] IGNRTM Switch to indicate whether scans should be time-
C> Checked (= 0) or not time checked (=1) {if =1, all.
C> Scans read in are processed regardless of their time..
C> The input arguments "kdate" and "ldate" (earliest and.
C> Latest date for processing data) are ignored in the.
C> Time checking for scans. (note: the earliest and.
C> Latest dates should still be specified to the.
C> "expected" time range, but they will not be used for.
C> Time checking in this case)}.
C> @param[out] IBUFTN Output buffer holding data for a scan (1737 words -
C> See remarks for format. some words may be missing
C> Depending upon lprod, lbrit, nnalg and gbalg
C> @param[out] IBDATE Input bufr message section 1 date (yyyymmddhh)
C> @param[out] IER Error return code (see remarks)
C>
C> @remark
C> Return code ier can have the following values:
C> - IER = 0  Successful return of scan
C> - IER = 1  All scans have been read, all done
C> - IER = 2  Abnormal return - input bufr file in unit
C> 'indta' is either empty (null) or is not bufr
C> - IER = 3  Abnormal return - requested earliest and
C> latest dates are backwards
C> - IER = 4  Abnormal return - error opening random
C> access file holding land/sea tags
C> - IER = 5  Abnormal return - the number of decoded
C> "levels" is not what is expected
C> - IER = 6  Abnormal return - sea-surface temperature
C> not found in grib index file - error returned
C> from grib decoder getgb is 96
C> - IER = 7  Abnormal return - sea-surface temperature
C> grib message has a date that is either:
C> 1) more than 7-days prior to the earliest
C> requested date or 2) more than 7-days after
C> the latest requested date
C> - IER = 8  Abnormal return - byte-addressable read error
C> for grib file containing sea-surface
C> temperature field - error returned from grib
C> decoder getgb is 97-99
C> - IER = 9  Abnormal return - error returned from grib
C> decoder - getgb - for sea-surface
C> temperature field - > 0 but not 96-99
C>
C> Input argument lsat is set-up as follows:
C> - LSAT(X) = TRUE -- Process scans from satellite id x (where x is code figure from bufr code table 0-01-007)
C> - LSAT(X) = FALSE - Do not process scans from satellite id x
C>  - X = 240 is f-7  dmsp satellite (this satellite is no longer available)
C>  - X = 241 is f-8  dmsp satellite (this satellite is no longer available)
C>  - X = 242 is f-9  dmsp satellite (this satellite is no longer available)
C>  - X = 243 is f-10 dmsp satellite (this satellite is no longer available)
C>  - X = 244 is f-11 dmsp satellite (this is available as of 8/96 but is not considered to be an operational dmsp ssm/i satellite)
C>  - X = 245 is f-12 dmsp satellite (this satellite is no longer available)
C>  - X = 246 is f-13 dmsp satellite (this is available and is considered to be an operational odd dmsp ssm/i satellite as of 8/1996)
C>  - X = 247 is f-14 dmsp satellite (this is available as of 5/97 but is not considered to be an operational dmsp ssm/i satellite)
C>  - X = 248 is f-15 dmsp satellite (this is available as of 2/2000 and is considered to be an operational odd dmsp ssm/i satellite as of 2/2000)
C>  - X = 249 is reserved for a future dmsp satellite
C>
C> @note Here "even" means value in ibuftn(1) is an odd number while "odd" means value in ibuftn(1) is an even number
C> Contents of array 'ibuftn' holding one complete scan (64 individual retrievlas (1737 words)
C>
C> #### Always returned:
C> WORD |  CONTENTS
C> ---- |  --------
C>   1  |  Satellite id (244 is f-11; 246 is f-13; 247 is f-14; 248 is f-15)
C>   2  |  4-digit year for scan
C>   3  |  2-digit month of year for scan
C>   4  |  2-digit day of month for scan
C>   5  |  2-digit hour of day for scan
C>   6  |  2-digit minute of hour for scan
C>   7  |  2-digit second of minute for scan
C>   8  |  Scan number in orbit
C>   9  |  Orbit number for scan
C>  10  |  Retrieval #1 latitude  (*100 degrees: + n, - s)
C>  11  |  Retrieval #1 longitude (*100 degrees east)
C>  12  |  Retrieval #1 position number
C>  13  |  Retrieval #1 surface tag (code figure)
C>
C> #### For LPROD = TRUE {Input products file: note all products below except sea-surface temperature are available in the fnoc "operational" products data dump; most ncep products data dumps contain only wind speed, total precipitable water, cloud water and sea-surface temperature (all over ocean only)}:
C> WORD |  CONTENTS
C> ---- |  --------
C> 14  |  Retrieval #1 cloud water (*100 kilogram/meter**2)
C> 15  |  Retrieval #1 rain rate (*1000000 millimeters/second)
C> 16  |  Retrieval #1 wind speed (*10 meters/second)
C> 17  |  Retrieval #1 soil moisture (millimeters)
C> 18  |  Retrieval #1 sea-ice concentration (per cent)
C> 19  |  Retrieval #1 sea-ice age (code figure)
C> 20  |  Retrieval #1 ice edge (code figure)
C> 21  |  Retrieval #1 total precip. water (*10 millimeters)
C> 22  |  Retrieval #1 surface temp (*100 k) if not over ocean -OR-
C> 22  |  Retrieval #1 sea-surface temp (*100 k) if over ocean
C> 23  |  Retrieval #1 snow depth (millimeters)
C> 24  |  Retrieval #1 rain flag (code figure)
C> 25  |  Retrieval #1 calculated surface type (code figure)
C>
C> #### For LBRIT = TRUE (Input brightness temperature file):
C> WORD |  CONTENTS
C> ---- |  --------
C> 26  |  Retrieval #1 19 ghz v brightness temp (*100 deg. k)
C> 27  |  Retrieval #1 19 ghz h brightness temp (*100 deg. k)
C> 28  |  Retrieval #1 22 ghz v brightness temp (*100 deg. k)
C> 29  |  Retrieval #1 37 ghz v brightness temp (*100 deg. k)
C> 30  |  Retrieval #1 37 ghz h brightness temp (*100 deg. k)
C> 31  |  Retrieval #1 85 ghz v brightness temp (*100 deg. k)
C> 32  |  Retrieval #1 85 ghz h brightness temp (*100 deg. k)
C>
C> #### For LBRIT = TRUE and NNALG = TRUE (Input brightness temperature file):
C> WORD |  CONTENTS
C> ---- |  --------
C> 33  |  Retrieval #1 Neural net 3 algorithm wind speed (generated in-line) (*10 meters/second)
C> 34  |  Retrieval #1 Neural net 3 algorithm total precip. water (generated in-line) (*10 millimeters)
C>
C> #### For LBRIT = TRUE and GBALG = TRUE (Input brightness temperature file):
C> WORD |  CONTENTS
C> ---- |  --------
C> 35  |  Retrieval #1 goodberlet algorithm wind speed (generated in-line) (*10 meters/second)
C> 36  |  Retrieval #1 goodberlet algorithm rain flag (code figure)
C> 37-1737 | Repeat 10-36 for 63 more retrievals
C>
C> @note All missing data or data not selected by calling program are set to 99999
C>
C> @author Dennis Keyser @date 1996-07-30

      SUBROUTINE W3MISCAN(INDTA,INLSF,INGBI,INGBD,LSAT,LPROD,LBRIT,
     $ NNALG,GBALG,KDATE,LDATE,IGNRTM,IBUFTN,IBDATE,IER)

      LOGICAL  LPROD,LBRIT,NNALG,GBALG,LSAT(240:249)

      CHARACTER*1  CDUMMY
      CHARACTER*2  ATXT(2)
      CHARACTER*8  SUBSET
      CHARACTER*20 RHDER,PROD2,BRITE
      CHARACTER*46 SHDER,PROD1

      REAL  SHDR(9),RHDR(4,64),PROD(13,64),BRIT(2,448),RINC(5),
     $ METFET(64)

      REAL(8)  SHDR_8(9),RHDR_8(4,64),PROD_8(13,64),BRIT_8(2,448),
     $ UFBINT_8(64)

      INTEGER  IBUFTN(1737),KDATA(7),KDATE(5),LDATE(5),LBTER(7),
     $ KSPSAT(239:249),KNTSAT(239:249),IFLAG(64),KDAT(8),LDAT(8),
     $ MDAT(8),ICDATE(5),IDDATE(5)

      COMMON/MISCCC/SSTDAT(360,180)
      COMMON/MISCEE/LFLAG,LICEC

      SAVE

      DATA  SHDER /'SAID  YEAR MNTH DAYS HOUR MINU SECO SCNN ORBN '/
      DATA  RHDER /'CLAT CLON POSN SFTG '/
      DATA  PROD1 /'VILWC REQV WSPD SMOI ICON ICAG ICED TPWT TMSK '/
      DATA  PROD2 /'TOSD RFLG SFTP SST1 '/
      DATA  BRITE /'CHNM TMBR           '/
      DATA  ATXT  /'NN','GB'/
      DATA  IMSG  /99999/,KNTSCN/0/,KNTTIM/0/,LAERR/0/,
     $ LOERR/0/,LBTER/7*0/,ITIMES/0/,NLR/0/,NIR/0/,DMAX/-99999./,
     $ DMIN/99999./,KSPSAT/11*0/,KNTSAT/11*0/,ILFLG/0/,BMISS/10.0E10/

      IF(ITIMES.EQ.0)  THEN

C***********************************************************************
C FIRST CALL INTO SUBROUTINE DO A FEW THINGS .....
         ITIMES = 1
         LFLAG = 0
         LICEC = 0
         PRINT 65, INDTA
   65    FORMAT(//' ---> W3MISCAN: Y2K/F90 VERSION 08/04/2011: ',
     $    'PROCESSING SSM/I DATA FROM BUFR DATA SET READ FROM UNIT ',
     $    I4/)
         IF(LPROD)  PRINT 66
   66    FORMAT(//'  ===> WILL READ FROM BUFR PRODUCTS DATA DUMP ',
     $    'FILE (EITHER FNOC OR NCEP) AND PROCESS ONE OR MORE SSM/I ',
     $    'PRODUCTS'//)
         IF(LBRIT)  THEN
            PRINT 167
  167       FORMAT(//'  ===> WILL READ FROM BUFR BRIGHTNESS ',
     $       'TEMPERATURE DATA DUMP FILE AND PROCESS BRIGHTNESS ',
     $       'TEMPERATURES'//)
            IF(NNALG)  PRINT 169
  169       FORMAT('  ===>  IN ADDITION, WILL PERFORM IN-LINE ',
     $       'CALCULATION OF NEURAL NETWORK 3 WIND SPEED AND TOTAL ',
     $       'PRECIPITABLE WATER AND PROCESS THESE'/)
            IF(GBALG)  PRINT 170
  170       FORMAT('  ===>  IN ADDITION, WILL PERFORM IN-LINE ',
     $       'CALCULATION OF GOODBERLET WIND SPEED AND PROCESS THESE'/)
         END IF
         IF(IGNRTM.EQ.1)  PRINT 704
  704    FORMAT(' W3MISCAN: INPUT ARGUMENT "IGNRTM" IS SET TO 1 -- NO ',
     $    'TIME CHECKS WILL BE PERFORMED ON SCANS - ALL SCANS READ IN ',
     $    'ARE PROCESSED'/)

         PRINT 104, KDATE,LDATE
  104    FORMAT(' W3MISCAN: REQUESTED EARLIEST DATE:',I7,4I5/
     $          '           REQUESTED LATEST   DATE:',I7,4I5)

         KDAT = 0
         KDAT(1:3) = KDATE(1:3)
         KDAT(5:6) = KDATE(4:5)
         LDAT = 0
         LDAT(1:3) = LDATE(1:3)
         LDAT(5:6) = LDATE(4:5)

C DO REQUESTED EARLIEST AND LATEST DATES MAKE SENSE?

         CALL W3DIFDAT(LDAT,KDAT,3,RINC)
         IF(RINC(3).LT.0)  THEN
C.......................................................................
            PRINT 103
  103       FORMAT(' ##W3MISCAN: REQUESTED EARLIEST AND LATEST DATES ',
     $       'ARE BACKWARDS!! - IER = 3'/)
            IER = 3
            RETURN
C.......................................................................
         END IF

C DETERMINE MACHINE WORD LENGTH IN BYTES AND TYPE OF CHARACTER SET
C  {ASCII(ICHTP=0) OR EBCDIC(ICHTP=1)}

         CALL W3FI04(IENDN,ICHTP,LW)
         PRINT 2213, LW, ICHTP, IENDN
 2213    FORMAT(/' ---> W3MISCAN: CALL TO W3FI04 RETURNS: LW = ',I3,
     $    ', ICHTP = ',I3,', IENDN = ',I3/)

         CALL DATELEN(10)

         CALL DUMPBF(INDTA,ICDATE,IDDATE)
cppppp
         print *,'CENTER DATE (ICDATE) = ',icdate
         print *,'DUMP DATE (IDDATE)   = ',iddate
cppppp

C COME HERE IF CENTER DATE COULD NOT BE READ FROM FIRST DUMMY MESSAGE
C  - RETURN WITH IRET = 2

         IF(ICDATE(1).LE.0)  GO TO 998

C COME HERE IF DUMP DATE COULD NOT BE READ FROM SECOND DUMMY MESSAGE
C  - RETURN WITH IRET = 2

         IF(IDDATE(1).LE.0)  GO TO 998
         IF(ICDATE(1).LT.100)  THEN

C IF 2-DIGIT YEAR RETURNED IN ICDATE(1), MUST USE "WINDOWING" TECHNIQUE
C  TO CREATE A 4-DIGIT YEAR

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

            PRINT *, '##W3MISCAN - THE FOLLOWING SHOULD NEVER ',
     $       'HAPPEN!!!!!'
            PRINT *, '##W3MISCAN - 2-DIGIT YEAR IN ICDATE(1) RETURNED ',
     $       'FROM DUMPBF (ICDATE IS: ',ICDATE,') - USE WINDOWING ',
     $       'TECHNIQUE TO OBTAIN 4-DIGIT YEAR'
            IF(ICDATE(1).GT.20)  THEN
               ICDATE(1) = 1900 + ICDATE(1)
            ELSE
               ICDATE(1) = 2000 + ICDATE(1)
            ENDIF
            PRINT *, '##W3MISCAN - CORRECTED ICDATE(1) WITH 4-DIGIT ',
     $       'YEAR, ICDATE NOW IS: ',ICDATE
         ENDIF

         IF(IDDATE(1).LT.100)  THEN

C IF 2-DIGIT YEAR RETURNED IN IDDATE(1), MUST USE "WINDOWING" TECHNIQUE
C  TO CREATE A 4-DIGIT YEAR

C IMPORTANT: IF DATELEN(10) IS CALLED, THE DATE HERE SHOULD ALWAYS
C            CONTAIN A 4-DIGIT YEAR, EVEN IF INPUT FILE IS NOT
C            Y2K COMPLIANT (BUFRLIB DOES THE WINDOWING HERE)

            PRINT *, '##W3MISCAN - THE FOLLOWING SHOULD NEVER ',
     $       'HAPPEN!!!!!'
            PRINT *, '##W3MISCAN - 2-DIGIT YEAR IN IDDATE(1) RETURNED ',
     $       'FROM DUMPBF (IDDATE IS: ',IDDATE,') - USE WINDOWING ',
     $       'TECHNIQUE TO OBTAIN 4-DIGIT YEAR'
            IF(IDDATE(1).GT.20)  THEN
               IDDATE(1) = 1900 + IDDATE(1)
            ELSE
               IDDATE(1) = 2000 + IDDATE(1)
            ENDIF
            PRINT *, '##W3MISCAN - CORRECTED IDDATE(1) WITH 4-DIGIT ',
     $       'YEAR, IDDATE NOW IS: ',IDDATE
         END IF

C  OPEN BUFR FILE - READ IN DICTIONARY MESSAGES (TABLE A, B, D ENTRIES)

         CALL OPENBF(INDTA,'IN',INDTA)

         print *, '                 '
         print *, 'OPEN NCEP BUFR SSM/I DUMP FILE'
         print *, '                 '

C  Check to see if the old (pre 9/2004) version of the mnemonic
C   table is being used here (had "PH2O" instead of "TPWT",
C   "SNDP" instead of "TOSD", "WSOS" instead of "WSPD")
C  ------------------------------------------------------------

         CALL STATUS(INDTA,LUN,IDUMMY1,IDUMMY2)
         CALL NEMTAB(LUN,'PH2O',IDUMMY1,CDUMMY,IRET_PH2O)
         CALL NEMTAB(LUN,'SNDP',IDUMMY1,CDUMMY,IRET_SNDP)
         CALL NEMTAB(LUN,'WSOS',IDUMMY1,CDUMMY,IRET_WSOS)
         CALL NEMTAB(LUN,'CH2O',IDUMMY1,CDUMMY,IRET_CH2O)

         IF(LBRIT.AND.(NNALG.OR.GBALG))  THEN

C-----------------------------------------------------------------------
C         IF IN-LINE CALC. OF WIND SPEED FROM GOODBERLET ALG. OR
C    IN-LINE CALCULATION OF WIND SPEED AND TPW FROM NEURAL NET 3 ALG.
C  FIRST CALL TO THIS SUBROUTINE WILL READ IN SEA-SURFACE TEMPERATURE
C                    FIELD AS A CHECK FOR ICE LIMITS
C            WILL ALSO OPEN DIRECT ACCESS NESDIS LAND SEA FILE
C-----------------------------------------------------------------------

            CALL MISC06(INGBI,INGBD,KDATE,LDATE,*993,*994,*995,*996)
            PRINT 67, INLSF
   67       FORMAT(//4X,'** W3MISCAN: OPEN R. ACCESS NESDIS LAND/SEA ',
     $       'FILE IN UNIT ',I2/)
         OPEN(UNIT=INLSF,ERR=997,ACCESS='DIRECT',IOSTAT=IERR,RECL=10980)
         END IF

C READ THE FIRST BUFR MESSAGE IN THE BUFR FILE

         CALL READMG(INDTA,SUBSET,IBDATE,IRET)

         print *, 'READ FIRST BUFR MESSAGE: SUBSET = ',SUBSET,
     $    '; IBDATE = ',IBDATE,'; IRET = ',IRET

         IF(IRET.NE.0)  GO TO 998

C***********************************************************************

      END IF

   30 CONTINUE

C TIME TO DECODE NEXT SUBSET (SCAN) OUT OF BUFR MESSAGE

      IBUFTN = IMSG
      CALL READSB(INDTA,IRET)
      IF(IRET.NE.0)  THEN

C ALL SUBSETS OUT OF THIS MESSAGE READ, TIME TO MOVE ON TO NEXT MESSAGE

         CALL READMG(INDTA,SUBSET,IBDATE,IRET)

         print *, 'READ NEXT BUFR MESSAGE: SUBSET = ',SUBSET,
     $    '; IBDATE = ',IBDATE,'; IRET = ',IRET

         IF(IRET.NE.0)  THEN
c.......................................................................

C NON-ZERO IRET IN READMG MEANS ALL BUFR MESSAGES IN FILE HAVE BEEN READ
C  - ALL FINISHED, NO OTHER SCANS W/I DESIRED TIME RANGE -- SET IER TO 1
C  AND RETURN TO CALLING PROGRAM

            PRINT 124, KNTSCN
  124       FORMAT(/' W3MISCAN: +++++ ALL VALID SCANS UNPACKED AND ',
     $       'RETURNED FROM THIS NCEP BUFR SSM/I DUMP FILE'//34X,
     $       '** W3MISCAN: SUMMARY **'//35X,'TOTAL NUMBER OF SCANS ',
     $       'PROCESSED AND RETURNED',11X,I7)
            DO JJ = 239,249
               IF(KNTSAT(JJ).GT.0)  THEN
                  PRINT  294, JJ,KNTSAT(JJ)
  294             FORMAT(35X,'......NO. OF SCANS PROCESSED AND ',
     $             'RETURNED FROM SAT',I4,':',I7)
               END IF
            END DO
            DO JJ = 239,249
               IF(KSPSAT(JJ).GT.0)  THEN
                  II = JJ
                  IF(JJ.EQ.239)  II = 1
                  PRINT  224, II,KSPSAT(JJ)
  224             FORMAT(35X,'NO. OF SCANS SKIPPED DUE TO BEING FROM ',
     $             'NON-REQ SAT',I4,':',I7)
               END IF
            END DO
            PRINT 194, KNTTIM
  194       FORMAT(35X,'NUMBER OF SCANS SKIPPED DUE TO BEING OUTSIDE ',
     $       'TIME INT.:',I7)
            PRINT 324, LAERR,LOERR
  324 FORMAT(
     $/35X,'NUMBER OF RETRIEVALS WITH LATITUDE OUT OF RANGE:       ',I7/
     $ 35X,'NUMBER OF RETRIEVALS WITH LONGITUDE OUT OF RANGE:      ',I7)
            IF(LBRIT)  THEN
               IF(NNALG.OR.GBALG)  PRINT 780, LBTER,NLR,NIR
  780 FORMAT(
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 19 GHZ V BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 19 GHZ H BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 22 GHZ V BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 37 GHZ V BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 37 GHZ H BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 85 GHZ V BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS W/ ERROR IN 85 GHZ H BRIGHT. TEMP:',I7/
     $ 35X,'NUMBER OF RETRIEVALS REJECTED DUE TO BEING OVER LAND:  ',I7/
     $ 35X,'NUMBER OF RETRIEVALS REJECTED DUE TO BEING OVER ICE:   ',I7)
               IF(NNALG)  PRINT 781, LFLAG,LICEC
  781 FORMAT(
     $ 35X,'NUMBER OF NN3 RETR. REJECTED DUE TO FAILING RAIN FLAG: ',I7/
     $ 35X,'NUMBER OF NN3 RETR. REJECTED DUE TO ICE CONTAMINATION: ',I7)
               IF(NNALG.OR.GBALG)  PRINT 782, DMAX,DMIN
  782 FORMAT(/'  ** FOR SEA-SFC TEMP AT ALL RETRIEVAL LOCATIONS: FIELD',
     $ ' MAX =',F8.3,' DEG K, FIELD MIN =',F8.3,' DEG K'/)
            END IF
            IER = 1
            RETURN
C.......................................................................
         END IF

         GO TO 30
      END IF

C***********************************************************************
C         COME HERE FOR BOTH PRODUCTS AND BRIGHTNESS TEMPERATURES
C***********************************************************************
      SHDR = BMISS
      CALL UFBINT(INDTA,SHDR_8,09,1,NLEV,SHDER) ; SHDR = SHDR_8
      ILFLG = 1
      IF(NLEV.NE.1)  GO TO 999

C STORE THE SCAN'S SATELLITE ID IN WORD 1
C STORE SCAN'S YEAR (YYYY), MONTH, DAY, HOUR, MIN, SEC INTO WORDS 2-7
C STORE THE SCAN NUMBER IN WORD 8
C STORE THE SCAN'S ORBIT NUMBER IN WORD 9

      IBUFTN(1:9) = MIN(IMSG,NINT(SHDR(1:9)))

C CHECK TO SEE IF SCAN IS FROM REQUESTED SATELLITE ID

      IF(IBUFTN(1).LT.240.OR.IBUFTN(1).GT.249) THEN
         PRINT 523, (IBUFTN(II),II=1,9)
         KSPSAT(239) = KSPSAT(239) + 1
         GO TO 30
      END IF
      IF(.NOT.LSAT(IBUFTN(1))) THEN
CDAK     PRINT 523, (IBUFTN(II),II=1,9)
  523    FORMAT(' ##W3MISCAN: SCAN NOT FROM REQ. SAT. ID -SAT. ID',I4,
     $   ', SCAN TIME:',6I4,', SCAN',I6,', ORBIT',I8,'-GO TO NEXT SCAN')
         KSPSAT(IBUFTN(1)) = KSPSAT(IBUFTN(1)) + 1
         GO TO 30
      END IF

      IF(IGNRTM.EQ.0)  THEN

C TIME CHECK THIS SCAN IF USER REQUESTS SUCH

         MDAT = 0
         MDAT(1:3) = IBUFTN(2:4)
         MDAT(5:7) = IBUFTN(5:7)
         CALL W3DIFDAT(KDAT,MDAT,4,RINC)
         KSEC = RINC(4)
         CALL W3DIFDAT(LDAT,MDAT,4,RINC)
         LSEC = RINC(4)
         IF(KSEC.GT.0.OR.LSEC.LT.0)  THEN

C TIME CHECK FOR SCAN FAILED: GO ON TO NEXT SCAN

CDAK        PRINT 123, (IBUFTN(II),II=2,9)
  123       FORMAT(' ##W3MISCAN: SCAN NOT IN REQUESTED TIME WINDOW-',
     $    'SCAN TIME:',6I5,' SCAN',I6,', ORBIT',I8,' - GO TO NEXT SCAN')
            KNTTIM = KNTTIM + 1
            GO TO 30
         END IF
      END IF
      RHDR = BMISS
      CALL UFBINT(INDTA,RHDR_8,04,64,NLEV,RHDER) ; RHDR = RHDR_8
      ILFLG = 2
      IF(NLEV.NE.64)  GO TO 999
      IFLAG = 0
      DO IRT = 1,64

C THIS ROUTINE EXPECTS LONGITUDE TO BE 0-360 E; BUFR NOW RETURNS -180-0
C  FOR WEST AND 0-180 FOR EAST

         IF(RHDR(2,IRT).LT.0.0)  RHDR(2,IRT) = RHDR(2,IRT) + 360.
C-----------------------------------------------------------------------
C             LOOP THROUGH THE 64 RETRIEVALS IN A SCAN
C-----------------------------------------------------------------------
C STORE THE LATITUDE (*100 DEGREES; + : NORTH, - : SOUTH)
         IF(NINT(RHDR(1,IRT)*100.).GE.-9000.AND.NINT(RHDR(1,IRT)*100.)
     $    .LE.9000)  THEN
            IBUFTN((27*IRT)-17) = NINT(RHDR(1,IRT)*100.)
         ELSE

C.......................................................................

C BAD LATITUDE

            LAERR = LAERR + 1
            PRINT 777, IRT,IBUFTN(8),IBUFTN(9),NINT(RHDR(1,IRT)*100.)
  777       FORMAT(' ##W3MISCAN: BAD LAT: RETR.',I3,', SCAN',I6,
     $       ', ORBIT',I8,'; INPUT LAT=',I7,' - ALL DATA IN THIS ',
     $       'RETRIEVAL SET TO MISSING')
            IFLAG(IRT) = 1
C.......................................................................

         END IF

C STORE THE LONGITUDE (*100 DEGREES EAST)

         IF(NINT(RHDR(2,IRT)*100.).GE.0.AND.NINT(RHDR(2,IRT)*100.).LE.
     $    36000)  THEN
            IF(IFLAG(IRT).EQ.0)
     $       IBUFTN((27*IRT)-16) = NINT(RHDR(2,IRT)*100.)
         ELSE

C.......................................................................

C BAD LONGITUDE

            LOERR = LOERR + 1
            PRINT 778, IRT,IBUFTN(8),IBUFTN(9),NINT(RHDR(2,IRT)*100.)
  778       FORMAT(' ##W3MISCAN: BAD LON: RETR.',I3,', SCAN',I6,
     $       ', ORBIT',I8,'; INPUT LON=',I7,' - ALL DATA IN THIS ',
     $       'RETRIEVAL SET TO MISSING')
            IFLAG(IRT) = 1
C.......................................................................

         END IF
         IF(IFLAG(IRT).NE.0)  GO TO 110

C STORE THE POSITION NUMBER

         IBUFTN((27*IRT)-15) = MIN(IMSG,NINT(RHDR(3,IRT)))

C STORE THE SURFACE TAG (0-6)

         IBUFTN((27*IRT)-14) = MIN(IMSG,NINT(RHDR(4,IRT)))
  110    CONTINUE
C-----------------------------------------------------------------------
      END DO

      IF(LPROD)  THEN
C***********************************************************************
C      COME HERE TO PROCESS PRODUCTS FROM INPUT SSM/I PRODUCTS FILE
C***********************************************************************

         PROD = BMISS
         CALL UFBINT(INDTA,PROD_8,13,64,NLEV,PROD1//PROD2)
         UFBINT_8 = BMISS
         IF(IRET_PH2O.GT.0) THEN  ! Prior to 9/2004
            CALL UFBINT(INDTA,UFBINT_8,1,64,NLEV,'PH2O')
            PROD_8(8,:) = UFBINT_8(:)
         END IF
         UFBINT_8 = BMISS
         IF(IRET_SNDP.GT.0) THEN  ! Prior to 9/2004
            CALL UFBINT(INDTA,UFBINT_8,1,64,NLEV,'SNDP')
            PROD_8(10,:) = UFBINT_8(:)
         END IF
         UFBINT_8 = BMISS
         IF(IRET_WSOS.GT.0) THEN  ! Prior to 9/2004
            CALL UFBINT(INDTA,UFBINT_8,1,64,NLEV,'WSOS')
            PROD_8(3,:) = UFBINT_8(:)
         END IF
         UFBINT_8 = BMISS
         IF(IRET_CH2O.GT.0) THEN  ! Prior to 9/2004
            CALL UFBINT(INDTA,UFBINT_8,1,64,NLEV,'CH2O')
            PROD_8(1,:) = UFBINT_8(:)
         ELSE
            CALL UFBINT(INDTA,UFBINT_8,1,64,NLEV,'METFET')
            METFET = UFBINT_8
            DO IRT = 1,64
               IF(NINT(METFET(IRT)).NE.12)  PROD_8(1,IRT) = BMISS
            END DO
         END IF

         PROD=PROD_8
         ILFLG = 3
         IF(NLEV.EQ.0)  THEN
            PRINT 797, IBUFTN(8),IBUFTN(9)
  797       FORMAT(' ##W3MISCAN: PRODUCTS REQ. BUT SCAN',I6,', ORBIT',
     $       I8,' DOES NOT CONTAIN PRODUCT DATA - CONTINUE PROCESSING ',
     $       'SCAN (B.TEMPS REQ.?)')
            GO TO 900
         ELSE  IF(NLEV.NE.64)  THEN
            GO TO 999
         END IF
         DO IRT = 1,64
C-----------------------------------------------------------------------
C             LOOP THROUGH THE 64 RETRIEVALS IN A SCAN
C-----------------------------------------------------------------------
            IF(IFLAG(IRT).NE.0)  GO TO 111

C STORE THE CLOUD WATER (*100 KG/M**2) IF AVAILABLE

            IF(NINT(PROD(01,IRT)).LT.IMSG)
     $       IBUFTN((27*IRT)-13) = NINT(PROD(01,IRT)*100.)

C STORE THE RAIN RATE (*1000000 KG/((M**2)*SEC)) IF AVAILABLE
C  (THIS IS ALSO RAIN RATE (*1000000 MM/SEC))

            IF(NINT(PROD(02,IRT)).LT.IMSG)
     $       IBUFTN((27*IRT)-12) = NINT(PROD(02,IRT)*1000000.)

C STORE THE WIND SPEED (*10 M/SEC) IF AVAILABLE

            IBUFTN((27*IRT)-11) = MIN(IMSG,NINT(PROD(03,IRT)*10.))

C STORE THE SOIL MOISTURE (MM) IF AVAILABLE

            IF(NINT(PROD(04,IRT)).LT.IMSG)
     $       IBUFTN((27*IRT)-10) = NINT(PROD(04,IRT)*1000.)

C STORE THE SEA ICE CONCENTRATION (PERCENT) IF AVAILABLE

            IBUFTN((27*IRT)-09) = MIN(IMSG,NINT(PROD(05,IRT)))

C STORE THE SEA ICE AGE (0,1) IF AVAILABLE

            IBUFTN((27*IRT)-08) = MIN(IMSG,NINT(PROD(06,IRT)))

C STORE THE ICE EDGE (0,1) IF AVAILABLE

            IBUFTN((27*IRT)-07) = MIN(IMSG,NINT(PROD(07,IRT)))

C STORE THE WATER VAPOR (*10 KG/M**2) IF AVAILABLE
C  (THIS IS ALSO TOTAL PRECIPITABLE WATER SCALED AS *10 MM)

            IBUFTN((27*IRT)-06) = MIN(IMSG,NINT(PROD(08,IRT)*10.))

            IF(IBUFTN((27*IRT)-14).NE.5)  THEN

C STORE THE SURFACE TEMPERATURE (*100 DEGREES KELVIN) IF AVAILABLE
C  (NOTE: SURFACE TAG MUST NOT BE 5)

               IBUFTN((27*IRT)-05) = MIN(IMSG,NINT(PROD(09,IRT)*100.))

            ELSE

C STORE THE SEA-SURFACE TEMPERATURE (*100 DEGREES KELVIN) IF AVAILABLE
C  (NOTE: SURFACE TAG MUST BE 5)

               IBUFTN((27*IRT)-05) = MIN(IMSG,NINT(PROD(13,IRT)*100.))

            END IF

C STORE THE SNOW DEPTH (MM) IF AVAILABLE

            IF(NINT(PROD(10,IRT)).LT.IMSG)
     $       IBUFTN((27*IRT)-04) = NINT(PROD(10,IRT)*1000.)

C STORE THE RAIN FLAG (0-3) IF AVAILABLE

            IBUFTN((27*IRT)-03) = MIN(IMSG,NINT(PROD(11,IRT)))

C STORE THE CALCULATED SURFACE TYPE (1-20) IF AVAILABLE

            IBUFTN((27*IRT)-02) = MIN(IMSG,NINT(PROD(12,IRT)))
  111       CONTINUE
C-----------------------------------------------------------------------
         END DO
      END IF
  900 CONTINUE

      IF(LBRIT)  THEN
C***********************************************************************
C    COME HERE TO PROCESS BRIGHTNESS TEMPERATURES FROM INPUT SSM/I
C                   BRIGHTNESS TEMPERATURE FILE
C   AND POSSIBLY FOR IN-LINE CALC. OF WIND SPEED VIA GOODBERLET ALG.
C AND POSSIBLY FOR IN-LINE CALC. OF WIND SPEED AND TPW VIA N. NET 3 ALG.
C***********************************************************************

         BRIT = BMISS
         CALL UFBREP(INDTA,BRIT_8,2,448,NLEV,BRITE) ; BRIT = BRIT_8
         ILFLG = 4
         IF(NLEV.EQ.0)  THEN
            PRINT 798, IBUFTN(8),IBUFTN(9)
  798       FORMAT(' ##W3MISCAN: B. TEMPS REQ. BUT SCAN',I6,', ORBIT',
     $       I8,' DOES NOT CONTAIN B. TEMP DATA - DONE PROCESSING THIS',
     $       ' SCAN')
            GO TO 901
         ELSE  IF(NLEV.NE.448)  THEN
            GO TO 999
         END IF
         DO IRT = 1,64
C-----------------------------------------------------------------------
C             LOOP THROUGH THE 64 RETRIEVALS IN A SCAN
C-----------------------------------------------------------------------
            IF(IFLAG(IRT).NE.0)  GO TO 112

C STORE THE 7 BRIGHTNESS TEMPS (*100 DEGREES KELVIN)
C  -- CHANNELS ARE IN THIS ORDER FOR A PARTICULAR RETRIEVAL:
C  19 GHZ V, 19 GHZ H, 22 GHZ V, 37 GHZ V, 37 GHZ H, 85 GHZ V, 85 GHZ H

            IGOOD = 0
            MINDX = (IRT * 7) - 6
            DO LCH = MINDX,MINDX+6
               ICHNN = NINT(BRIT(1,LCH))
               IF(ICHNN.GT.7)  GO TO 79
               IF(NINT(BRIT(2,LCH)).LT.IMSG)  THEN
                  IBUFTN((27*IRT)-02+ICHNN) = NINT(BRIT(2,LCH)*100.)
                  IGOOD = 1
               END IF
   79          CONTINUE
            END DO

            IF(NNALG.OR.GBALG)  THEN
               KDATA = IMSG
               IF(IGOOD.EQ.1)  THEN
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COME HERE FOR IN-LINE CALC. OF WIND SPEED VIA GOODBERLET ALG. AND/OR
C      FOR IN-LINE CALC. OF WIND SPEED AND TPW VIA NEURAL NET 3 ALG.
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

C GET LAND/SEA TAG AND CHECK FOR LAT/LON OVER LAND OR ICE

             BALON=REAL(MOD(IBUFTN((27*IRT)-16)+18000,36000)-18000)/100.
                  IALON = MOD(36000-IBUFTN((27*IRT)-16),36000)
                  IX = 361. - REAL(IALON)/100.
                  JY = 91 - NINT(REAL(IBUFTN((27*IRT)-17))/100. + 0.50)
                  DMIN = MIN(DMIN,SSTDAT(IX,JY))
                  DMAX = MAX(DMAX,SSTDAT(IX,JY))
           CALL MISC04(INLSF,REAL(IBUFTN((27*IRT)-17))/100.,BALON,LSTAG)

C      ..... REJECT IF OVER LAND (USE LAND/SEA TAG HERE)

                  IF(LSTAG.NE.0)  THEN
                     NLR = NLR + 1
                     GO TO 112
                  END IF

C      ..... REJECT IF OVER ICE (USE SEA-SURFACE TEMPERATURE HERE)

                  IF(SSTDAT(IX,JY).LE.272.96)  THEN
                     NIR = NIR + 1
                     GO TO 112
                  END IF

                  KDATA = IBUFTN((27*IRT)-01:(27*IRT)+05)
                  DO IT = 1,7
                     IF((IT.NE.2.AND.KDATA(IT).LT.10000).OR.
     $                  (IT.EQ.2.AND.KDATA(IT).LT. 8000))  THEN
                        LBTER(IT) = LBTER(IT) + 1
                       PRINT 779,IT,IBUFTN(8),IBUFTN(9),KDATA
  779 FORMAT(' ##W3MISCAN: BT, CHN',I2,' BAD: SCAN',I6,', ORBIT',I8,
     $ '; BT:',7I6,'-CANNOT CALC. PRODS VIA ALG.')
                        GO TO 112
                     END IF
                  END DO

C CALL SUBR. MISC01 TO INITIATE IN-LINE PRODUCT CALCULATION

                  CALL MISC01(NNALG,GBALG,KDATA,SWNN,TPWNN,SWGB,NRFGB)

                  IF(NNALG)  THEN
CDAK                 IF(MOD(KNTSCN,100).EQ.0)  PRINT 6021, ATXT(1),SWNN,
CDAK $                TPWNN,REAL(KDATA(1))/100.,(REAL(KDATA(KKK))/100.,
CDAK $                KKK=3,5),(REAL(KDATA(4)-KDATA(5)))/100.
 6021 FORMAT(' W3MISCAN: ',A2,' SPD',F6.1,'  TPW',F6.1,'  TB19V',F6.1,
     $ '  TB22V',F6.1,'  TB37V',F6.1,'  TB37H',F6.1,'  TD37',F5.1)

C STORE THE CALCULATED NEURAL NET 3 WIND SPEED (*10 M/SEC)

                     IBUFTN((27*IRT)+6) = MIN(IMSG,NINT(SWNN*10.))

C STORE THE CALCULATED NEURAL NET 3 TPW (*10 MILLIMETERS)

                     IBUFTN((27*IRT)+7) = MIN(IMSG,NINT(TPWNN*10.))
                  END IF

                  IF(GBALG)  THEN
CDAK                 IF(MOD(KNTSCN,100).EQ.0)  PRINT 602, ATXT(2),NRFGB,
CDAK $                SWGB,REAL(KDATA(1))/100.,(REAL(KDATA(KKK))/100.,
CDAK $                KKK=3,5),(REAL(KDATA(4)-KDATA(5)))/100.
  602 FORMAT(' W3MISCAN: ',A2,' RF, SPD',I2,F6.1,'  TB19V',F6.1,
     $ '  TB22V',F6.1,'  TB37V',F6.1,'  TB37H',F6.1,'  TD37',F5.1)

C STORE THE CALCULATED GOODBERLET WIND SPEED (*10 M/SEC)

                     IBUFTN((27*IRT)+8) = MIN(IMSG,NINT(SWGB*10.))

C STORE THE GOODBERLET RAIN FLAG (0-3)

                     IBUFTN((27*IRT)+9) = MIN(IMSG,NRFGB)
                  END IF

C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
               ELSE

C......................................................................

C PROBLEM - CAN'T CALCULATE PRODUCTS VIA ANY ALG., ALL B.TEMPS MISSING

                  PRINT 879, IBUFTN(8),IBUFTN(9),KDATA
  879             FORMAT(' ##W3MISCAN: ALL B.TMPS MSSNG: SCAN',I6,', ',
     $             'ORBIT',I8,'; BT:',7I6,'-CANNOT CALC PRODS VIA ALG.')
C......................................................................

               END IF
            END IF

  112       CONTINUE
C-----------------------------------------------------------------------
         END DO
      END IF
C***********************************************************************
  901 CONTINUE

C RETURN TO CALLING PROGRAM - IER = 0 SCAN SUCCESSFULLY READ

      KNTSCN = KNTSCN + 1
      KNTSAT(IBUFTN(1)) = KNTSAT(IBUFTN(1)) + 1
      IER = 0
      RETURN

C.......................................................................
  993 CONTINUE

C PROBLEM: SEA-SURFACE TEMPERATURE NOT FOUND IN GRIB INDEX FILE - ERROR
C          RETURNED FROM GRIB DECODER GETGB IS 96 - SET IER = 6 & RETURN

      PRINT 2008, INGBI
 2008 FORMAT(/' ##W3MISCAN: SEA-SURFACE TEMPERATURE NOT FOUND IN GRIB ',
     $ 'INDEX FILE IN UNIT ',I2,' - IER = 6'/)
      IER = 6
      RETURN

C.......................................................................
  994 CONTINUE

C PROBLEM: SEA-SURFACE TEMPERATURE GRIB MESSAGE HAS A DATE THAT IS
C          EITHER: 1) MORE THAN 7-DAYS PRIOR TO THE EARLIEST REQ. DATE
C          (INPUT ARG. "KDATE") OR 2) MORE THAN 7-DAYS AFTER THE LATEST
C          REQ. DATE (INPUT ARG. "LDATE") - SET IER = 7 AND RETURN

      PRINT 2009
 2009 FORMAT('           SST GRIB MSG HAS DATE WHICH IS EITHER 7-DAYS',
     $ ' PRIOR TO EARLIEST REQ. DATE'/14X,'OR 7-DAYS LATER THAN LATEST',
     $ ' REQ. DATE - IER = 7'/)
      IER = 7
      RETURN

C.......................................................................
  995 CONTINUE

C PROBLEM: BYTE-ADDRESSABLE READ ERROR FOR GRIB FILE CONTAINING SEA-
C          SURFACE TEMPERATURE FIELD - ERROR RETURNED FROM GRIB DECODER
C          GETGB IS 97-99 - SET IER = 8 AND RETURN

      PRINT 2010
 2010 FORMAT('           BYTE-ADDRESSABLE READ ERROR FOR GRIB FILE ',
     $ 'CONTAINING SEA-SURFACE TEMPERATURE FIELD - IER = 8'/)
      IER = 8
      RETURN

C.......................................................................
  996 CONTINUE

C PROBLEM: ERROR RETURNED FROM GRIB DECODER - GETGB - FOR SEA-SURFACE
C          TEMPERATURE FIELD - > 0 BUT NOT 96-99 - SET IER = 9 & RETURN

      PRINT 2011
 2011 FORMAT('           - IER = 9'/)
      IER = 9
      RETURN

C.......................................................................
  997 CONTINUE

C PROBLEM: ERROR OPENING R. ACCESS FILE HOLDING LAND/SEA TAGS - SET IER
C          = 4 AND RETURN

      PRINT 2012, IERR,INLSF
 2012 FORMAT(/' ##W3MISCAN: ERROR OPENING R. ACCESS LAND/SEA FILE IN ',
     $ 'UNIT ',I2,' -- IOSTAT =',I5,' -- NO SCANS PROCESSED - IER = 4'/)
      IER = 4
      RETURN

C.......................................................................
  998 CONTINUE

C PROBLEM: THE INPUT DATA SET IS EITHER EMPTY (NULL), NOT BUFR, OR
C          CONTAINS NO DATA MESSAGES - SET IER = 2 AND RETURN

      PRINT 14, INDTA
   14 FORMAT(/' ##W3MISCAN: SSM-I DATA SET IN UNIT',I3,' IS EITHER ',
     $'EMPTY (NULL), NOT BUFR, OR CONTAINS NO DATA MESSAGES - IER = 2'/)
      IER = 2
      RETURN

C.......................................................................
  999 CONTINUE

C PROBLEM: THE NUMBER OF DECODED "LEVELS" IS NOT WHAT IS EXPECTED - SET
C          IER = 5 AND RETURN

      PRINT 217, NLEV,ILFLG
  217 FORMAT(/' ##W3MISCAN: THE NUMBER OF DECODED "LEVELS" (=',I5,') ',
     $ 'IS NOT WHAT IS EXPECTED (ILFLG=',I1,') - IER = 5'/)
      IER = 5
      RETURN

C.......................................................................
      END
C> @brief Prepares for in-line caluclation of prods.
C> @author Dennis Keyser @date 1995-01-04

C> Based on input 7-channel ssm/i brightness temperatures,
C> determines the rain flag category for wind speed product for the
C> goodberlet algorithm. Then calls the appropriate function to
C> calculate either the wind speed product for the goodberlet
C> algorithm (if requested) or the wind speed and tpw products for
C> the neural net 3 algorithm (if requested).
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> ????-??-?? | W. Gemmill | (w/nmc21) -- original author
C> 1995-01-04 | Dennis Keyser | -- incorporated into w3miscan and
C> streamlined code
C> 1996-05-07 | Dennis Keyser | (np22) -- in-line neural network 1 algoritm
C> replaced by neural network 2 algorithm
C> 1996-07-30 | Dennis Keyser | (np22) -- can now process wind speed from
C> both algorithms if desired
C> 1998-01-28 | Dennis Keyser | (np22) -- replaced neural net 2 algorithm
C> which calculated only wind speed product with neural net 3
C> algorithm which calculates both wind speed and total
C> precipitable water products (among others) but, unlike nn2,
C> does not return a rain flag value (it does set all retrievals
C> to missing that fail rain flag and ice contamination tests)
C>
C> @param[in] NNALG Process wind speed and tpw via neural net 3 algorithm if true
C> @param[in] GBALG Process wind speed via goodberlet algorithm if true
C> @param[in] KDATA 7-word array containing 7 channels of brightness temperature (kelvin x 100)
C> @param[out] SWNN  alculated wind speed based on neural net 3 algorithm (meters/second)
C> @param[out] TPWNN Calculated total column precipitable water based on neural net 3 algorithm (millimeters)
C> @param[out] SWGB Calculated wind speed based on goodberlet algorith (meters/second)
C> @param[out] NRFGB Rain flag category for calculated wind speed from goodberlet algorithm
C>
C> @remark If an algorithm is not chosen, the output products are set
C> to values of 99999. for that algorithm and, for the goodberlet
C> algorithm only, the rain flag is set to 99999. Called by
C> subroutine w3miscan().
C>
C> @author Dennis Keyser @date 1995-01-04
      SUBROUTINE MISC01(NNALG,GBALG,KDATA,SWNN,TPWNN,SWGB,NRFGB)
      LOGICAL  NNALG,GBALG
      REAL  BTA(4),BTAA(7)
      INTEGER  KDATA(7)

      COMMON/MISCEE/LFLAG,LICEC

      SAVE

      SWNN  = 99999.
      TPWNN = 99999.
      SWGB  = 99999.
      NRFGB = 99999

      TB19V = REAL(KDATA(1))/100.
      TB19H = REAL(KDATA(2))/100.
      TB22V = REAL(KDATA(3))/100.
      TB37V = REAL(KDATA(4))/100.
      TB37H = REAL(KDATA(5))/100.
      TB85V = REAL(KDATA(6))/100.
      TB85H = REAL(KDATA(7))/100.
      TD37  = TB37V - TB37H

      IF(NNALG)  THEN
C COMPUTE WIND SPEED FROM NEURAL NET 2 ALGORITHM (1995)
C  (no longer a possibility - subr. expects dim. of 5 on BTAA)
cdak     NRFNN = 1
cdak     IF(TB19H.LE.185.0.AND.TB37H.LE.210.0.AND.TB19V.LT.TB37V)
cdak $      NRFNN = 0
cdak     BTAA(1) = TB19V
cdak     BTAA(2) = TB22V
cdak     BTAA(3) = TB37V
cdak     BTAA(4) = TB37H
cdak     BTAA(5) = TB85V
cdak     SWNN = RISC02xx(BTAA)

C COMPUTE WIND SPEED AND TPW FROM NEURAL NET 3 ALGORITHM (1997)
         BTAA(1) = TB19V
         BTAA(2) = TB19H
         BTAA(3) = TB22V
         BTAA(4) = TB37V
         BTAA(5) = TB37H
         BTAA(6) = TB85V
         BTAA(7) = TB85H
         SWNN = RISC02(BTAA,TPWNN,LQWNN,SSTNN,JERR)
         IF(JERR.EQ.1)  LFLAG = LFLAG + 1
         IF(JERR.EQ.2)  LICEC = LICEC + 1
      END IF

      IF(GBALG)  THEN
C COMPUTE WIND SPEED FROM GOODBERLET ALGORITHM
         NRFGB = 0
         IF(TD37.LE.50.0.OR.TB19H.GE.165.0)  THEN
            IF(TD37.LE.50.0.OR.TB19H.GE.165.0)  NRFGB = 1
            IF(TD37.LE.37.0)  NRFGB = 2
            IF(TD37.LE.30.0)  NRFGB = 3
         END IF
         BTA(1) = TB19V
         BTA(2) = TB22V
         BTA(3) = TB37V
         BTA(4) = TB37H
         SWGB = RISC03(BTA)
      END IF

      RETURN
      END
C> @brief Calc. ssm/i prods from neural net 3 alg.
C> @author V. Krasnopolsky @date 1997-02-02

C> This retrieval algorithm is a neural network implementation
C> of the ssm/i transfer function. It retrieves the wind speed (w)
C> at the height 20 meters, columnar water vapor (v), columnar liquid
C> water (l) and sst. The nn was trained using back-propagation
C> algorithm. Transfer function is described and compared with
C> cal/val and other algorithms in omb technical note no. 137. See
C> remarks for detailed info on this algorithm. This is an improved
C> version of the earlier neural network 2 algorithm.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1997-02-02 | V. Krasnopolsky | Initial.
C>
C> @param[in] XT 7-word array containing brightness temperature in the order:
C> t19v (word 1), t19h (word 2), t22v (word 3), t37v (word 4), t37h (word 5),
C> t85v (word 6), t85h (word 7) (all in kelvin)
C> @param[in] V Columnar water vapor (total precip. water) (mm)
C> @param[in] L Columnar liquid water (mm)
C> @param[in] SST Sea surface temperature (deg. c)
C> @param[in] JERR Error return code:
C> - = 0 -- Good retrievals
C> - = 1 -- Retrievals could not be made due to one or
C> more brightness temperatures out of range
C> (i.e, failed the rain flag test)
C> - = 2 -- Retrievals could not be made due to ice
C> contamination
C> {for either 1 or 2 above, all retrievals set to
C> 99999. (missing)}
C>
C> @remark Function, called by subroutine misc01.
C> Description of training and test data set:
C> ------------------------------------------
C> The training set consists of 3460 matchups which were received
C> from two sources:
C> - 1.  3187 F11/SSMI/buoy matchups were filtered out from a
C> preliminary version of the new NRL database which was
C> kindly provided by G. Poe (NRL). Maximum available wind
C> speed is 24 m/s.
C> - 2.  273 F11/SSMI/OWS matchups were filtered out from two
C> datasets collected by high latitude OWS LIMA and MIKE.
C> These data sets were kindly provided by D. Kilham
C> (University of Bristol).  Maximum available wind speed
C> is 26.4 m/s.
C>
C> Satellite data are collocated with both buoy and OWS data in
C> space within 15 km and in time within 15 min.
C>
C> The test data set has the same structure, the same number of
C> matchups and maximum buoy wind speed.
C>
C> Description of retrieval flags:
C> -------------------------------
C> Retrieval flags by Stogryn et al. are used.  The algorithm
C> produces retrievals under CLEAR + CLOUDY conditions, that is
C> if:
C> - T37V - T37H > 50.   => CLEAR condition -or-
C> - T37V - T37H =< 50.|
C> - T19H =< 185.  and |
C> - T37H =< 210.  and | => CLOUDY conditions
C> - T19V  < T37V      |
C>
C> @author V. Krasnopolsky @date 1997-02-02
      FUNCTION RISC02(XT,V,L,SST,JERR)
      PARAMETER (IOUT =4)
      LOGICAL  LQ1,LQ2,LQ3,LQ4
      REAL  XT(7),Y(IOUT),V,L,SST
      EQUIVALENCE (Y(1),SPN)

      JERR = 0

C --------  Retrieval flag (Stogryn) -------------------------

C  T19H =< 185

      LQ1 = (XT(2).LE.185.)

C  T37H =< 210

      LQ2 = (XT(5).LE.210.)

C  T19V < T37V

      LQ3 = (XT(1).LT.XT(4))

C  T37V - T37H =< 50.

      LQ4 = ((XT(4) - XT(5)).LE.50.)
      LQ1 = (LQ1.AND.LQ2.AND.LQ3)
      IF(.NOT.LQ1.AND.LQ4) THEN
         SPN  = 99999.
         V    = 99999.
         L    = 99999.
         SST  = 99999.
         JERR = 1
         GO TO 111
      END IF

C --------------- Call NN ----------------------

C  NN WIND SPEED

      CALL MISC10(XT,Y)
      V   = Y(2)
      L   = Y(3)
      SST = Y(4)

C --------- Remove negative values  ----------------------------

      IF(SPN.LT.0.0)  SPN = 0.0
      IF(SST.LT.0.0)  SST = 0.0
      IF(V  .LT.0.0)  V   = 0.0

C ------ Remove ice contamination ------------------------------------

      ICE = 0
      SI85 = -174.4 + (0.715 * XT(1)) + (2.439 * XT(3)) - (0.00504 *
     $  XT(3) * XT(3)) - XT(6)
      TT = 44. + (0.85 * XT(1))
      IF(SI85.GE.10.)  THEN
         IF(XT(3).LE.TT)  ICE = 1
         IF((XT(3).GT.264.).AND.((XT(3)-XT(1)).LT.2.))  ICE = 1
      END IF
      IF(ICE.EQ.1)  THEN
         SPN = 99999.
         V   = 99999.
         L   = 99999.
         SST = 99999.
         JERR = 2
      END IF

  111 CONTINUE

      RISC02 = SPN

      RETURN
      END
C> @brief Calc. ssm/i prods from neural net 3 alg.
C> @author V. Krasnopolsky @date 1996-07-15

C> This nn calculates w (in m/s), v (in mm), l (in mm), and
C> sst (in deg c). This nn was trained on blended f11 data set
C> (ssmi/buoy matchups plus ssmi/ows matchups 15 km x 15 min) under
C> clear + cloudy conditions.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1996-07-15 | V. Krasnopolsky | Initial.
C>
C> @param[in] X 5-word array containing brightness temperature in the
C> order: t19v (word 1), t19h (word 2), t22v (word 3),
C> t37v (word 4), t37h (word 5) (all in kelvin)
C> @param[out] Y 4-word array containing calculated products in the
C> order: wind speed (m/s) (word 1), columnar water
C> vapor (total precip. water) (mm) (word 2),  columnar
C> liquid water (mm) (word 3), sea surface temperature
C> (deg. c) (word 4)
C>
C> @remark Called by subroutine risc02().
C>
C> @author V. Krasnopolsky @date 1996-07-15
      SUBROUTINE MISC10(X,Y)
      INTEGER  HID,OUT

C IN IS THE NUMBER OF NN INPUTS, HID IS THE NUMBER OF HIDDEN NODES,
C  OUT IS THE NUMBER OF OUTPUTS

      PARAMETER (IN =5, HID =12, OUT =4)
      DIMENSION  X(IN),Y(OUT),W1(IN,HID),W2(HID,OUT),B1(HID),B2(OUT),
     $ O1(IN),X2(HID),O2(HID),X3(OUT),O3(OUT),A(OUT),B(OUT)

C W1 HOLDS INPUT WEIGHTS

      DATA ((W1(I,J),J = 1,HID),I = 1,IN)/
     $-0.0435901, 0.0614709,-0.0453639,-0.0161106,-0.0271382, 0.0229015,
     $-0.0650678, 0.0704302, 0.0383939, 0.0773921, 0.0661954,-0.0643473,
     $-0.0108528,-0.0283174,-0.0308437,-0.0199316,-0.0131226, 0.0107767,
     $ 0.0234265,-0.0291637, 0.0140943, .00567931,-.00931768,
     $-.00860661, 0.0159747,-0.0749903,-0.0503523, 0.0524172, 0.0195771,
     $ 0.0302056, 0.0331725, 0.0326714,-0.0291429, 0.0180438, 0.0281923,
     $-0.0269554, 0.102836,  0.0591511, 0.134313, -0.0109854,-0.0786303,
     $ 0.0117111, 0.0231543,-0.0205603,-0.0382944,-0.0342049,
     $ 0.00052407,0.110301, -0.0404777, 0.0428816, 0.0878070, 0.0168326,
     $ 0.0196183, 0.0293995, 0.00954805,-.00716287,0.0269475,
     $-0.0418217,-0.0165812, 0.0291809/

C W2 HOLDS HIDDEN WEIGHTS

      DATA ((W2(I,J),J = 1,OUT),I = 1,HID)/
     $-0.827004, -0.169961,-0.230296, -0.311201, -0.243296, 0.00454425,
     $ 0.950679,  1.09296,  0.0842604, 0.0140775, 1.80508, -0.198263,
     $-0.0678487, 0.428192, 0.827626,  0.253772,  0.112026, 0.00563793,
     $-1.28161,  -0.169509, 0.0019085,-0.137136, -0.334738, 0.224899,
     $-0.189678,  0.626459,-0.204658, -0.885417, -0.148720, 0.122903,
     $ 0.650024,  0.715758, 0.735026, -0.123308, -0.387411,-0.140137,
     $ 0.229058,  0.244314,-1.08613,  -0.294565, -0.192568, 0.608760,
     $-0.753586,  0.897605, 0.0322991,-0.178470,  0.0807701,
     $-0.781417/

C B1 HOLDS HIDDEN BIASES

      DATA (B1(I), I=1,HID)/
     $ -9.92116,-10.3103,-17.2536,  -5.26287, 17.7729,-20.4812,
     $ -4.80869,-11.5222,  0.592880,-4.89773,-17.3294, -7.74136/

C B2 HOLDS OUTPUT BIAS

      DATA (B2(I), I=1,OUT)/-0.882873,-0.0120802,-3.19400,1.00314/

C A(OUT), B(OUT) HOLD TRANSFORMATION COEFFICIENTS

      DATA (A(I), I=1,OUT)/18.1286,31.8210,0.198863,37.1250/
      DATA (B(I), I=1,OUT)/13.7100,32.0980,0.198863,-5.82500/

C INITIALIZE

      O1 = X

C START NEURAL NETWORK

C  - INITIALIZE X2

      DO I = 1,HID
         X2(I) = 0.
         DO J = 1,IN
            X2(I) = X2(I) + (O1(J) * W1(J,I))
         END DO
         X2(I) = X2(I) + B1(I)
         O2(I) = TANH(X2(I))
      END DO

C - INITIALIZE X3

      DO K = 1,OUT
         X3(K) = 0.
         DO J = 1,HID
            X3(K) = X3(K) + (W2(J,K) * O2(J))
         END DO

         X3(K) = X3(K) + B2(K)

C --- CALCULATE O3

         O3(K) = TANH(X3(K))
         Y(K) = (A(K) * O3(K)) + B(K)
      END DO

      RETURN
      END
C> @brief Calc. wspd from neural net 2 algorithm
C> @author V. Krasnopolsky @date 1996-05-07

C> Calculates a single neural network output for wind speed.
C> the network was trained on the whole data set without any
C> separation into subsets. It gives rms = 1.64 m/s for training set
C> and 1.65 m/s for testing set. This is an improved version of the
C> earlier neural network 1 algorithm.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1994-03-20 | V. Krasnopolsky | Initial.
C> 1995-05-07 | V. Krasnopolsky | Replaced with neural net 2 algorithm.
C>
C> @param[in] X 5-Word array containing brightness temperature in the
C> order: t19v (word 1), t22v (word 2), t37v (word 3),
C> t37h (word 4), t85v (word 5) (all in kelvin)
C> @return XX Wind speed (meters/second)
C>
C> @remark Function, no longer called by this program. It is here
C> simply to save neural net 2 algorithm for possible later use
C> (has been replaced by neural net 3 algorithm, see subr. risc02
C> and misc10).
C>
C> @author V. Krasnopolsky @date 1996-05-07
      FUNCTION RISC02xx(X)
      INTEGER  HID
C IN IS THE NUMBER OF B. TEMP. CHNLS, HID IS THE NUMBER OF HIDDEN NODES
      PARAMETER (IN =5, HID =2)
      DIMENSION  X(IN),W1(IN,HID),W2(HID),B1(HID),O1(IN),X2(HID),O2(HID)

      SAVE

C W1 HOLDS INPUT WEIGHTS
      DATA ((W1(I,J),J=1,HID),I=1,IN)/
     $ 4.402388E-02, 2.648334E-02, 6.361322E-04,-1.766535E-02,
     $ 7.876555E-03,-7.387260E-02,-2.656543E-03, 2.957161E-02,
     $-1.181134E-02, 4.520317E-03/
C W2 HOLDS HIDDEN WEIGHTS
      DATA (W2(I),I=1,HID)/8.705661E-01,1.430968/
C B1 HOLDS HIDDEN BIASES
      DATA (B1(I),I=1,HID)/-6.436114,8.799655/
C B2 HOLDS OUTPUT BIAS
C AY AND BY HOLD OUTPUT TRANSFORMATION COEFFICIENTS
      DATA  B2/-0.736255/,AY/16.7833/,BY/11.08/
      O1 = X
C INITIALIZE
      X3 = 0.
      DO I = 1, HID
         O2(I) = 0.
         X2(I) = 0.
         DO J = 1,IN
            X2(I) = X2(I) + (O1(J) * W1(J,I))
         END DO
         X2(I) = X2(I) + B1(I)
         O2(I) = TANH(X2(I))
         X3 = X3 + (O2(I)* W2(I))
      END DO
      X3 = X3 + B2
      O3 = TANH(X3)
      RISC02xx = (AY * O3) + BY
      RISC02xx = MAX(RISC02xx,0.0)
C BIAS CORRECTION
      BIAS = 0.5 + 0.004*((RISC02xx-10.)**3)*(1.-EXP(-0.5*RISC02xx))
      RISC02xx = RISC02xx + BIAS
      RETURN
      END
C> @brief Calc. w.spd from b temp.- goodberlet alg.
C> @author W. Gemmill @date 1994-08-15

C> Calculates a single goodberlet output for wind speed.
C> This is a linear regression algorithm from 1989.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1994-08-15 | W. Gemmill | Initial.
C>
C> @param[in] X 4-word array containing brightness temperature in the
C> order: t19v (word 1), t22v (word 2), t37v (word 3),
C> t37h (word 4) (all in kelvin)
C> @return XX Wind speed (meters/second)
C>
C> @remark Function, called by subroutine misc01.
C>
C> @author W. Gemmill @date 1994-08-15
      FUNCTION RISC03(X)
      DIMENSION  X(4)

      SAVE

      RISC03 = 147.90 + (1.0969 * X(1)) - (0.4555 * X(2)) -
     $ (1.76 * X(3)) + (0.7860 * X(4))
      RETURN
      END
C> @brief Returns land/sea tag for given lat/lon
C> @author Dennis Keyser @date 1995-01-04

C> Finds and returns the low resolution land/sea tag nearest
C> to the requested latitude and longitude.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1978-01-20 | J. K. Kalinowski (S11213) | Original author
C> 1978-10-03 | J. K. Kalinowski (S1214) | Changes unknown
C> 1985-03-01 | N. Digirolamo (SSAI) | Conversion to vs fortran
C> 1995-01-04 | Dennis Keyser | Incorporated into w3miscan and streamlined code
C>
C> @param[in] INLSF Unit number of direct access nesdis land/sea file
C> @param[in] BLAT Latitude (whole degrees: range is 0. to +90. north,
C> 0. to -90. south)
C> @param[in] BLNG Longitude (whole degrees: range is 0. to +179.99 east,
C> 0. to -180. west)
C> @param[out] LSTAG Land/sea tag {=0 - sea; =1 - land; =2 - coastal
C> interface (higher resolution tags are available);
C> =3 - coastal interface (no higher resolution tags
C> exist)}
C>
C> @remark Called by subroutine w3miscan.
C>
C> @author Dennis Keyser @date 1995-01-04
      SUBROUTINE MISC04(INLSF,BLAT,BLNG,LSTAG)
      CHARACTER*1  LPUT
      REAL  RGS(3)
C LPUT CONTAINS A REGION OF LAND/SEA TAGS (RETURNED FROM CALL TO MISC05)
      COMMON/MISCDD/LPUT(21960)

      SAVE

C RGS IS ARRAY HOLDING SOUTHERN BOUNDARIES OF EACH LAND/SEA TAG REGION
      DATA  RGS/-85.,-30.,25./,NUMRGL/0/,IFLAG/0/
C INITIALIZE LAND/SEA TAG AS 1 (OVER LAND)
      LSTAG = 1
C FIND NEAREST POINT OF A HALF-DEGREE (LAT,LONG) GRID
C ..ALAT IS LATITUDE TO THE NEAREST HALF-DEGREE
      ALAT = INT((BLAT+SIGN(.25,BLAT))/.5) * .5
C ..ALNG IS LONGITUDE TO THE NEAREST HALF-DEGREE
      ALNG = INT((BLNG+SIGN(.25,BLNG))/.5) * .5
      IF(NINT(ALNG*10.).EQ.1800)  ALNG = -180.
C IDENTIFY DATABASE REGION IN WHICH TO FIND CORRECT TAG
      NUMRGN = 1
      IF(IABS(NINT(ALAT*10)).GT.850)  THEN
         RETURN
      ELSE  IF(NINT(ALAT*10).GT.275)  THEN
         NUMRGN = 3
      ELSE  IF(NINT(ALAT*10.).GE.-275)  THEN
         NUMRGN = 2
      END IF
      IF(NUMRGN.NE.NUMRGL.OR.IFLAG.EQ.1)  THEN
         NUMRGL = NUMRGN
         CALL MISC05(INLSF,NUMRGN,*99)
      END IF
C FIND THE BYTE & BIT PAIR W/I DATA BASE REGION CONTAINING DESIRED TAG
      TRM1  = ((ALAT - RGS(NUMRGN)) * 1440.) + 360.
      LSTPT = TRM1 + (2. * ALNG)
C ..NBYTE IS THE BYTE IN LPUT CONTAINING THE TAG
      NBYTE = (180 * 8) + (LSTPT/4 * 8)
      NSHFT = (2 * (MOD(LSTPT,4) + 1)) - 2
C PULL OUT THE TAG
      CALL GBYTE(LPUT,LSTAG,NBYTE+NSHFT,2)
      IFLAG = 0
      RETURN
C-----------------------------------------------------------------------
   99 CONTINUE
C COME HERE IF LAND/SEA TAG COULD NOT BE RETURNED FROM SUBR. W3MISCAN
C  (IN THIS CASE IT WILL REMAIN SET TO 1 INDICATING OVER LAND)
      IFLAG = 1
      RETURN
C-----------------------------------------------------------------------
      END
C> @brief Reads 2 records from land/sea tag database
C> @author Dennis Keyser @date 195-01-04

C> Reads two records from a low resolution land/sea database and stores into common.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1978-01-20 | J. K. Kalinowski (S11213) | Original author
C> 1995-01-04 | Dennis Keyser | Incorporated into w3miscan and
C> streamlined code; modified to be machine independent thru
C> use of standard fortran direct access read
C>
C> @param[in] INLSF Unit number of direct access nesdis land/sea file
C> @param[in] NUMRGN The region (1,2 or 3) of the database to be accessed
C> (dependent on latitude band)
C>
C> @remark Called by subroutne misc04.
C>
C> @author Dennis Keyser @date 195-01-04
      SUBROUTINE MISC05(INLSF,NUMRGN,*)
      CHARACTER*1  LPUT

C LPUT CONTAINS A REGION OF LAND/SEA TAGS (COMPRISED OF 2 RECORDS FROM
C  LAND/SEA FILE) -- 180 BYTES OF DOCUMENTATION FOLLOWED BY 21780 BYTES
C  OF LAND/SEA TAGS

      COMMON/MISCDD/LPUT(21960)

      SAVE

      NREC = (2 * NUMRGN) - 1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II),II=1,10980)
      NREC = NREC + 1
      READ(INLSF,REC=NREC,ERR=10)  (LPUT(II),II=10981,21960)
      RETURN
C-----------------------------------------------------------------------
   10 CONTINUE
C ERROR READING IN A RECORD FROM LAND-SEA FILE -- RETURN (TAG WILL BE
C  SET TO 1 MEANING OVER LAND IN THIS CASE)
      PRINT 1000, NREC,INLSF
 1000 FORMAT(' ##W3MISCAN/MISC05: ERROR READING IN LAND-SEA DATA ',
     $ 'RECORD',I7,' IN UNIT ',I2,' -- SET TAG TO LAND'/)
      RETURN 1
C-----------------------------------------------------------------------
      END
C> @brief Reads in nh and sh 1-deg. sea-sfc temps.
C> @author Dennis Keyser @date 200-02-18

C> Reads in global sea-surface temperature field on a one-degree grid from grib file.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> ????-??-?? | W. Gemmill (NP21) | Original author
C> 1995-01-04 | Dennis Keyser | Incorporated into w3miscan and
C> streamlined code; converted sst input file from vsam/on84 to
C> grib to allow code compile and run on the cray machines.
C> 2000-02-18 | Dennis Keyser | Modified to call w3lib routine "getgb",
C> this allows code to compile and run properly on ibm-sp
C>
C> @param[in] INGBI Unit number of grib index file for grib file
C> containing global 1-degree sea-surface temp field
C> @param[in] INGBD Unit number of grib file containing global 1-degree
C> sea-surface temp field
C> @param[in] IDAT1 Requested earliest year(yyyy), month, day, hour, min
C> @param[in] IDAT2 Requested latest   year(yyyy), month, day, hour, min
C>
C> @remark Called by subroutine w3miscan.
C>
C> @author Dennis Keyser @date 200-02-18
      SUBROUTINE MISC06(INGBI,INGBD,IDAT1,IDAT2,*,*,*,*)
      PARAMETER (MAXPTS=360*180)
      LOGICAL*1  LBMS(360,180)
      INTEGER  KPDS(200),KGDS(200),LPDS(200),LGDS(200),IDAT1(5),
     $ IDAT2(5),JDAT1(8),JDAT2(8),KDAT(8),LDAT(8),MDATE(8)
      REAL  RINC(5)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FILEB,FILEI
      COMMON/MISCCC/SSTDAT(360,180)

      SAVE

      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') INGBD
      CALL GETENV(ENVVAR,FILEB)
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') INGBI
      CALL GETENV(ENVVAR,FILEI)
      CALL BAOPENR(INGBD,FILEB,IRET1)
ccccc PRINT *,'SAGT: ',INGBD,FILEB,IRET1
      CALL BAOPENR(INGBI,FILEI,IRET2)
ccccc PRINT *,'SAGT: ',INGBI,FILEI,IRET2

      KPDS = -1
      KGDS = -1
      N = -1
      KPDS(5)  = 11
      KPDS(6)  =  1
      KPDS(7)  =  0
      KPDS(8)  = -1
      KPDS(9)  = -1
      KPDS(10) = -1
      PRINT 68, INGBD
   68 FORMAT(//4X,'** W3MISCAN/MISC06: READ IN "CURRENT" SEA-SURFACE ',
     $ 'TEMPERATURE DATA FROM GRIB MESSAGE IN UNIT',I3)
      CALL GETGB(INGBD,INGBI,MAXPTS,0,KPDS,KGDS,KF,K,LPDS,LGDS,LBMS,
     $ SSTDAT,IRET)
C.......................................................................
C ABNORMAL RETURN IF PROBLEM WITH SST IN GRIB FILE
      IF(IRET.NE.0)  THEN
        WRITE(6,*)' ERROR READING SST USING GETGB.  IRET = ',IRET
        IF (IRET.EQ.96) RETURN 1
        IF (IRET.EQ.97) RETURN 3
        IF (IRET.EQ.98) RETURN 3
        IF (IRET.EQ.99) RETURN 3
        RETURN 4
      ENDIF
C.......................................................................
C READ SUCCESSFUL
      JDAT1 = 0
      JDAT2 = 0
      JDAT1(1:3) = IDAT1(1:3)
      JDAT1(5:6) = IDAT1(4:5)
      JDAT2(1:3) = IDAT2(1:3)
      JDAT2(5:6) = IDAT2(4:5)
      MDATE = 0
      MDATE(1) = ((LPDS(21) - 1) * 100) + LPDS(8)
      MDATE(2:3) = LPDS(9:10)
      MDATE(5:6) = LPDS(11:12)
      CALL W3MOVDAT((/-7.,0.,0.,0.,0./),JDAT1,KDAT)
      CALL W3MOVDAT((/ 7.,0.,0.,0.,0./),JDAT2,LDAT)
cppppp
      print *, '** W3MISCAN/MISCO6: SST GRIB FILE MUST HAVE DATE ',
     $ 'BETWEEN ',(kdat(iii),iii=1,3),(kdat(iii),iii=5,6),' AND ',
     $ (ldat(iii),iii=1,3),(ldat(iii),iii=5,6)
      print *, '                    RETURNED FROM GRIB FILE IS YEAR ',
     $ 'OF CENTURY = ',lpds(8),' AND CENTURY = ',lpds(21)
      print *, '                    CALULATED 4-DIGIT YEAR IS = ',
     $ mdate(1)
cppppp
      CALL W3DIFDAT(KDAT,MDATE,3,RINC)
      KMIN = RINC(3)
      CALL W3DIFDAT(LDAT,MDATE,3,RINC)
      LMIN = RINC(3)
      IF(KMIN.GT.0.OR.LMIN.LT.0)  THEN
C.......................................................................
C COME HERE IF SST GRIB MSG HAS A DATE THAT IS EITHER: 1) MORE THAN 7-
C  DAYS PRIOR TO THE EARLIEST REQ. DATE (INPUT ARG. "IDAT1" TO W3MISCAN)
C  OR 2) MORE THAN 7-DAYS AFTER THE LATEST REQ. DATE (INPUT ARG.
C  "IDAT2" TO W3MISCAN)
         PRINT 27, (MDATE(III),III=1,3),(MDATE(III),III=5,6)
   27    FORMAT(/' ##W3MISCAN/MISC06: SST GRIB MSG HAS DATE:',I5,4I3,
     $    ' - AS A RESULT......')
         RETURN 2
C.......................................................................
      END IF
      PRINT 60, (MDATE(III),III=1,3),(MDATE(III),III=5,6)
   60 FORMAT(/4X,'** W3MISCAN/MISC06: SEA-SFC TEMP SUCCESSFULLY READ ',
     $ 'IN FROM GRIB FILE, DATE IS: ',I5,4I3/)
      RETURN

      CALL BACLOSE(INGBI,IRET)
      CALL BACLOSE(INGBD,IRET)

      END
