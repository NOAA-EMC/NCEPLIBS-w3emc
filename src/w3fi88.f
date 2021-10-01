C> @file
C> @brief BUFR message decoder
C> @author Bill Cavanaugh @date 1988-08-31

C> This set of routines will decode a bufr message and
C> place information extracted from the bufr message into selected
C> arrays for the user. the array kdata can now be sized by the user
C> by indicating the maximum number of subsets and the maximum
C> number of descriptors that are expected in the course of decoding
C> selected input data.  this allows for realistic sizing of kdata
C> and the mstack arrays. this version also allows for the inclusion
C> of the unit numbers for tables b and d into the
C> argument list. this routine does not include ifod processing.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-08-31
C> - Bill Cavanaugh 1990-12-07 Now Utilizing gbyte routines to gather
C> and separate bit fields. this should improve
C> (decrease) the time it takes to decode any
C> bufr message. have entered coding that will
C> permit processing bufr editions 1 and 2.
C> improved and corrected the conversion into
C> ifod format of decoded bufr messages.
C> - Bill Cavanaugh 1991-01-18 Program/routines modified to properly handle
C> serial profiler data.
C> - Bill Cavanaugh 1991-04-04 Modified to handle text supplied thru
C> descriptor 2 05 yyy.
C> - Bill Cavanaugh 1991-04-17 Errors in extracting and scaling data
C> corrected. improved handling of nested
C> queue descriptors is added.
C> - Bill Cavanaugh 1991-05-10 Array 'data' has been enlarged to real*8
C> to better contain very large numbers more
C> accurately. the preious size real*4 could not
C> contain sufficient significant digits.
C> coding has been introduced to process new
C> table c descriptor 2 06 yyy which permits in
C> line processing of a local descriptor even if
C> the descriptor is not contained in the users
C> table b.
C> a second routine to process ifod messages
C> (ifod0) has been removed in favor of the
C> improved processing of the one
C> remaining (ifod1).
C> new coding has been introduced to permit
C> processing of bufr messages based on bufr
C> edition up to and including edition 2.
C> please note increased size requirements
C> for arrays ident(20) and iptr(40).
C> - Bill Cavanaugh 1991-07-26 Add Array mtime to calling sequence to
C> permit inclusion of receipt/transfer times
C> to ifod messages.
C> - Bill Cavanaugh 1991-09-25 All processing of decoded bufr data into
C> ifod (a local use reformat of bufr data)
C> has been isolated from this set of routines.
C> for those interested in the ifod form,
C> see w3fl05 in the w3lib routines.
C>     processing of bufr messages containing
C> delayed replication has been altered so that
C> single subsets (reports) and and a matching
C> descriptor list for that particular  subset
C> will be passed to the user will be passed to
C> the user one at a time to assure that each
C> subset can be fully defined with a minimum
C> of reprocessing.
C>     processing of associated fields has been
C> tested with messages containing non-compressed
C> data.
C>     in order to facilitate user processing
C> a matching list of scale factors are included
C> with the expanded descriptor list (mstack).
C> - Bill Cavanaugh 1991-11-21 Processing of descriptor 2 03 yyy
C> has corrected to agree with fm94 standards.
C> - Bill Cavanaugh 1991-12-19 Calls to fi8803 and fi8804 have been
C> corrected to agree called program argument
C> list. some additional entries have been
C> included for communicating with data access
C> routines. additional error exit provided for
C> the case where table b is damaged.
C> - Bill Cavanaugh 1992-01-24 Routines fi8801, fi8803 and fi8804
C> have been modified to handle associated fields
C> all descriptors are set to echo to mstack(1,n)
C> - Bill Cavanaugh 1992-05-21 Further expansion of information collected
C> from within upper air soundings has produced
C> the necessity to expand some of the processing
C> and output arrays. (see remarks below)
C>   corrected descriptor denoting height of
C> each wind level for profiler conversions.
C> - Bill Cavanaugh 1992-07-23 Expansion of table b requires adjustment
C> of arrays to contain table b values needed to
C> assist in the decoding process.
C> arrays containing data from table b
C> - KFXY1 Descriptor
C> - ANAME1 Descriptor name
C> - AUNIT1 Units for descriptor
C> - ISCAL1 Scale for value of descriptor
C> - IRFVL1 Reference value for descriptor
C> - IWIDE1 Bit width for value of descriptor
C> - Bill Cavanaugh 1992-09-09 First encounter with operator descriptor
C> 2 05 yyy showed error in decoding. that error
C> is corrected with this implementation. further
C> testing of upper air data has encountered
C> the condition of large (many level) soundings
C> arrays in the decoder have been expanded (again)
C> to allow for this condition.
C> - Bill Cavanaugh 1992-10-02 Modified routine to reformat profiler data
C> (fi8809) to show descriptors, scale value and
C> data in proper order.  corrected an error that
C> prevented user from assigning the second dimension
C> of kdata(500,*).
C> - Bill Cavanaugh 1992-10-20 Removed error that prevented full
C> implementation of previous corrections and
C> made corrections to table b to bring it up to
C> date. changes include proper reformat of profiler
C> data and user capability for assigning second
C> dimension of kdata array.
C> - Bill Cavanaugh 1992-12-09 Thanks to dennis keyser for the suggestions
C> and coding, this implementation will allow the
C> inclusion of unit numbers for tables b & d, and
C> in addition allows for realistic sizing of kdata
C> and mstack arrays by the user. as of this
C> implementation, the upper size limit for a bufr
C> message allows for a message size greater than
C> 15000 bytes.
C> - Bill Cavanaugh 1993-01-26 Routine fi8810 has been added to permit
C> reformatting of profiler data in edition 2.
C> - Bill Cavanaugh 1993-05-13 Routine fi8811 has been added to permit
C> processing of run-line encoding. this provides for
C> the handling of data for graphics products.
C> please note the addition of two arguments in the
C> calling sequence.
C> - Bill Cavanaugh 1993-12-01 Routine fi8803 to correct handling of
C> associated fields and arrays associated with
C> table b entries enlarged to handle larger table b
C> - Bill Cavanaugh 1994-05-25 Routines have been modified to construct a
C> modified table b i.e., it is tailored to contain o
C> those descriptors that will be used to decode
C> data in current and subsequent bufr messages.
C> table b and table d descriptors will be isolated
C> and merged with the main tables for use with
C> following bufr messages.
C> the descriptors indicating the replication of
C> descriptors and data are activated with this
C> implementation.
C> - Bill Cavanaugh 1994-08-30 Added statements that will allow use of
C> these routines directly on the cray with no
C> modification. handling od table d entries has been
C> modified to prevent loss of ancillary entries.
C> coding has been added to allow processing on
C> either an 8 byte word or 4 byte word machine.
C>
C> For those users of the bufr decoder that are
C> processing sets of bufr messages that include
C> type 11 messages, coding has been added to allow
C> the recovery of the added or modified table b
C> entries by writing them to a disk file available
C> to the user. this is accomplished with no change
C> to the calling sequence. table b entries will be
C> designated as follows:
C> IUNITB - Is the unit number for the master table b.
C> IUNITB+1 - Will be the unit number for the table b entries that are to be used
C> in the decoding of subsequent messages. this device will be formatted the same
C> the disk file on iunitb.
C>
C> - Dennis Keyser 1995-06-07 Corrected an error which required input
C> argument "maxd" to be nearly twice as large as
C> needed for decoding wind profiler reports (limit
C> upper bound for "iwork" array was set to "maxd",
C> now it is set to 15000). also, a correction was
C> made in the wind profiler processing to prevent
C> unnecessary looping when all requested
C> descriptors are missing.  also corrected an
C> error which resulted in returned scale in
C> "mstack(2, ..)" always being set to zero for
C> compressed data.
C> - Bill Cavanaugh 1996-02-15 Modified identification of ascii/ebcdic
C> machine. modified handling of table b to permit
C> faster processing of multiple messages with
C> changing data types and/or subtypes.
C> - Bill Cavanaugh 1996-04-02 Deactivated extraneous write statement.
C> enlarged arrays for table b entries to contain
C> up to 1300 entries in preparation for new
C> additions to table b.
C> - Dennis Keyser 2001-02-01 The table b file will now be read whenever the
C> input argument "iunitb" (table b unit number)
C> changes from its value in the previous call to
C> this routine (normally it is only read the
C> first time this routine is called)
C> - Boi Vuong 2002-10-15 Replaced function ichar with mova2i
C>
C> @param[in] MSGA Array containing supposed bufr message
C> size is determined by user, can be greater
C> than 15000 bytes.
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[in] IUNITB Unit number of data set holding table b, this is the
C> number of a pair of data sets
C> -IUNITB+Unit number for a dataset to contain table b entries
C> from master table b and table b entries extracted
C> from type 11 bufr messages that were used to decode
C> current bufr messages.
C> @param[in] IUNITD Unit number of data set holding tab
C> @param[out] ISTACK Original array of descriptors extracted from
C> source bufr message.
C> @param[out] MSTACK (A,B)-LEVEL B Descriptor number (limited to value of
C> input argument maxd)
C> - Level A:
C>  - = 1 Descriptor
C>  - = 2 10**N scaling to return to original value
C> @param[out] IPTR Utility array (should have at last 42 entries)
C> - IPTR(1)- Error return
C> - IPTR(2)- Byte count section 1
C> - IPTR(3)- Pointer to start of section 1
C> - IPTR(4)- Byte count section 2
C> - IPTR(5)- Pointer to start of section 2
C> - IPTR(6)- Byte count section 3
C> - IPTR(7)- Pointer to start of section 3
C> - IPTR(8)- Byte count section 4
C> - IPTR(9)- Pointer to start of section 4
C> - IPTR(10)- Start of requested subset, reserved for dar
C> - IPTR(11)- Current descriptor ptr in iwork
C> - IPTR(12)- Last descriptor pos in iwork
C> - IPTR(13)- Last descriptor pos in istack
C> - IPTR(14)- Number of master table b entries
C> - IPTR(15)- Requested subset pointer, reserved for dar
C> - IPTR(16)- Indicator for existance of section 2
C> - IPTR(17)- Number of reports processed
C> - IPTR(18)- Ascii/text event
C> - IPTR(19)- Pointer to start of bufr message
C> - IPTR(20)- Number of entries from table d
C> - IPTR(21)- Nr table b entries
C> - IPTR(22)- Nr table b entries from current message
C> - IPTR(23)- Code/flag table switch
C> - IPTR(24)- Aditional words added by text info
C> - IPTR(25)- Current bit number
C> - IPTR(26)- Data width change - add to table b width
C> - IPTR(27)- Data scale change - modifies table b scale
C> - IPTR(28)- Data reference value change - ?????????
C> - IPTR(29)- Add data associated field
C> - IPTR(30)- Signify characters
C> - IPTR(31)- Number of expanded descriptors in mstack
C> - IPTR(32)- Current descriptor segment f
C> - IPTR(33)- Current descriptor segment x
C> - IPTR(34)- Current descriptor segment y
C> - IPTR(35)- Data/descriptor replication in progress
C>  - 0  = No
C>  - 1  = Yes
C> - IPTR(36)- Next descriptor may be undecipherable
C> - IPTR(37)- Machine text type flag
C>  - 0  = EBCIDIC
C>  - 1  = ASCII
C> - IPTR(38)- Data/descriptor replication flag
C>  -  0 - Does not exist in current message
C>  - 1 - Exists in current message
C> - IPTR(39)- Delayed replication flag
C>  - 0 - No delayed replication
C>  - 1 - Message contains delayed replication
C> - IPTR(40)- Number of characters in text for curr descriptor
C> - IPTR(41)- Number of ancillary table b entries
C> - IPTR(42)- Number of ancillary table d entries
C> - IPTR(43)- Number of added table b entries encountered while
C> processing a bufr message. these entries only
C> exist durng processing of current bufr message
C> IPTR(44)- Bits per word
C> IPTR(45)- Bytes per word
C> @param[out] IDENT Array contains message information extracted from BUFR message:
C> - IDENT(1)  - Edition number (byte 4, section 1)
C> - IDENT(2)  - Originating center (bytes 5-6, section 1)
C> - IDENT(3)  - Update sequence (byte 7, section 1)
C> - IDENT(4)  - Optional section (byte 8, section 1)
C> - IDENT(5)  - Bufr message type (byte 9, section 1)
C>  -  0 = Surface data (land)
C>  -  1 = Surface data (ship)
C>  -  2 = Vertical soundings (other than satellite)
C>  -  3 = Vertical soundings (satellite)
C>  -  4 = Single lvl upper-air data(other than satellite)
C>  -  5 = Single level upper-air data (satellite)
C>  -  6 = Radar data
C>  -  7 = Synoptic features
C>  -  8 = Physical/chemical constituents
C>  -  9 = Dispersal and transport
C>  - 10 = Radiological data
C>  - 11 = Bufr tables (complete, replacement or update)
C>  - 12 = Surface data (satellite)
C>  - 21 = Radiances (satellite measured)
C>  - 31 = Oceanographic data
C> - IDENT(6)  - Bufr msg sub-type (byte 10, section 1)
C>              | TYPE | SBTYP |
C>              | :--- | :---- |
C>              | 2 | 7 = PROFILER |
C> - IDENT(7)  - (bytes 11-12, section 1)
C> - IDENT(8)  - Year of century (byte 13, section 1)
C> - IDENT(9)  - Month of year (byte 14, section 1)
C> - IDENT(10) - Day of month (byte 15, section 1)
C> - IDENT(11) - Hour of day (byte 16, section 1)
C> - IDENT(12) - Minute of hour (byte 17, section 1)
C> - IDENT(13) - Rsvd by adp centers(byte 18, section 1)
C> - IDENT(14) - Nr of data subsets (byte 5-6, section 3)
C> - IDENT(15) - Observed flag (byte 7, bit 1, section 3)
C> - IDENT(16) - Compression flag (byte 7, bit 2, section 3)
C> - IDENT(17) - Master table number(byte 4, section 1, ed 2 or gtr)
C> @param[out] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (Report number limited to value of input argument
C> maxr and parameter number limited to value of input
C> argument maxd)
C> @param[out] INDEX Pointer to available subset
C> @param KNR
C> @param LDATA
C> @param LSTACK
C>
C>  ===========================================================
C> Arrays containing data from table b
C> new - base arrays containing data from table b
C> - KFXY1 - Decimal descriptor value of f x y values
C> - ANAME1 - Descriptor name
C> - AUNIT1 - Units for descriptor
C> - ISCAL1 - Scale for value of descriptor
C> - IRFVL1 - Reference value for descriptor
C> - IWIDE1 - Bit width for value of descriptor
C>  ===========================================================
C> New - ancillary arrays containing data from table b
C> containing table b entries extracted
C> from type 11 bufr messages
C> - KFXY2 - Decimal descriptor value of f x y values
C> - ANAME2 - Descriptor name
C> - AUNIT2 - Units for descriptor
C> - ISCAL2 - Scale for value of descriptor
C> - IRFVL2 - Reference value for descriptor
C> - IWIDE2 - Bit width for value of descriptor
C>  ===========================================================
C> New - added arrays containing data from table b
C> containing table b entries extracted
C> from non-type 11 bufr messages
C> these exist for the life of current bufr message
C> - KFXY3 - Decimal descriptor value of f x y values
C> - ANAME3 - Descriptor name
C> - AUNIT3 - Units for descriptor
C> - ISCAL3 - Scale for value of descriptor
C> - IRFVL3 - Reference value for descriptor
C> - IWIDE3 - Bit width for value of descriptor
C>  ===========================================================
C>
C> Error returns:
C> IPTR(1)
C> - = 1 'BUFR' Not found in first 125 characters
C> - = 2 '7777' Not found in location determined by
C> by using counts found in each section. one or
C> more sections have an erroneous byte count or
C> characters '7777' are not in test message.
C> - = 3 Message contains a descriptor with f=0 that does
C> not exist in table b.
C> - = 4 Message contains a descriptor with f=3 that does
C> not exist in table d.
C> - = 5 Message contains a descriptor with f=2 with the
C> value of x outside the range 1-6.
C> - = 6 Descriptor element indicated to have a flag value
C> does not have an entry in the flag table.
C> (to be activated)
C> - = 7 Descriptor indicated to have a code value does
C> not have an entry in the code table.
C> (to be activated)
C> - = 8 Error reading table d
C> - = 9 Error reading table b
C> - = 10 Error reading code/flag table
C> - = 11 Descriptor 2 04 004 not followed by 0 31 021
C> - = 12 Data descriptor operator qualifier does not follow
C> delayed replication descriptor.
C> - = 13 Bit width on ascii characters not a multiple of 8
C> - = 14 Subsets = 0, no content bulletin
C> - = 20 Exceeded count for delayed replication pass
C> - = 21 Exceeded count for non-delayed replication pass
C> - = 22 Exceeded combined bit width, bit width > 32
C> - = 23 No element descriptors following 2 03 yyy
C> - = 27 Non zero lowest on text data
C> - = 28 Nbinc not nr of characters
C> - = 29 Table b appears to be damaged
C> - = 30 Table d entry with more than 18 in sequence
C> being entered from type 11 message
C> - = 99 No more subsets (reports) available in current
C> bufr mesage
C> - = 400 Number of subsets exceeds the value of input
C> argument maxr; must increase maxr to value of
C> ident(14) in calling program
C> - = 401 Number of parameters (and associated fields)
C> exceeds limits of this program.
C> - = 500 Value for nbinc has been found that exceeds
C> standard width plus any bit width change.
C> check all bit widths up to point of error.
C> - = 501 Corrected width for descriptor is 0 or less
C> - = 888 Non-numeric character in conversion request
C> - = 890 Class 0 element descriptor w/width of 0
C>
C> On the initial call to w3fi88 with a bufr message the argument
C> index must be set to zero (index = 0). on the return from w3fi88
C> 'index' will be set to the next available subset/report. when
C> there are no more subsets available a 99 err return will occur.
C>
C> If the original bufr message does not contain delayed replication
C> the bufr message will be completely decoded and 'index' will point
C> to the first decoded subset. the users will then have the option
C> of indexing through the subsets on their own or by recalling this
C> routine (without resetting 'index') to have the routine do the
C> indexing.
C>
C> If the original bufr message does contain delayed replication
C> one subset/report will be decoded at a time and passed back to
C> the user. this is not an option.
C>
C>  =============================================
C>   To use this routine
C>  =============================================
C>       the arrays to contain the output information are defined
C>       as follows:
C>
C>           KDATA(A,B)  is the a data entry  (integer value)
C>                       where a is the maximum number of reports/subsets
C>                       that may be contained in the bufr message (this
C>                       is now set to "maxr" which is passed as an input
C>                       argument to w3fi88), and where b is the maximum
C>                       number of descriptor combinations that may
C>                       be processed (this is now set to "maxd" which
C>                       is also passed as an input argument to w3fi88;
C>                       upper air data and some satellite data require
C>                       a value for maxd of 1700, but for most other
C>                       data a value for maxd of 500 will suffice)
C>           MSTACK(1,B) contains the descriptor that matches the
C>                       data entry (max. value for b is now "maxd"
C>                       which is passed as an input argument to w3fi88)
C>           MSTACK(2,B) is the scale (power of 10) to be applied to
C>                       the data (max. value for b is now "maxd"
C>                       which is passed as an input argument to w3fi88)
C>
      SUBROUTINE W3FI88(IPTR,IDENT,MSGA,ISTACK,MSTACK,KDATA,KNR,INDEX,
     *            LDATA,LSTACK,MAXR,MAXD,IUNITB,IUNITD)
C
C
C
C          THE MEMORY REQUIREMENTS FOR LSTACK AND LDATA ARE USED WITH
C          RUN-LINE CODING PROVIDING FOR THE HANDLING OF DATA FOR
C          GRAPHICS. I.E., RADAR DISPLAYS. IF THE DECODING PROCESS WILL
C          NOT BE USED TO PROCESS THOSE TYPE OF MESSAGES, THEN THE
C          VARIABLE SIZES FOR THE ARRAYS CAN BE MINIMIZED.
C          IF THE DECODING PROCESS WILL BE USED TO DECODE THOSE MESSAGE
C          TYPES, THEN MAXD MUST REFLECT THE MAXIMUM NUMBER OF
C          DESCRIPTORS (FULLY EXPANDED LIST) TO BE EXPECTED IN THE
C          MESSAGE.
C
      INTEGER        LDATA(MAXD)
      INTEGER        LSTACK(2,MAXD)
C
      INTEGER        MSGA(*)
      INTEGER        IPTR(*),KPTRB(16384),KPTRD(16384)
      INTEGER        KDATA(MAXR,MAXD)
      INTEGER        MSTACK(2,MAXD)
C
      INTEGER        IVALS(1000)
      INTEGER        KNR(MAXR)
      INTEGER        IDENT(*)
      INTEGER        ISTACK(*),IOLD11
cdak KEYSER fix 02/02/2001 VVVVV
      INTEGER        IOLDTB
cdak KEYSER fix 02/02/2001 AAAAA
      INTEGER        IWORK(15000)
      INTEGER        INDEX
C
      INTEGER        IIII
      CHARACTER*1    BLANK
      CHARACTER*4    DIRID(2)
C
      LOGICAL        SEC2
C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
      INTEGER        KFXY1(1300),ISCAL1(1300)
      INTEGER        IRFVL1(3,1300),IWIDE1(1300)
      CHARACTER*40   ANAME1(1300)
      CHARACTER*24   AUNIT1(1300)
C  ..................................................
C
C        NEW            ANCILLARY TABLE B FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        KFXY2(200),ISCAL2(200),IRFVL2(200),IWIDE2(200)
      CHARACTER*64   ANAME2(200)
      CHARACTER*24   AUNIT2(200)
C  ..................................................
C
C           NEW         ADDED TABLE B FROM NON-TYPE 11 BUFR MESSAGE
C
C     INTEGER        KFXY3(200),ISCAL3(200),IRFVL3(200),IWIDE3(200)
C     CHARACTER*64   ANAME3(200)
C     CHARACTER*24   AUNIT3(200)
C  ..................................................
C
C                       NEW BASE TABLE D
C
      INTEGER        ITBLD(20,400)
C  ..................................................
C
C                       ANCILLARY TABLE D FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        ITBLD2(20,50)
C  ..................................................
C
      SAVE

cdak KEYSER fix 02/02/2001 VVVVV
      DATA              IOLD11/0/
      DATA              IOLDTB/-99/
cdak KEYSER fix 02/02/2001 AAAAA
C
      CALL W3FI01(LW)
      IPTR(45) = LW
      IPTR(44) = LW * 8
C
      BLANK  = ' '
      IF (MOVA2I(BLANK).EQ.32) THEN
          IPTR(37) = 1
C         PRINT *,'ASCII MACHINE'
      ELSE
          IPTR(37) = 0
C         PRINT *,'EBCDIC MACHINE'
      END IF
C
C     PRINT *,' W3FI88 DECODER'
C                            INITIALIZE ERROR RETURN
      IPTR(1)   = 0
      IF (INDEX.GT.0) THEN
C                                 HAVE RE-ENTRY
          INDEX   = INDEX + 1
C         PRINT *,'RE-ENTRY LOOKING FOR SUBSET NR',INDEX
          IF (INDEX.GT.IDENT(14)) THEN
C                                 ALL SUBSETS PROCESSED
              IPTR(1)  = 99
              IPTR(38) = 0
              IPTR(39) = 0
          ELSE IF (INDEX.LE.IDENT(14)) THEN
              IF (IPTR(39).NE.0) THEN
                  DO 3000 J =1, IPTR(13)
                      IWORK(J)  = ISTACK(J)
 3000             CONTINUE
                  IPTR(12)  = IPTR(13)
                 CALL FI8801(IPTR,IDENT,MSGA,ISTACK,IWORK,KDATA,IVALS,
     *           MSTACK,KNR,INDEX,MAXR,MAXD,
     *           KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,IRF1SW,INEWVL,
     *           KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2,
     *           KFXY3,ANAME3,AUNIT3,ISCAL3,IRFVL3,IWIDE3,
     *           IUNITB,IUNITD,ITBLD,ITBLD2,KPTRB,KPTRD)
C
              END IF
          END IF
          RETURN
      ELSE
          INDEX  = 1
C         PRINT *,'INITIAL ENTRY FOR THIS BUFR MESSAGE'
      END IF
      IPTR(39)  = 0
C                        FIND 'BUFR' IN FIRST 125 CHARACTERS
      DO 1000 KNOFST = 0, 999, 8
          INOFST     = KNOFST
          CALL GBYTE (MSGA,IVALS,INOFST,8)
          IF (IVALS(1).EQ.66) THEN
              IPTR(19)   = INOFST
              INOFST     = INOFST + 8
              CALL GBYTE (MSGA,IVALS,INOFST,24)
              IF (IVALS(1).EQ.5588562) THEN
C                 PRINT *,'FOUND BUFR AT',IPTR(19)
                  INOFST     = INOFST + 24
                  GO TO 1500
              END IF
          END IF
 1000 CONTINUE
      PRINT *,'BUFR - START OF BUFR MESSAGE NOT FOUND'
      IPTR(1)    = 1
      RETURN
 1500 CONTINUE
      IDENT(1)   = 0
C                            TEST FOR EDITION NUMBER
C  ======================
      CALL GBYTE (MSGA,IDENT(1),INOFST+24,8)
C     PRINT *,'THIS IS AN EDITION',IDENT(1),' BUFR MESSAGE'
C
      IF (IDENT(1).GE.2) THEN
C                           GET TOTAL COUNT
          CALL GBYTE (MSGA,IVALS,INOFST,24)
          ITOTAL   = IVALS(1)
          KENDER   = ITOTAL * 8 - 32 + IPTR(19)
          CALL GBYTE (MSGA,ILAST,KENDER,32)
C         IF (ILAST.EQ.926365495) THEN
C             PRINT *,'HAVE TOTAL COUNT FROM SEC 0',IVALS(1)
C         END IF
          INOFST     = INOFST + 32
C                           GET SECTION 1 COUNT
          IPTR(3)  = INOFST
          CALL GBYTE (MSGA,IVALS,INOFST,24)
C         PRINT *,'SECTION 1 STARTS AT',INOFST,'  SIZE',IVALS(1)
          INOFST     = INOFST + 24
          IPTR( 2)   = IVALS(1)
C                           GET MASTER TABLE
          CALL GBYTE (MSGA,IVALS,INOFST,8)
          INOFST     = INOFST + 8
          IDENT(17)  = IVALS(1)
C         PRINT *,'BUFR MASTER TABLE NR',IDENT(17)
      ELSE
          IPTR(3)    = INOFST
C                           GET SECTION 1 COUNT
          CALL GBYTE (MSGA,IVALS,INOFST,24)
C         PRINT *,'SECTION 1 STARTS AT',INOFST,'  SIZE',IVALS(1)
          INOFST     = INOFST + 32
          IPTR( 2)   = IVALS(1)
      END IF
C  ======================
C                            ORIGINATING CENTER
      CALL GBYTE (MSGA,IVALS,INOFST,16)
      INOFST     = INOFST + 16
      IDENT(2)   = IVALS(1)
C                            UPDATE SEQUENCE
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      INOFST     = INOFST + 8
      IDENT(3)   = IVALS(1)
C                            OPTIONAL SECTION FLAG
      CALL GBYTE (MSGA,IVALS,INOFST,1)
      IDENT(4)   = IVALS(1)
      IF (IDENT(4).GT.0) THEN
          SEC2       = .TRUE.
      ELSE
C         PRINT *,'              NO OPTIONAL SECTION 2'
          SEC2       = .FALSE.
      END IF
      INOFST     = INOFST + 8
C                            MESSAGE TYPE
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(5)   = IVALS(1)
      INOFST     = INOFST + 8
C                            MESSAGE SUBTYPE
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(6)   = IVALS(1)
      INOFST     = INOFST + 8
cdak KEYSER fix 02/02/2001 VVVVV
      IF (IUNITB.NE.IOLDTB) THEN
C                IF HAVE A CHANGE IN TABLE B UNIT NUMBER , READ TABLE B
          IF(IOLDTB.NE.-99)  PRINT *, 'W3FI88 - NEW TABLE B UNIT NUMBER'
          IOLDTB = IUNITB
          IPTR(14)  = 0
          IPTR(21)  = 0
      END IF
cdak KEYSER fix 02/02/2001 AAAAA
C                            IF HAVE CHANGE IN DATA TYPE , RESET TABLE B
      IF (IOLD11.EQ.11) THEN
          IOLD11 = IDENT(5)
          IOLDSB = IDENT(6)
C                            JUST CONTINUE PROCESSING
      ELSE IF (IOLD11.NE.11) THEN
          IF (IDENT(5).EQ.11) THEN
              IOLD11  = IDENT(5)
              IPTR(21) = 0
          ELSE IF (IDENT(5).NE.IOLD11) THEN
              IOLD11  = IDENT(5)
              IPTR(21) = 0
          ELSE IF (IDENT(5).EQ.IOLD11) THEN
C                      IF HAVE A CHANGE IN SUBTYPE, RESET TABLE B
              IF (IOLDSB.NE.IDENT(6)) THEN
                  IOLDSB  = IDENT(6)
                  IPTR(21)  = 0
C             ELSE IF
              END IF
          END IF
      END IF
C                           IF BUFR EDITION 0 OR 1 THEN
C                                 NEXT 2 BYTES ARE BUFR TABLE VERSION
C                           ELSE
C                               BYTE 11 IS VER NR OF MASTER TABLE
C                               BYTE 12 IS VER NR OF LOCAL TABLE
      IF (IDENT(1).LT.2) THEN
          CALL GBYTE (MSGA,IVALS,INOFST,16)
          IDENT(7)   = IVALS(1)
          INOFST     = INOFST + 16
      ELSE
C                               BYTE 11 IS VER NR OF MASTER TABLE
          CALL GBYTE (MSGA,IVALS,INOFST,8)
          IDENT(18)  = IVALS(1)
          INOFST     = INOFST + 8
C                               BYTE 12 IS VER NR OF LOCAL TABLE
          CALL GBYTE (MSGA,IVALS,INOFST,8)
          IDENT(19)  = IVALS(1)
          INOFST     = INOFST + 8

      END IF
C                            YEAR OF CENTURY
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(8)   = IVALS(1)
      INOFST     = INOFST + 8
C                            MONTH
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(9)   = IVALS(1)
      INOFST     = INOFST + 8
C                            DAY
C     PRINT *,'DAY AT ',INOFST
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(10)  = IVALS(1)
      INOFST     = INOFST + 8
C                            HOUR
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(11)  = IVALS(1)
      INOFST     = INOFST + 8
C                            MINUTE
      CALL GBYTE (MSGA,IVALS,INOFST,8)
      IDENT(12)  = IVALS(1)
C                            RESET POINTER (INOFST) TO START OF
C                                NEXT SECTION
C                                (SECTION 2 OR SECTION 3)
      INOFST     = IPTR(3) + IPTR(2) * 8
      IPTR(4)    = 0
      IPTR(5)    = INOFST
      IF (SEC2) THEN
C                            SECTION 2 COUNT
          CALL GBYTE (MSGA,IPTR(4),INOFST,24)
          INOFST   = INOFST + 32
C         PRINT *,'SECTION 2 STARTS AT',INOFST,' BYTES=',IPTR(4)
          KENTRY  = (IPTR(4) - 4) / 14
C         PRINT *,'SHOULD BE A MAX OF',KENTRY,' REPORTS'
          IF (IDENT(2).EQ.7) THEN
              DO 2000 I = 1, KENTRY
                  CALL GBYTE (MSGA,KDSPL ,INOFST,16)
                  INOFST  = INOFST + 16
                  CALL GBYTE (MSGA,LAT   ,INOFST,16)
                  INOFST  = INOFST + 16
                  CALL GBYTE (MSGA,LON   ,INOFST,16)
                  INOFST  = INOFST + 16
                  CALL GBYTE (MSGA,KDAHR ,INOFST,16)
                  INOFST  = INOFST + 16
                  CALL GBYTE (MSGA,DIRID(1),INOFST,32)
                  INOFST  = INOFST + 32
                  CALL GBYTE (MSGA,DIRID(2),INOFST,16)
                  INOFST  = INOFST + 16
C                 PRINT *,KDSPL,LAT,LON,KDAHR,DIRID(1),DIRID(2)
 2000         CONTINUE
          END IF
C                            RESET POINTER (INOFST) TO START OF
C                                SECTION 3
          INOFST     = IPTR(5) + IPTR(4) * 8
      END IF
C                            BIT OFFSET TO START OF SECTION 3
      IPTR( 7)   = INOFST
C                            SECTION 3 COUNT
      CALL GBYTE (MSGA,IPTR(6),INOFST,24)
C     PRINT *,'SECTION 3 STARTS AT',INOFST,' BYTES=',IPTR(6)
      INOFST     = INOFST + 24
C                           SKIP RESERVED BYTE
      INOFST     = INOFST + 8
C                            NUMBER OF DATA SUBSETS
      CALL GBYTE (MSGA,IDENT(14),INOFST,16)
C
      IF (IDENT(14).GT.MAXR) THEN
          PRINT *,'THE NUMBER OF SUBSETS EXCEEDS THE MAXIMUM OF',MAXR
          PRINT *,'PASSED INTO W3FI88; MAXR MUST BE INCREASED IN '
          PRINT *,'THE CALLING PROGRAM TO AT LEAST THE VALUE OF'
          PRINT *,IDENT(14),'TO BE ABLE TO PROCESS THIS DATA'
C
          IPTR(1)  = 400
          RETURN
      END IF
      INOFST     = INOFST + 16
C                            OBSERVED DATA FLAG
      CALL GBYTE (MSGA,IVALS,INOFST,1)
      IDENT(15)  = IVALS(1)
      INOFST     = INOFST + 1
C                            COMPRESSED DATA FLAG
      CALL GBYTE (MSGA,IVALS,INOFST,1)
      IDENT(16)  = IVALS(1)
      INOFST     = INOFST + 7
C                            CALCULATE NUMBER OF DESCRIPTORS
      NRDESC     = (IPTR( 6) - 8) / 2
      IPTR(12)   = NRDESC
      IPTR(13)   = NRDESC
C                            EXTRACT DESCRIPTORS
      CALL GBYTES (MSGA,ISTACK,INOFST,16,0,NRDESC)
C     PRINT *,'INITIAL DESCRIPTOR LIST OF',NRDESC,' DESCRIPTORS'
      DO 10 L = 1, NRDESC
          IWORK(L)   = ISTACK(L)
C         PRINT *,L,ISTACK(L)
   10 CONTINUE
      IPTR(13)   = NRDESC
C  ===============================================================
C
C                            CONSTRUCT A TABLE B TO MATCH THE
C                            LIST OF DESCRIPTORS FOR THIS MESSAGE
C
      IF (IPTR(21).EQ.0) THEN
          PRINT *,'W3FI88- TABLE B NOT YET ENTERED'
          CALL FI8812(IPTR,IUNITB,IUNITD,ISTACK,NRDESC,KPTRB,KPTRD,
     *                     IRF1SW,NEWREF,ITBLD,ITBLD2,
     *                     KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
     *                     KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2)
      ELSE
C         PRINT *,'W3FI88- TABLE B ALL READY IN PLACE'
          IF (IPTR(41).NE.0) THEN
C             PRINT *,'MERGE',IPTR(41),' ENTRIES INTO TABLE B'
C             CALL FI8818(IPTR,KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
C    *                 KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2,KPTRB)
          END IF
      END IF
      IF (IPTR(1).NE.0) RETURN
C ================================================================
C                            RESET POINTER TO START OF SECTION 4
      INOFST     = IPTR(7) + IPTR(6) * 8
C                            BIT OFFSET TO START OF SECTION 4
      IPTR( 9)   = INOFST
C                            SECTION 4 COUNT
      CALL GBYTE (MSGA,IVALS,INOFST,24)
C     PRINT *,'SECTION 4 STARTS AT',INOFST,' VALUE',IVALS(1)
      IPTR( 8)   = IVALS(1)
      INOFST     = INOFST + 32
C                            SET FOR STARTING BIT OF DATA
      IPTR(25)   = INOFST
C                            FIND OUT IF '7777' TERMINATOR IS THERE
      INOFST     = IPTR(9) + IPTR(8) * 8
      CALL GBYTE  (MSGA,IVALS,INOFST,32)
C     PRINT *,'SECTION 5 STARTS AT',INOFST,' VALUE',IVALS(1)
      IF (IVALS(1).NE.926365495) THEN
          PRINT *,'BAD SECTION COUNT'
          IPTR(1)     = 2
          RETURN
      ELSE
          IPTR(1)     = 0
      END IF
C
      CALL FI8801(IPTR,IDENT,MSGA,ISTACK,IWORK,KDATA,IVALS,
     *           MSTACK,KNR,INDEX,MAXR,MAXD,
     *           KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,IRF1SW,INEWVL,
     *           KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2,
     *           KFXY3,ANAME3,AUNIT3,ISCAL3,IRFVL3,IWIDE3,
     *           IUNITB,IUNITD,ITBLD,ITBLD2,KPTRB,KPTRD)
C
C     PRINT *,'HAVE RETURNED FROM FI8801'
      IF (IPTR(1).NE.0) THEN
          RETURN
      END IF
C                FURTHER PROCESSING REQUIRED FOR PROFILER DATA
      IF (IDENT(5).EQ.2) THEN
          IF (IDENT(6).EQ.7) THEN
C             PRINT *,'REFORMAT PROFILER DATA'
C
C             DO 7151 I = 1, 40
C                 IF (I.LE.20) THEN
C                     PRINT *,'IPTR(',I,')=',IPTR(I),
C    *                                       ' IDENT(',I,')= ',IDENT(I)
C                 ELSE
C                     PRINT *,'IPTR(',I,')=',IPTR(I)
C                 END IF
C7151         CONTINUE
C             DO 152 I = 1, IPTR(31)
C                 PRINT *,MSTACK(1,I),MSTACK(2,I),(KDATA(J,I),J=1,5)
C 152         CONTINUE
              IF (IDENT(1).LT.2) THEN
                  CALL FI8809(IDENT,MSTACK,KDATA,IPTR,MAXR,MAXD)
              ELSE
                  CALL FI8810(IDENT,MSTACK,KDATA,IPTR,MAXR,MAXD)
              END IF
C             DO 151 I = 1, 40
C                 IF (I.LE.20) THEN
C                     PRINT *,'IPTR(',I,')=',IPTR(I),
C    *                                       ' IDENT(',I,')= ',IDENT(I)
C                 ELSE
C                     PRINT *,'IPTR(',I,')=',IPTR(I)
C                 END IF
C 151         CONTINUE
              IF (IPTR(1).NE.0) THEN
                  RETURN
              END  IF
C
C             DO 154 I = 1, IPTR(31)
C                PRINT *,I,MSTACK(1,I),MSTACK(2,I),KDATA(1,I),KDATA(2,I)
C 154         CONTINUE
          END IF
      END IF
C                        IF DATA/DESCRIPTOR REPLICATION FLAG IS ON,
C                         MUST COMPLETE EXPANSION OF DATA AND
C                          DESCRIPTORS.
      IF (IPTR(38).EQ.1) THEN
          CALL FI8811(IPTR,IDENT,MSTACK,KDATA,KNR,
     *                  LDATA,LSTACK,MAXD,MAXR)
      END IF
C
C                        IF HAVE A LIST OF TABLE ENTRIES FROM
C                        A BUFR MESSAGE TYPE 11
C                        PRINT OUT THE ENTRIES
C
      IF (IDENT(5).EQ.11) THEN
C         DO 100 I = 1, IPTR(31)+IPTR(24)
C             PRINT *,I,MSTACK(1,I),(KDATA(J,I),J=1,4)
C 100     CONTINUE
          CALL FI8813 (IPTR,MAXR,MAXD,MSTACK,KDATA,IDENT,KPTRD,KPTRB,
     *         ITBLD,ANAME1,AUNIT1,KFXY1,ISCAL1,IRFVL1,IWIDE1,IUNITB)
      END IF
      RETURN
      END
C> @brief Data extraction
C> @author Bill Cavanaugh @date 1988-09-01

C> Control the extraction of data from section 4 based on data descriptors.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-09-01\
C> - Bill Cavanaugh 1991-01-18 Corrections to properly handle non-compressed
C>                         DATA.
C> - Bill Cavanaugh 1991-09-23 Coding added to handle single subsets with
C>                         DELAYED REPLICATION.
C> - Bill Cavanaugh 1992-01-24 Modified to echo descriptors to mstack(1,n)
C> - Dennis Keyser 1995-06-07 Corrected an error which required input
C> argument "maxd" to be nearly twice as large
C> as needed for decoding wind profiler reports
C> (limit upper bound for "iwork" array was set
C> to "maxd", now it is set to 15000)
C>
C> @param[in] IPTR See w3fi88() routine docblock
C> @param[in] IDENT See w3fi88() routine docblock
C> @param[in] MSGA Array containing bufr message
C> @param[inout] ISTACK Original array of descriptors extracted from
C> source bufr message.
C> @param[in] MSTACK Working array of descriptors (expanded)and scaling
C> factor
C> @param[inout] KFXY1+KFXY2+KFXY3 Image of current descriptor
C> @param[in] INDEX
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[in] IUNITB Unit number of data set holding table b
C> @param[in] IUNITD Unit number of data set holding table d
C> @param[out] IWORK Working descriptor list
C> @param[out] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C>
C> arrays containing data from table b
C> @param[out] AUNIT1+AUNIT2+AUNIT3 Units for descriptor
C> @param[out] ANAME1+ANAME2+ANAME3 Descriptor name
C> @param[out] ISCAL1+ISCAL2+ISCAL3 Scale for value of descriptor
C> @param[out] IRFVL1+IRFVL2+IRFVL3 Reference value for descriptor
C> @param[out] IWIDE1+IWIDE2+IWIDE3 Bit width for value of descriptor
C> @param ITBLD+ITBLD2
C> @param KPTRB
C> @param KPTRD
C> @param KNR
C> @param IVALS
C> @param IRF1SW
C> @param INEWVL
C>
C> Error return:
C> - IPTR(1)
C>  - = 8 Error reading table b
C>  - = 9 Error reading table d
C>  - = 11 Error opening table b
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8801(IPTR,IDENT,MSGA,ISTACK,IWORK,KDATA,IVALS,
     *        MSTACK,KNR,INDEX,MAXR,MAXD,
     *        KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,IRF1SW,INEWVL,
     *        KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2,
     *        KFXY3,ANAME3,AUNIT3,ISCAL3,IRFVL3,IWIDE3,
     *        IUNITB,IUNITD,ITBLD,ITBLD2,KPTRB,KPTRD)
C

C  ..................................................
C
C        NEW            ANCILLARY TABLE B FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        KFXY2(*),ISCAL2(*),IRFVL2(*),IWIDE2(*)
      CHARACTER*64   ANAME2(*)
      CHARACTER*24   AUNIT2(*)
C  ..................................................
C
C           NEW         ADDED TABLE B FROM NON-TYPE 11 BUFR MESSAGE
C
      INTEGER        KFXY3(200),ISCAL3(200),IRFVL3(200),IWIDE3(200)
      CHARACTER*64   ANAME3(200)
      CHARACTER*24   AUNIT3(200)
C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
      INTEGER        KFXY1(*),ISCAL1(*),IRFVL1(3,*),IWIDE1(*)
      CHARACTER*40   ANAME1(*)
      CHARACTER*24   AUNIT1(*)
C  ..................................................
C
C                       ANCILLARY TABLE D FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        ITBLD2(20,*)
C  ..................................................
C
C                       NEW BASE TABLE D
C
      INTEGER        ITBLD(20,*)
C  ..................................................
C
C
      INTEGER        MAXD, MAXR
C
      INTEGER        MSGA(*),KDATA(MAXR,MAXD),IVALS(*)
C
      INTEGER        KNR(MAXR)
      INTEGER        LX,LY,LL,J
C     INTEGER        IHOLD(33)
      INTEGER        IPTR(*),KPTRB(*),KPTRD(*)
      INTEGER        IDENT(*)
      INTEGER        ISTACK(*),IWORK(*)
C
      INTEGER        MSTACK(2,MAXD)
C
      INTEGER        JDESC
      INTEGER        INDEX
C
      SAVE
C
C     PRINT *,' DECOLL FI8801'
      IF (INDEX.GT.1) THEN
              GO TO 1000
      END IF
C                      ---------  DECOLL  ---------------
      IPTR(23)    = 0
      IPTR(26)    = 0
      IPTR(27)    = 0
      IPTR(28)    = 0
      IPTR(29)    = 0
      IPTR(30)    = 0
      IPTR(36)    = 0
C                            INITIALIZE OUTPUT AREA
C                              SET POINTER TO BEGINNING OF DATA
C                                   SET BIT
      IPTR(17)   = 1
 1000 CONTINUE
C     IPTR(12)   = IPTR(13)
      LL         = 0
      IPTR(11)   = 1
      IF (IPTR(10).EQ.0) THEN
C                              RE-ENTRY POINT FOR MULTIPLE
C                                 NON-COMPRESSED REPORTS
      ELSE
          INDEX     = IPTR(15)
          IPTR(17)  = INDEX
          IPTR(25)  = IPTR(10)
          IPTR(10)  = 0
          IPTR(15)  = 0
      END IF
C     PRINT *,'FI8801 - RPT',IPTR(17),'  STARTS AT',IPTR(25)
      IPTR(24)  = 0
      IPTR(31)  = 0
C                              POINTING AT NEXT AVAILABLE DESCRIPTOR
      MM         = 0
      IF (IPTR(21).EQ.0) THEN
          NRDESC  = IPTR(13)
          CALL FI8812(IPTR,IUNITB,IUNITD,ISTACK,NRDESC,KPTRB,KPTRD,
     *                     IRF1SW,NEWREF,ITBLD,ITBLD2,
     *                     KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
     *                     KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2)
      END IF
   10     CONTINUE
C                              PROCESS THRU THE FOLLOWING
C                              DEPENDING UPON THE VALUE OF 'F' (LF)
          MM     = MM + 1
   12     CONTINUE
          IF (MM.GT.MAXD) THEN
              GO TO 200
          END IF
C                            END OF CYCLE TEST (SERIAL/SEQUENTIAL)
          IF (IPTR(11).GT.IPTR(12)) THEN
C             PRINT *,' HAVE COMPLETED REPORT SEQUENCE'
              IF (IDENT(16).NE.0) THEN
C                 PRINT *,' PROCESSING COMPRESSED REPORTS'
C                            REFORMAT DATA FROM DESCRIPTOR
C                            FORM TO USER FORM
                  RETURN
              ELSE
C                 WRITE (6,1)
C   1             FORMAT (1H1)
C                 PRINT *,' PROCESSED SERIAL REPORT',IPTR(17),IPTR(25)
                  IPTR(17) = IPTR(17) + 1
                  IF (IPTR(17).GT.IDENT(14)) THEN
                      IPTR(17)  = IPTR(17) - 1
                      GO TO 200
                  END IF
                  DO 300 I = 1, IPTR(13)
                      IWORK(I)  = ISTACK(I)
  300             CONTINUE
C                             RESET POINTERS
                  LL       = 0
                  IPTR(1)   = 0
                  IPTR(11)  = 1
                  IPTR(12)  = IPTR(13)
C                         IS THIS LAST REPORT ?
C                 PRINT *,'READY',IPTR(39),INDEX
                  IF (IPTR(39).GT.0) THEN
                      IF (INDEX.GT.0) THEN
C                         PRINT *,'HERE IS SUBSET NR',INDEX
                          RETURN
                      END IF
                  END IF
                  GO TO 1000
              END IF
          END IF
   14     CONTINUE
C                               GET NEXT DESCRIPTOR
          CALL FI8808 (IPTR,IWORK,LF,LX,LY,JDESC)
C         PRINT *,IPTR(11)-1,'JDESC= ',JDESC,' AND NEXT ',
C    *                       IPTR(11),IWORK(IPTR(11)),IPTR(31)
C         PRINT *,IPTR(11)-1,'DESCRIPTOR',JDESC,LF,LX,LY,
C    *       ' FOR LOC',IPTR(17),IPTR(25)
CVVVVVCHANGE#2 FIX BY KEYSER  -- 12/06/1994
C  NOTE: THIS FIX NEEDED BECAUSE IWORK ARRAY DOES NOT HAVE TO BE
C        LIMITED TO SIZE OF "MAXD" -- WASTES SPACE BECAUSE "MAXD"
C        MUST BECOME OVER TWICE AS LARGE AS NEEDED FOR PROFILERS
C        IN ORDER TO AVOID SATISFYING THIS BELOW IF TEST
CDAK      IF (IPTR(11).GT.MAXD) THEN
          IF (IPTR(11).GT.15000) THEN
CAAAAACHANGE#2 FIX BY KEYSER -- 12/06/1994
              IPTR(1)  = 401
              RETURN
          END IF
C
              KPRM        = IPTR(31) + IPTR(24)
              IF (KPRM.GT.MAXD) THEN
                  IF (KPRM.GT.KOLD) THEN
                      PRINT *,'EXCEEDED  ARRAY SIZE',KPRM,IPTR(31),
     *                        IPTR(24)
                      KOLD  = KPRM
                  END IF
              END IF
C                          REPLICATION PROCESSING
          IF (LF.EQ.1) THEN
C                                        ----------  F1  ---------
              IPTR(31)    = IPTR(31) + 1
              KPRM        = IPTR(31) + IPTR(24)
              MSTACK(1,KPRM)       = JDESC
              MSTACK(2,KPRM)       = 0
              KDATA(IPTR(17),KPRM) = 0
C             PRINT *,'FI8801-1',KPRM,MSTACK(1,KPRM),
C    *                       MSTACK(2,KPRM),KDATA(IPTR(17),KPRM)
              CALL FI8805(IPTR,IDENT,MSGA,IWORK,LX,LY,
     *                       KDATA,LL,KNR,MSTACK,MAXR,MAXD)
C    *                              KDATA,LL,KNR,MSTACK,MAXR,MAXD)
              IF (IPTR(1).NE.0) THEN
                  RETURN
              ELSE
                  GO TO 12
              END IF
C
C                             DATA DESCRIPTION OPERATORS
          ELSE IF (LF.EQ.2)THEN
              IF (LX.EQ.4) THEN
                  IPTR(31)       = IPTR(31) + 1
                  KPRM           = IPTR(31) + IPTR(24)
                  MSTACK(1,KPRM) = JDESC
                  MSTACK(2,KPRM) = 0
                  KDATA(IPTR(17),KPRM) = 0
C                 PRINT *,'FI8801-2',KPRM,MSTACK(1,KPRM),
C    *                       MSTACK(2,KPRM),KDATA(IPTR(17),KPRM)
              END IF
              CALL FI8806 (IPTR,LX,LY,IDENT,MSGA,KDATA,IVALS,MSTACK,
     *          IWIDE1,IRFVL1,ISCAL1,J,LL,KFXY1,IWORK,JDESC,MAXR,MAXD,
     *          KPTRB)
              IF (IPTR(1).NE.0) THEN
                  RETURN
              END IF
              GO TO 12
C                              DESCRIPTOR SEQUENCE STRINGS
          ELSE IF (LF.EQ.3) THEN
C             PRINT *,'F3  SEQUENCE DESCRIPTOR'
C                                  READ IN TABLE D, BUT JUST ONCE
              IF (IPTR(20).EQ.0) THEN
                  CALL FI8820 (ITBLD,IUNITD,IPTR,ITBLD2,KPTRD)
                  IF (IPTR(1).GT.0) THEN
                       RETURN
                  END IF
C             ELSE
C                 IF (IPTR(42).NE.0) THEN
C                     PRINT *,'MERGE',IPTR(42),' ENTRIES INTO TABLE D'
C                     CALL FI8819(IPTR,ITBLD,ITBLD2,KPTRD)
C                 END IF
              END IF
              CALL FI8807(IPTR,IWORK,ITBLD,ITBLD2,JDESC,KPTRD)
              IF (IPTR(1).GT.0) THEN
                   RETURN
              END IF
              GO TO 14
C
C                              ELEMENT DESCRIPTOR PROCESSING
C
          ELSE
              KPRM           = IPTR(31) + IPTR(24)
              CALL FI8802(IPTR,IDENT,MSGA,KDATA,KFXY1,LL,MSTACK,
     *          AUNIT1,IWIDE1,IRFVL1,ISCAL1,JDESC,IVALS,J,MAXR,MAXD,
     *          KPTRB)
C                             TURN OFF SKIP FLAG AFTER STD DESCRIPTOR
              IPTR(36)   = 0
              IF (IPTR(1).GT.0) THEN
                  RETURN
              ELSE
C
C                               IF ENCOUNTER CLASS 0 DESCRIPTOR
C                                  NOT CONTAINED WITHIN A BUFR
C                                  MESSAGE OF TYPE 11, THEN COLLECT
C                                  ALL TABLE B ENTRIES FOR USE ON
C                                  CURRENT BUFR MESSAGE
C
                  IF (JDESC.LE.20.AND.JDESC.GE.10) THEN
                      IF (IDENT(5).NE.11) THEN
C                                     COLLECT TABLE B ENTRIES
                          CALL FI8815(IPTR,IDENT,JDESC,KDATA,
     *                                KFXY3,MAXR,MAXD,ANAME3,AUNIT3,
     *                                ISCAL3,IRFVL3,IWIDE3,
     *                                KEYSET,IBFLAG,IERR)
                          IF (IERR.NE.0) THEN
                          END IF
                          IF (IAND(IBFLAG,16).NE.0) THEN
                            IF (IAND(IBFLAG,8).NE.0) THEN
                              IF (IAND(IBFLAG,4).NE.0) THEN
                                IF (IAND(IBFLAG,2).NE.0) THEN
                                  IF (IAND(IBFLAG,1).NE.0) THEN
C                                          HAVE A COMPLETE TABLE B ENTRY
                                    IPTR(43) = IPTR(43) + IDENT(14)
                                    KEYSET  = 0
                                    IBFLAG  = 0
                                    GO TO 1000
                                  END IF
                                END IF
                              END IF
                            END IF
                          END IF
                      END IF
                  END IF
                  IF (IDENT(16).EQ.0) THEN
                      KNR(IPTR(17))  = IPTR(31)
                  ELSE
                      DO 310 KJ = 1, MAXR
                          KNR(KJ) = IPTR(31)
  310                 CONTINUE
                  END IF
                  GO TO 10
              END IF
          END IF
C     END IF
C         END DO WHILE
  200 CONTINUE
C     IF (IDENT(16).NE.0) THEN
C         PRINT *,'RETURN WITH',IDENT(14),' COMPRESSED REPORTS'
C     ELSE
C         PRINT *,'RETURN WITH',IPTR(17),' NON-COMPRESSED REPORTS'
C     END IF
      RETURN
      END
C> @brief Process element descriptor.
C> @author Bill Cavanaugh @date 1988-09-01

C> Process an element descriptor (f = 0) and store data
C> in output array.
C>
C> Program history log:
C>   88-09-01
C>   91-04-04 Changed to pass width of text fields in bytes
C>
C> @param[in] IPTR See w3fi88 routine docblock
C> @param[in] IDENT See w3fi88 routine docblock
C> @param[in] MSGA Array containing bufr message
C> @param[inout] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C> @param[inout] KFXY1 Image of current descriptor
C> @param[in] MSTACK
C> @param[in] MAXR Maximum number of reports/subsets that may be contained in
C> a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> arrays containing data from table b
C> @param[out] AUNIT1 Units for descriptor
C> @param[out] ISCAL1 Scale for value of descriptor
C> @param[out] IRFVL1 Reference value for descriptor
C> @param[out] IWIDE1 Bit width for value of descriptor
C> @param LL
C> @param JDESC
C> @param IVALS
C> @param J
C> @param KPTRB
C>
C> Error return:
C> IPTR(1) = 3 - Message contains a descriptor with f=0 that does not exist
C> in table b.
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8802(IPTR,IDENT,MSGA,KDATA,KFXY1,LL,MSTACK,AUNIT1,
     *      IWIDE1,IRFVL1,ISCAL1,JDESC,IVALS,J,MAXR,MAXD,KPTRB)

C                                          TABLE B ENTRY
      CHARACTER*24   ASKEY
      INTEGER        MSGA(*)
      INTEGER        IPTR(*)
      INTEGER        KPTRB(*)
      INTEGER        IDENT(*)
      INTEGER        J
      INTEGER        JDESC
      INTEGER        MSTACK(2,MAXD)
      INTEGER        KDATA(MAXR,MAXD),IVALS(*)
C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
      INTEGER        KFXY1(*),ISCAL1(*),IRFVL1(3,*),IWIDE1(*)
C     CHARACTER*40   ANAME1(*)
      CHARACTER*24   AUNIT1(*)
C  ..................................................
      SAVE
C
      DATA  ASKEY /'CCITT IA5               '/
C
C     PRINT *,' FI8802 - ELEMENT DESCRIPTOR ',JDESC,KPTRB(JDESC)
C                                      FIND TABLE B ENTRY
      J  = KPTRB(JDESC)
C                                   HAVE A MATCH
C                                        SET FLAG IF TEXT EVENT
C     PRINT *,'ASKEY=',ASKEY,'AUNIT1(',J,')=',AUNIT1(J),JDESC
      IF (ASKEY(1:9).EQ.AUNIT1(J)(1:9)) THEN
          IPTR(18)     = 1
          IPTR(40)     = IWIDE1(J) / 8
      ELSE
          IPTR(18)     = 0
      END IF
C     PRINT *,'FI8802 - BIT WIDTH =',IWIDE1(J),IPTR(18),' FOR',JDESC
      IF (IDENT(16).NE.0) THEN
C                                   COMPRESSED
          CALL FI8803(IPTR,IDENT,MSGA,KDATA,IVALS,MSTACK,
     *                IWIDE1,IRFVL1,ISCAL1,J,JDESC,MAXR,MAXD)
C         IF (IPTR(1).NE.0) THEN
C             RETURN
C         END IF
      ELSE
C                                   NOT COMPRESSED
C         PRINT *,' FROM FI8802',J
          CALL FI8804(IPTR,MSGA,KDATA,IVALS,MSTACK,
     *                  IWIDE1,IRFVL1,ISCAL1,J,LL,JDESC,MAXR,MAXD)
C         IF (IPTR(1).NE.0) THEN
C             RETURN
C         END IF
      END IF
      RETURN
      END
C> @brief Process compressed data
C> @author Bill Cavanaugh @date 1988-09-01

C> Process compressed data and place individual elements
C> into output array.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-09-01
C> - Bill Cavanaugh 1991-04-04 Text handling portion of this routine
C> modified to hanle width of fields in bytes.
C> - Bill Cavanaugh 1991-04-17 Tests showed that the same data in compressed
C> and uncompressed form gave different results.
C> this has been corrected.
C> - Bill Cavanaugh 1991-06-21 Processing of text data has been changed to
C> provide exact reproduction of all characters.
C> - Bill Cavanaugh 1994-04-11 Corrected processing of data when all values
C> the same (nbinc = 0). corrected test of lowest
C> value against proper bit mask.
C> - Dennis Keyser 1995-06-07 Corrected an error which resulted in
C> returned scale in "mstack(2, ..)" always
C> being set to zero for compressed data.  also,
C> scale changes were not being recognized.
C>
C> @param[in] IPTR See w3fi88 routine docblock
C> @param[in] IDENT See w3fi88 routine docblock
C> @param[in] MSGA Array containing bufr message,mstack,
C> @param[in] IVALS Array of single parameter values
C> @param[inout] J
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[out] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C> arrays containing data from table b
C> @param[out] ISCAL1 Scale for value of descriptor
C> @param[out] IRFVL1 Reference value for descriptor
C> @param[out] IWIDE1 Bit width for value of descriptor
C> @param MSTACK
C> @param JDESC
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8803(IPTR,IDENT,MSGA,KDATA,IVALS,MSTACK,
     *                      IWIDE1,IRFVL1,ISCAL1,J,JDESC,MAXR,MAXD)

C
C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
C     INTEGER        KFXY1(*)
      INTEGER        ISCAL1(*)
      INTEGER        IRFVL1(3,*)
      INTEGER        IWIDE1(*)
C     CHARACTER*40   ANAME1(*)
C     CHARACTER*24   AUNIT1(*)
C  ..................................................
      INTEGER        MAXD,MAXR
      INTEGER        MSGA(*),JDESC,MSTACK(2,MAXD)
      INTEGER        IPTR(*),IVALS(*),KDATA(MAXR,MAXD)
      INTEGER        NRVALS,JWIDE,IDATA
      INTEGER        IDENT(*)
      INTEGER        J
      INTEGER        KLOW(256)
C
      LOGICAL        TEXT
C
      INTEGER        MSK(32)
C
      SAVE
C
      DATA   MSK  /1,  3,  7,  15,  31,  63,  127,
C                  1   2   3   4    5    6    7
     *             255,  511,  1023,  2047,  4095,
C                  8     9     10     11     12
     *             8191,  16383,  32767,  65535,
C                  13     14      15      16
     *             131071,  262143,  524287,
C                  17       18       19
     *             1048575, 2097151,  4194303,
C                  20       21        22
     *             8388607,  16777215,  33554431,
C                  23        24         25
     *             67108863,  134217727,  268435455,
C                  26         27          28
     *             536870911, 1073741823, 2147483647,-1     /
C                  29         30          31          32
      CALL W3FI01(LW)
      MWDBIT  = IPTR(44)
      IF (IPTR(45).EQ.8) THEN
          I        = 2147483647
          MSK(32)  = I + I + 1
      END IF
C
C     PRINT *,' FI8803  COMPR    J=',J,' IWIDE1(J) =',IWIDE1(J),
C    *             ' EXTRA BITS =',IPTR(26),' START AT',IPTR(25)
      IF (IPTR(18).EQ.0) THEN
          TEXT    = .FALSE.
      ELSE
          TEXT    = .TRUE.
      END IF
C     PRINT *,'DESCRIPTOR',KPRM,JDESC
      IF (.NOT.TEXT) THEN
          IF (IPTR(29).GT.0.AND.JDESC.NE.7957) THEN
C             PRINT *,'ASSOCIATED FIELD AT',IPTR(25)
C                        WORKING WITH ASSOCIATED FIELDS HERE
              IPTR(31)   = IPTR(31) + 1
              KPRM       = IPTR(31) + IPTR(24)
C                        GET LOWEST
              CALL GBYTE (MSGA,LOWEST,IPTR(25),IPTR(29))
              IPTR(25)   = IPTR(25) + IPTR(29)
C                        GET NBINC
              CALL GBYTE (MSGA,NBINC,IPTR(25),6)
              IPTR(25)   = IPTR(25) + 6
C             PRINT *,'LOWEST=',LOWEST,' NBINC=',NBINC
              IF (NBINC.GT.32) THEN
                  IPTR(1)  = 22
                  RETURN
              END IF
C                        EXTRACT DATA FOR ASSOCIATED FIELD
              IF (NBINC.GT.0) THEN
                  CALL GBYTES (MSGA,IVALS,IPTR(25),NBINC,0,IPTR(21))
                  IPTR(25)   = IPTR(25) + NBINC * IPTR(21)
                  DO 50 I = 1, IDENT(14)
                      KDATA(I,KPRM) = IVALS(I) + LOWEST
                      IF (NBINC.EQ.32) THEN
                          IF (KDATA(I,KPRM).EQ.MSK(NBINC)) THEN
                              KDATA(I,KPRM) = 999999
                          END IF
                      ELSE IF (KDATA(I,KPRM).GE.MSK(NBINC)) THEN
                          KDATA(I,KPRM) = 999999
                      END IF
   50             CONTINUE
              ELSE
                  DO 51 I = 1, IDENT(14)
                      KDATA(I,KPRM) = LOWEST
                      IF (NBINC.EQ.32) THEN
                          IF (LOWEST.EQ.MSK(32)) THEN
                              KDATA(I,KPRM) = 999999
                          END IF
                      ELSE IF(LOWEST.GE.MSK(NBINC)) THEN
                          KDATA(I,KPRM) = 999999
                      END IF
   51             CONTINUE
              END IF
          END IF
C                                       SET PARAMETER
C                                       ISOLATE COMBINED BIT WIDTH
          JWIDE     = IWIDE1(J) + IPTR(26)
C
          IF (JWIDE.GT.32) THEN
C                                       TOO MANY BITS IN COMBINED
C                                       BIT WIDTH
              PRINT *,'ERR 22 - HAVE EXCEEDED COMBINED BIT WIDTH'
              IPTR(1)  = 22
              RETURN
          END IF
C                                       SINGLE VALUE FOR LOWEST
          NRVALS    = 1
C                                       LOWEST
C         PRINT *,'PARAM',KPRM
          CALL GBYTE (MSGA,LOWEST,IPTR(25),JWIDE)
C         PRINT *,'            LOWEST=',LOWEST,' AT BIT LOC ',IPTR(25)
          IPTR(25)  = IPTR(25) + JWIDE
C                                       ISOLATE COMPRESSED BIT WIDTH
          CALL GBYTE (MSGA,NBINC,IPTR(25),6)
C         PRINT *,'            NBINC=',NBINC,' AT BIT LOC',IPTR(25)
          IF (NBINC.GT.32) THEN
C                                NBINC TOO LARGE
              IPTR(1)  = 22
              RETURN
          END IF
          IF (IPTR(32).EQ.2.AND.IPTR(33).EQ.5) THEN
          ELSE
              IF (NBINC.GT.JWIDE) THEN
C                 PRINT *,'FOR DESCRIPTOR',JDESC
C              PRINT *,J,'NBINC=',NBINC,' LOWEST=',LOWEST,' IWIDE1(J)=',
C    *          IWIDE1(J),' IPTR(26)=',IPTR(26),' AT BIT LOC',IPTR(25)
C                 DO 110 I = 1, KPRM
C                     WRITE (6,111)I,(KDATA(J,I),J=1,6)
C 110             CONTINUE
C 111             FORMAT (1X,5HDATA ,I3,6(2X,I10))
                  IPTR(1) = 500
                  PRINT *,'NBINC CALLS FOR LARGER BIT WIDTH THAN TABLE',
     *                       ' B  PLUS  WIDTH CHANGES'
              END IF
          END IF
          IPTR(25)  = IPTR(25) + 6
C         PRINT *,'LOWEST',LOWEST,' NBINC=',NBINC
C                                       IF TEXT EVENT, PROCESS TEXT
C                                       GET COMPRESSED VALUES
C         PRINT *,'COMPRESSED VALUES - NONTEXT'
          NRVALS    = IDENT(14)
          IPTR(31)  = IPTR(31) + 1
          KPRM      = IPTR(31) + IPTR(24)
          IF (NBINC.NE.0) THEN
              CALL GBYTES (MSGA,IVALS,IPTR(25),NBINC,0,NRVALS)
              IPTR(25)  = IPTR(25) + NBINC * NRVALS
C                                       RECALCULATE TO ORIGINAL VALUES
              DO 100 I = 1, NRVALS
C                 PRINT *,IVALS(I),MSK(NBINC),NBINC
                  IF (IVALS(I).GE.MSK(NBINC)) THEN
                      KDATA(I,KPRM) = 999999
                  ELSE
                      IF (IRFVL1(2,J).EQ.0) THEN
                         JRV  = IRFVL1(1,J)
                     ELSE
                         JRV  = IRFVL1(3,J)
                     END IF
                     KDATA(I,KPRM) = IVALS(I) + LOWEST + JRV
                  END IF
  100         CONTINUE
C             PRINT *,I,JDESC,LOWEST,IRFVL1(1,J),IRFVL1(3,J)
          ELSE
              IF (LOWEST.EQ.MSK(JWIDE)) THEN
                  DO 105 I = 1, NRVALS
                      KDATA(I,KPRM)    = 999999
  105             CONTINUE
              ELSE
                  IF (IRFVL1(2,J).EQ.0) THEN
                      JRV  = IRFVL1(1,J)
                  ELSE
                      JRV  = IRFVL1(3,J)
                  END IF
                  ICOMB  = LOWEST + JRV
                  DO 106 I = 1, NRVALS
                      KDATA(I,KPRM) = ICOMB
  106             CONTINUE
              END IF
          END IF
C         PRINT *,'KPRM=',KPRM,'  IPTR(25)=',IPTR(25)
          MSTACK(1,KPRM)  = JDESC
C         WRITE (6,80) (KDATA(I,KPRM),I=1,10)
   80     FORMAT(2X,10(F10.2,1X))
CVVVVVCHANGE#3 FIX BY KEYSER  -- 12/06/1994
C  NOTE: THIS FIX NEEDED BECAUSE THE RETURNED SCALE IN MSTACK(2,..)
C        WAS ALWAYS '0' FOR COMPRESSED DATA, INCL. CHANGED SCALES)
          MSTACK(2,KPRM) = ISCAL1(J) + IPTR(27)
CAAAAACHANGE#3 FIX BY KEYSER  -- 12/06/1994
      ELSE IF (TEXT) THEN
C         PRINT *,' FOUND TEXT MODE IN COMPRESSED DATA',IPTR(40)
C                                   GET LOWEST
C         PRINT *,' PICKED UP LOWEST',(KLOW(K),K=1,IPTR(40))
          DO 1906 K = 1, IPTR(40)
              CALL GBYTE (MSGA,KLOW,IPTR(25),8)
              IPTR(25)   = IPTR(25) + 8
              IF (KLOW(K).NE.0) THEN
                  IPTR(1)   = 27
                  PRINT *,'NON-ZERO LOWEST ON TEXT DATA'
                  RETURN
              END IF
 1906     CONTINUE
C         PRINT *,'TEXT - LOWEST = 0'
C                                   GET NBINC
          CALL GBYTE (MSGA,NBINC,IPTR(25),6)
          IPTR(25)     = IPTR(25) + 6
          IF (NBINC.NE.IPTR(40)) THEN
              IPTR(1)  = 28
              PRINT *,'NBINC IS NOT THE NUMBER OF CHARACTERS',NBINC
              RETURN
          END IF
C         PRINT *,'TEXT    NBINC =',NBINC
C                                   FOR NUMBER OF OBSERVATIONS
          IPTR(31)      = IPTR(31) + 1
          KPRM          = IPTR(31) + IPTR(24)
          ISTART        = KPRM
          I24           = IPTR(24)
          DO 1900 N   = 1, IDENT(14)
              KPRM        = ISTART
              IPTR(24)    = I24
              NBITS       = IPTR(40) * 8
 1700         CONTINUE
C             PRINT *,N,IDENT(14),'KPRM-B=',KPRM,IPTR(24),NBITS
              IF (NBITS.GT.MWDBIT) THEN
                  CALL GBYTE (MSGA,IDATA,IPTR(25),MWDBIT)
                  IPTR(25)     = IPTR(25) + MWDBIT
                  NBITS        = NBITS - MWDBIT
                  IF (IPTR(37).EQ.0) THEN
C                               CONVERTS ASCII TO EBCIDIC
                      CALL W3AI39 (IDATA,LW)
                  END IF
                  MSTACK(1,KPRM) = JDESC
                  MSTACK(2,KPRM) = 0
                  KDATA(N,KPRM) = IDATA
C                 PRINT *,'TEXT        ',N,KPRM,KDATA(N,KPRM)
C                         SET FOR NEXT PART
                  KPRM          = KPRM + 1
                  IPTR(24)      = IPTR(24) + 1
C                 PRINT 1701,1,KDATA(N,KPRM),N,KPRM,NBITS,IDATA
C1701             FORMAT (1X,I1,1X,6HKDATA=,A4,2X,I5,2X,I5,2X,I5,2X,I12)
                  GO TO 1700
              ELSE IF (NBITS.GT.0) THEN
                  CALL GBYTE (MSGA,IDATA,IPTR(25),NBITS)
                  IPTR(25)     = IPTR(25) + NBITS
                  IBUF         = (IPTR(44) - NBITS) / 8
                  IF (IBUF.GT.0) THEN
                      DO 1750 MP = 1, IBUF
                          IDATA  = IDATA * 256 + 32
 1750                 CONTINUE
                  END IF
C                               CONVERTS ASCII TO EBCIDIC
                  IF (IPTR(37).EQ.0) THEN
                      CALL W3AI39 (IDATA,LW)
                  END IF
                  MSTACK(1,KPRM) = JDESC
                  MSTACK(2,KPRM) = 0
                  KDATA(N,KPRM) = IDATA
C                 PRINT *,'TEXT        ',N,KPRM,KDATA(N,KPRM)
C                 PRINT 1701,2,KDATA(N,KPRM),N,KPRM,NBITS
                  NBITS          = 0
              END IF
C             WRITE (6,1800)N,(KDATA(N,I),I=KPRS,KPRM)
C1800         FORMAT (2X,I4,2X,3A4)
 1900     CONTINUE
      END IF
      RETURN
      END
C> @brief Process serial data
C> @author Bill Cavanaugh @date 1988-09-01

C> Process data that is not compressed
C>
C> Program history log:
C> - Bill cavanaugh 1988-09-01
C> - Bill cavanaugh 1991-01-18 Modified to properly handle non-compressed
C> data.
C> - Bill cavanaugh 1991-04-04 Text handling portion of this routine
C> modified to handle field width in bytes.
C> - Bill cavanaugh 1991-04-17 ests showed that the same data in compressed
C> and uncompressed form gave different results.
C> this has been corrected.
C>
C> @param[in] IPTR See w3fi88() routine docblock
C> @param[in] MSGA Array containing bufr message
C> @param[inout] IVALS Array of single parameter values
C> @param[inout] J
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[out] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C> Arrays containing data from table b
C> @param[out] ISCAL1 Scale for value of descriptor
C> @param[out] IRFVL1 Reference value for descriptor
C> @param[out] IWIDE1 Bit width for value of descriptorE
C> @param MSTACK
C> @param LL
C> @param JDESC
C>
C> Error return:
C> IPTR(1) = 13 - Bit width on ascii chars not a multiple of 8
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8804(IPTR,MSGA,KDATA,IVALS,MSTACK,
     *                  IWIDE1,IRFVL1,ISCAL1,J,LL,JDESC,MAXR,MAXD)

C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
C     INTEGER        KFXY1(*)
      INTEGER        ISCAL1(*)
      INTEGER        IRFVL1(3,*)
      INTEGER        IWIDE1(*)
C     CHARACTER*40   ANAME1(*)
C     CHARACTER*24   AUNIT1(*)
C  ..................................................
C
      INTEGER        MSGA(*),MAXD,MAXR
      INTEGER        IPTR(*)
      INTEGER        JDESC
      INTEGER        IVALS(*)
C     INTEGER        LSTBLK(3)
      INTEGER        KDATA(MAXR,MAXD),MSTACK(2,MAXD)
      INTEGER        J,LL
C     LOGICAL        LKEY
C
C
      INTEGER        ITEST(32)
C
       SAVE
C
      DATA  ITEST  /1,3,7,15,31,63,127,255,
     *             511,1023,2047,4095,8191,16383,
     *             32767, 65535,131071,262143,524287,
     *             1048575,2097151,4194303,8388607,
     *             16777215,33554431,67108863,134217727,
     *             268435455,536870911,1073741823,
     *             2147483647,-1/
C
      MWDBIT  = IPTR(44)
      IF (IPTR(45).NE.4) THEN
          I         = 2147483647
          ITEST(32) = I + I + 1
      END IF
C
C     PRINT *,' FI8804 NOCMP',J,JDESC,IWIDE1(J),IPTR(26),IPTR(25)
C                  --------  NOCMP  --------
C                                       IF NOT TEXT EVENT, PROCESS
      IF (IPTR(18).EQ.0) THEN
C          PRINT *,'                              NOT TEXT'
          IF ((IPTR(26)+IWIDE1(J)).LT.1) THEN
C            PRINT *,' FI8804 NOCMP',J,JDESC,IWIDE1(J),IPTR(26),IPTR(25)
              IPTR(1)  = 501
              RETURN
          END IF
C                                ISOLATE BIT WIDTH
          JWIDE    = IWIDE1(J) + IPTR(26)
C                                    IF ASSOCIATED FIELD SW ON
          IF (IPTR(29).GT.0) THEN
              IF (JDESC.NE.7957.AND.JDESC.NE.7937) THEN
                  IPTR(31)        = IPTR(31) + 1
                  KPRM            = IPTR(31) + IPTR(24)
                  MSTACK(1,KPRM)  = 33792 + IPTR(29)
                  MSTACK(2,KPRM)  = 0
                  CALL GBYTE (MSGA,IVALS,IPTR(25),IPTR(29))
                  IPTR(25)        = IPTR(25) + IPTR(29)
                  KDATA(IPTR(17),KPRM) = IVALS(1)
C                 PRINT *,'FI8804-A',KPRM,MSTACK(1,KPRM),
C    *                      MSTACK(2,KPRM),IPTR(17),KDATA(IPTR(17),KPRM)
              END IF
          END IF
          IPTR(31) = IPTR(31) + 1
          KPRM     = IPTR(31) + IPTR(24)
          MSTACK(1,KPRM) = JDESC
C         IF (IPTR(27).NE.0) THEN
C             MSTACK(2,KPRM) = IPTR(27)
C         ELSE
              MSTACK(2,KPRM) = ISCAL1(J) + IPTR(27)
C         END IF
C                                       GET VALUES
C                                CALL TO GET DATA OF GIVEN BIT WIDTH
          CALL GBYTE (MSGA,IVALS,IPTR(25),JWIDE)
C         PRINT *,'DATA  TO',IPTR(17),KPRM,IVALS(1),JWIDE,IPTR(25)
          IPTR(25) = IPTR(25) + JWIDE
C                                RETURN WITH SINGLE VALUE
          IF (IRFVL1(2,J).EQ.0) THEN
              JRV  = IRFVL1(1,J)
          ELSE
              JRV  = IRFVL1(3,J)
          END IF
          IF (JWIDE.EQ.32) THEN
              IF (IVALS(1).EQ.ITEST(JWIDE)) THEN
                  KDATA(IPTR(17),KPRM) = 999999
              ELSE
                  KDATA(IPTR(17),KPRM) = IVALS(1) + JRV
              END IF
          ELSE IF (IVALS(1).GE.ITEST(JWIDE)) THEN
              KDATA(IPTR(17),KPRM) = 999999
          ELSE
              KDATA(IPTR(17),KPRM) = IVALS(1) + JRV
          END IF
C         PRINT *,'FI8804-B',KPRM,MSTACK(1,KPRM),
C    *                     MSTACK(2,KPRM),IPTR(17),KDATA(IPTR(17),KPRM)
C         IF(JDESC.EQ.2049) THEN
C             PRINT *,'VERT SIG =',KDATA(IPTR(17),KPRM)
C         END IF
C         PRINT *,'FI8804  ',KPRM,MSTACK(1,KPRM),
C    *                       MSTACK(2,KPRM),KDATA(IPTR(17),KPRM)
      ELSE
C         PRINT *,'                             TEXT'
C         PRINT *,' FOUND TEXT MODE ****** NOT COMPRESSED *********'
          JWIDE      = IPTR(40) * 8
C         PRINT *,'                              WIDTH =',JWIDE,IPTR(40)
          NRCHRS     = IPTR(40)
          NRBITS     = JWIDE
C         PRINT *,'          CHARS =',NRCHRS,' BITS =',NRBITS
          IPTR(31)        = IPTR(31) + 1
          KANY   = 0
 1800     CONTINUE
          KANY  = KANY + 1
C         PRINT *,'          NR BITS THIS PASS',NRBITS
          IF (NRBITS.GT.MWDBIT) THEN
              CALL GBYTE (MSGA,IDATA,IPTR(25),MWDBIT)
C             PRINT 1801,KANY,IDATA,IPTR(17),KPRM,NRBITS
 1801         FORMAT (1X,I2,4X,Z8,2(4X,I4))
C                               CONVERTS ASCII TO EBCIDIC
C                               COMMENT OUT IF NOT IBM370 COMPUTER
              IF (IPTR(37).EQ.0) THEN
                  CALL W3AI39 (IDATA,IPTR(45))
              END IF
              KPRM            = IPTR(31) + IPTR(24)
              KDATA(IPTR(17),KPRM)  = IDATA
              MSTACK(1,KPRM)  = JDESC
              MSTACK(2,KPRM)  = 0
C             PRINT *,'BODY ',KPRM,MSTACK(1,KPRM),MSTACK(2,KPRM),
C    *                   KDATA(IPTR(17),KPRM)
              IPTR(25)    = IPTR(25) + MWDBIT
              NRBITS      = NRBITS - MWDBIT
              IPTR(24)    = IPTR(24) + 1
              GO TO 1800
          ELSE IF (NRBITS.GT.0) THEN
              CALL GBYTE (MSGA,IDATA,IPTR(25),NRBITS)
              IPTR(25)    = IPTR(25) + NRBITS
C                               CONVERTS ASCII TO EBCIDIC
C                               COMMENT OUT IF NOT IBM370 COMPUTER
              IF (IPTR(37).EQ.0) THEN
                  CALL W3AI39 (IDATA,IPTR(45))
              END IF
              KPRM            = IPTR(31) + IPTR(24)
              KSHFT           = MWDBIT - NRBITS
              IF (KSHFT.GT.0) THEN
                  KTRY  = KSHFT / 8
                  DO  1722  LAK = 1, KTRY
                      IF (IPTR(37).EQ.0) THEN
                          IDATA  = IDATA * 256 + 64
                      ELSE
                          IDATA  = IDATA * 256 + 32
                      END IF
C                     PRINT 1723,IDATA
C1723                 FORMAT (12X,Z8)
 1722             CONTINUE
              END IF
              KDATA(IPTR(17),KPRM) = IDATA
C             PRINT 1801,KANY,IDATA,KDATA(IPTR(17),KPRM),KPRM
              MSTACK(1,KPRM)  = JDESC
              MSTACK(2,KPRM)  = 0
C             PRINT *,'TAIL ',KPRM,MSTACK(1,KPRM),
C    *                   KDATA(IPTR(17),KPRM)
          END IF
      END IF
      RETURN
      END
C> @brief Process a replication descriptor
C> @author  Bill Cavanaugh @date 1988-09-01

C> Process a replication descriptor, must extract number
C> of replications of n descriptors from the data stream.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-09-01
C>
C> @param[in] IWORK Working descriptor list
C> @param[in] IPTR See w3fi88 routine docblock
C> @param[in] IDENT See w3fi88 routine docblock
C> @param[inout] LX X portion of current descriptor
C> @param[inout] LY Y portion of current descriptor
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[out] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C> @param MSGA
C> @param LL
C> @param KNR
C> @param MSTACK
C>
C> Error return:
C> - IPTR(1)
C>  - = 12  Data descriptor qualifier does not follow delayed replication descriptor
C>  - = 20  Exceeded count for delayed replication pass
C>
C> @author  Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8805(IPTR,IDENT,MSGA,IWORK,LX,LY,
     *                          KDATA,LL,KNR,MSTACK,MAXR,MAXD)

C
      INTEGER        IPTR(*)
      INTEGER        KNR(MAXR)
      INTEGER        ITEMP(2000)
      INTEGER        LL
      INTEGER        KTEMP(2000)
      INTEGER        KDATA(MAXR,MAXD)
      INTEGER        LX,MSTACK(2,MAXD)
      INTEGER        LY
      INTEGER        MSGA(*)
      INTEGER        KVALS(1300)
CVVVVVCHANGE#2 FIX BY KEYSER  -- 12/06/1994
C  NOTE: THIS FIX JUST CLEANS UP CODE SINCE IWORK ARRAY IS EARLIER
C        DEFINED AS 15000 WORDS
      INTEGER        IWORK(*)
CDAK  INTEGER        IWORK(MAXD)
CAAAAACHANGE#2 FIX BY KEYSER  -- 12/06/1994
      INTEGER        IDENT(*)
C
      SAVE
C
C     PRINT *,' REPLICATION FI8805'
C     DO 7100 I = 1, IPTR(13)
C         PRINT *,I,IWORK(I)
C7100 CONTINUE
C                         NUMBER OF DESCRIPTORS
      NRSET   = LX
C                         NUMBER OF REPLICATIONS
      NRREPS  = LY
      ICURR   = IPTR(11) - 1
      IPICK   = IPTR(11) - 1
C
      IF (NRREPS.EQ.0) THEN
          IPTR(39)  = 1
C                         SAVE PRIMARY DELAYED REPLICATION DESCRIPTOR
C         IPTR(31)  = IPTR(31) + 1
C         KPRM      = IPTR(31) + IPTR(24)
C         MSTACK(1,KPRM)  = JDESC
C         MSTACK(2,KPRM)  = 0
C         KDATA(IPTR(17),KPRM) = 0
C         PRINT *,'FI8805-1',KPRM,MSTACK(1,KPRM),
C    *                       MSTACK(2,KPRM),KDATA(IPTR(17),KPRM)
C                          DELAYED REPLICATION - MUST GET NUMBER OF
C                              REPLICATIONS FROM DATA.
C                                GET NEXT DESCRIPTOR
          CALL FI8808(IPTR,IWORK,LF,LX,LY,JDESC)
C          PRINT *,' DELAYED REPLICATION',LF,LX,LY,JDESC
C                                MUST BE DATA DESCRIPTION
C                                   OPERATION QUALIFIER
          IF (JDESC.EQ.7937.OR.JDESC.EQ.7947) THEN
              JWIDE     = 8
          ELSE IF (JDESC.EQ.7938.OR.JDESC.EQ.7948) THEN
              JWIDE     = 16
          ELSE IF (JDESC.EQ.7936) THEN
              JWIDE  = 1
          ELSE
              IPTR(1)   = 12
              RETURN
          END IF
C                        THIS IF BLOCK IS SET TO HANDLE
C                        DATA/DESCRIPTOR REPLICATION
          IF (JDESC.EQ.7947.OR.JDESC.EQ.7948) THEN
C                         SET DATA/DESCRIPTOR REPLICATION FLAG = ON
              IPTR(38)  = 1
C                         SAVE AS NEXT ENTRY IN KDATA, MSTACK
              IPTR(31)  = IPTR(31) + 1
              KPRM      = IPTR(31) + IPTR(24)
              MSTACK(1,KPRM)  = JDESC
              MSTACK(2,KPRM)  = 0
              CALL GBYTE (MSGA,KVALS,IPTR(25),JWIDE)
              IPTR(25)   = IPTR(25) + JWIDE
              KDATA(IPTR(17),KPRM) = KVALS(1)
              RETURN
          END IF

C                             SET SINGLE VALUE FOR SEQUENTIAL,
C                                  MULTIPLE VALUES FOR COMPRESSED
          IF (IDENT(16).EQ.0) THEN

C                               NON COMPRESSED
              CALL GBYTE (MSGA,KVALS,IPTR(25),JWIDE)
C             PRINT *,LF,LX,LY,JDESC,' NR OF REPLICATIONS',KVALS(1)
              IPTR(25)   = IPTR(25) + JWIDE
              IPTR(31)  = IPTR(31) + 1
              KPRM       = IPTR(31) + IPTR(24)
              MSTACK(1,KPRM)  = JDESC
              MSTACK(2,KPRM)  = 0
              KDATA(IPTR(17),KPRM) = KVALS(1)
              NRREPS          = KVALS(1)
C             PRINT *,'FI8805-2',KPRM,MSTACK(1,KPRM),
C    *                       MSTACK(2,KPRM),KDATA(IPTR(17),KPRM)
          ELSE
              NRVALS     = IDENT(14)
              CALL GBYTES (MSGA,KVALS,IPTR(25),JWIDE,0,NRVALS)
              IPTR(25)   = IPTR(25) + JWIDE * NRVALS
              IPTR(31)  = IPTR(31) + 1
              KPRM       = IPTR(31) + IPTR(24)
              MSTACK(1,KPRM)  = JDESC
              MSTACK(2,KPRM)  = 0
              KDATA(IPTR(17),KPRM) = KVALS(1)
              DO 100 I = 1, NRVALS
                  KDATA(I,KPRM) = KVALS(I)
  100         CONTINUE
              NRREPS     = KVALS(1)
          END IF
      ELSE
C         PRINT *,'NOT DELAYED REPLICATION'
      END IF
C                             RESTRUCTURE WORKING STACK W/REPLICATIONS
      IF (NRREPS.EQ.0) THEN
C         PRINT *,'RESTRUCTURING - NO REPLICATION'
          IPTR(11)  = IPICK + NRSET + 2
          GO TO 9999
      END IF
C     PRINT *,' SAVE OFF',NRSET,' DESCRIPTORS'
C                                 PICK UP DESCRIPTORS TO BE REPLICATED
      DO 1000 I = 1, NRSET
          CALL FI8808(IPTR,IWORK,LF,LX,LY,JDESC)
          ITEMP(I)    = JDESC
C         PRINT *,'REPLICATION        ',I,ITEMP(I)
 1000 CONTINUE
C                              MOVE TRAILING DESCRIPTORS TO HOLD AREA
      LAX       = IPTR(12) - IPTR(11) + 1
C     PRINT *,LAX,' TRAILING DESCRIPTORS TO HOLD AREA',IPTR(11),IPTR(12)
      DO 2000 I = 1, LAX
          CALL FI8808(IPTR,IWORK,LF,LX,LY,JDESC)
          KTEMP(I)   = JDESC
C         PRINT *,'                             ',I,KTEMP(I)
 2000 CONTINUE
C                              REPLICATIONS INTO ISTACK
C     PRINT *,' MUST REPLICATE ',KX,' DESCRIPTORS',KY,' TIMES'
C     PRINT *,'REPLICATIONS INTO STACK. LOC',ICURR
      DO 4000 I = 1, NRREPS
          DO 3000 J = 1, NRSET
              IWORK(ICURR) = ITEMP(J)
C             PRINT *,'FI8805 A',ICURR,IWORK(ICURR)
              ICURR         = ICURR + 1
 3000     CONTINUE
 4000 CONTINUE
C     PRINT *,'                     TO  LOC',ICURR-1
C                              RESTORE TRAILING DESCRIPTORS
C     PRINT *,'TRAILING DESCRIPTORS INTO STACK. LOC',ICURR
      DO 5000 I = 1, LAX
          IWORK(ICURR) = KTEMP(I)
C         PRINT *,'FI8805 B',ICURR,IWORK(ICURR)
          ICURR        = ICURR + 1
 5000 CONTINUE
      IPTR(12)  = ICURR - 1
      IPTR(11)  = IPICK
 9999 CONTINUE
C     DO 5500 I = 1, IPTR(12)
C         PRINT *,'FI8805 B',I,IWORK(I),IPTR(11)
C5500 CONTINUE
      RETURN
      END
C> @brief Process operator descriptors
C> @author Bill Cavanaugh @date 1988-09-01

C> Extract and save indicated change values for use
C> until changes are rescinded, or extract text strings indicated
C> through 2 05 yyy.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-09-01
C> - Bill Cavanaugh 1991-04-04 Modified to handle descriptor 2 05 yyy
C> - Bill Cavanaugh 1991-05-10 Coding has been added to process properly
C> table c descriptor 2 06 yyy.
C> - Bill Cavanaugh 1991-11-21 Coding has been added to properly process
C> table c descriptor 2 03 yyy, the change
C> to new reference value for selected
C> descriptors.
C>
C> @param[in] IPTR See w3fi88 routine docblock
C> @param[in] LX X portion of current descriptor
C> @param[in] LY Y portion of current descriptor
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[out] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C> Arrays containing data from table b
C> @param[out] ISCAL1 Scale for value of descriptor
C> @param[out] IRFVL1 Reference value for descriptor
C> @param[out] IWIDE1 Bit width for value of descriptor
C> @param IDENT
C> @param MSGA
C> @param IVALS
C> @param MSTACK
C> @param J
C> @param LL
C> @param KFXY1
C> @param IWORK
C> @param JDESC
C> @param KPTRB
C>
C> Error return:
C> IPTR(1) = 5 - Erroneous x value in data descriptor operator
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8806 (IPTR,LX,LY,IDENT,MSGA,KDATA,IVALS,MSTACK,
     *     IWIDE1,IRFVL1,ISCAL1,J,LL,KFXY1,IWORK,JDESC,MAXR,MAXD,KPTRB)

C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
      INTEGER        KFXY1(*),ISCAL1(*),IRFVL1(3,*),IWIDE1(*)
C     CHARACTER*40   ANAME1(*)
C     CHARACTER*24   AUNIT1(*)
C  ..................................................
      INTEGER        IPTR(*),KDATA(MAXR,MAXD),IVALS(*)
      INTEGER        IDENT(*),IWORK(*),KPTRB(*)
      INTEGER        MSGA(*),MSTACK(2,MAXD)
      INTEGER        J,JDESC
      INTEGER        LL
      INTEGER        LX
      INTEGER        LY
C
      SAVE
C
C     PRINT *,' F2 - DATA DESCRIPTOR OPERATOR'
      IF (LX.EQ.1) THEN
C                                CHANGE BIT WIDTH
          IF (LY.EQ.0) THEN
C             PRINT *,' RETURN TO NORMAL WIDTH'
              IPTR(26) = 0
          ELSE
C             PRINT *,' EXPAND WIDTH BY',LY-128,' BITS'
              IPTR(26)  = LY - 128
          END IF
      ELSE IF (LX.EQ.2) THEN
C                                  CHANGE SCALE
          IF (LY.EQ.0) THEN
C                              RESET TO STANDARD SCALE
              IPTR(27) = 0
          ELSE
C                               SET NEW SCALE
              IPTR(27)  = LY - 128
          END IF
      ELSE IF (LX.EQ.3) THEN
C                      CHANGE REFERENCE VALUE
C                                 FOR EACH OF THOSE DESCRIPTORS BETWEEN
C                                 2 03 YYY WHERE Y LT 255 AND
C                                 2 03 255, EXTRACT THE NEW REFERENCE
C                                 VALUE (BIT WIDTH YYY) AND PLACE
C                                 IN TERTIARY TABLE B REF VAL POSITION,
C                                 SET FLAG IN SECONDARY REFVAL POSITION
C                                 THOSE DESCRIPTORS DO NOT HAVE DATA
C                                 ASSOCIATED WITH THEM, BUT ONLY
C                                 IDENTIFY THE TABLE B ENTRIES THAT
C                                 ARE GETTING NEW REFERENCE VALUES.
          KYYY     = LY
          IF (KYYY.GT.0.AND.KYYY.LT.255) THEN
C                               START CYCLING THRU DESCRIPTORS UNTIL
C                               TERMINATE NEW REF VALS IS FOUND
  300         CONTINUE
              CALL FI8808 (IPTR,IWORK,LF,LX,LY,JDESC)
              IF (JDESC.EQ.33791) THEN
C                    IF 2 03 255 THEN RETURN
                  RETURN
              END IF
C                               FIND MATCHING TABLE B ENTRY
              LJ  = KPTRB(JDESC)
              IF (LJ.LT.1) THEN
C                         MATCHING DESCRIPTOR NOT FOUND, ERROR ERROR
                  PRINT *,'2 03 YYY - MATCHING DESCRIPTOR NOT FOUND'
                  IPTR(1)  = 23
                  RETURN
              END IF
C                                  TURN ON SWITCH
              IRFVL1(2,LJ) = 1
C                                 INSERT NEW REFERENCE VALUE
              CALL GBYTE (MSGA,IRFVL1(3,LJ),IPTR(25),KYYY)
              GO TO 300
          ELSE IF (KYYY.EQ.0) THEN
C                                      MUST TURN OFF ALL NEW
C                                      REFERENCE VALUES
              DO 400 I = 1, IPTR(21)
                  IRFVL1(2,I) = 0
  400         CONTINUE
          END IF
C                                      LX = 3
C                                      MUST BE CONCLUDED WITH Y=255
      ELSE IF (LX.EQ.4) THEN
C                             ASSOCIATED VALUES
          IF (LY.EQ.0) THEN
              IPTR(29) = 0
C             PRINT *,'RESET ASSOCIATED VALUES',IPTR(29)
          ELSE
              IPTR(29)  = LY
              IF (IWORK(IPTR(11)).NE.7957) THEN
                  PRINT *,'2 04 YYY NOT FOLLOWED BY 0 31 021'
                  IPTR(1)   = 11
              END IF
C             PRINT *,'SET ASSOCIATED VALUES',IPTR(29)
          END IF
      ELSE IF (LX.EQ.5) THEN
          MWDBIT = IPTR(44)
C                        PROCESS TEXT DATA
          IPTR(40)  = LY
          IPTR(18)  = 1
          J = KPTRB(JDESC)
          IF (IDENT(16).EQ.0) THEN
C             PRINT *,'FROM FI8806 - 2 05 YYY - NONCOMPRESSED TEXT',J
              CALL FI8804(IPTR,MSGA,KDATA,IVALS,MSTACK,
     *                IWIDE1,IRFVL1,ISCAL1,J,LL,JDESC,MAXR,MAXD)
          ELSE
C             PRINT *,'2 05 YYY - TEXT - COMPRESSED MODE YYY=',LY
C             PRINT *,'TEXT - LOWEST = 0'
              IPTR(25) = IPTR(25) + IPTR(40) * 8
C                                   GET NBINC
C             CALL GBYTE (MSGA,NBINC,IPTR(25),6)
              IPTR(25)     = IPTR(25) + 6
              NBINC  = IPTR(40)
C             PRINT *,'TEXT    NBINC =',NBINC,IPTR(40)
C                                   FOR NUMBER OF OBSERVATIONS
              IPTR(31)      = IPTR(31) + 1
              KPRM          = IPTR(31) + IPTR(24)
              ISTART        = KPRM
              DO 1900 N   = 1, IDENT(14)
                  KPRM        = ISTART
                  NBITS       = IPTR(40) * 8
 1700             CONTINUE
C                 PRINT *,'1700',KDATA(N,KPRM),N,KPRM,NBITS
                  IF (NBITS.GT.MWDBIT) THEN
                      CALL GBYTE (MSGA,IDATA,IPTR(25),MWDBIT)
                      IPTR(25)     = IPTR(25) + MWDBIT
                      NBITS        = NBITS - MWDBIT
C                               CONVERTS ASCII TO EBCIDIC
C                               COMMENT OUT IF NOT IBM370 COMPUTER
                      IF (IPTR(37).EQ.0) THEN
                          CALL W3AI39 (IDATA,IPTR(45))
                      END IF
                      MSTACK(1,KPRM) = JDESC
                      MSTACK(2,KPRM) = 0
                      KDATA(N,KPRM) = IDATA
C                     PRINT *,'TEXT        ',N,KPRM,KDATA(N,KPRM)
C                          SET FOR NEXT PART
                      KPRM          = KPRM + 1
C                     PRINT 1701,1,KDATA(N,KPRM),N,KPRM,NBITS,IDATA
C1701                 FORMAT (1X,I1,1X,6HKDATA=,A4,2X,I5,2X,I5,2X,I5,2X,
C    *                 I10)
                      GO TO 1700
                  ELSE IF (NBITS.EQ.MWDBIT) THEN
                      CALL GBYTE (MSGA,IDATA,IPTR(25),MWDBIT)
                      IPTR(25)     = IPTR(25) + MWDBIT
                      NBITS        = NBITS - MWDBIT
C                               CONVERTS ASCII TO EBCIDIC
C                               COMMENT OUT IF NOT IBM370 COMPUTER
                      IF (IPTR(37).EQ.0) THEN
                          CALL W3AI39 (IDATA,IPTR(45))
                      END IF
                      MSTACK(1,KPRM) = JDESC
                      MSTACK(2,KPRM) = 0
                      KDATA(N,KPRM) = IDATA
C                     PRINT *,'TEXT        ',N,KPRM,KDATA(N,KPRM)
C                          SET FOR NEXT PART
                      KPRM          = KPRM + 1
C                     PRINT 1701,1,KDATA(N,KPRM),N,KPRM,NBITS,IDATA
                  ELSE IF (NBITS.GT.0) THEN
                      CALL GBYTE (MSGA,IDATA,IPTR(25),NBITS)
                      IPTR(25)     = IPTR(25) + NBITS
                      IBUF         = (MWDBIT - NBITS) / 8
                      IF (IBUF.GT.0) THEN
                          DO 1750 MP = 1, IBUF
                              IDATA  = IDATA * 256 + 32
 1750                     CONTINUE
                      END IF
C                               CONVERTS ASCII TO EBCIDIC
C                               COMMENT OUT IF NOT IBM370 COMPUTER
                      IF (IPTR(37).EQ.0) THEN
                          CALL W3AI39 (IDATA,IPTR(45))
                      END IF
                      MSTACK(1,KPRM) = JDESC
                      MSTACK(2,KPRM) = 0
                      KDATA(N,KPRM) = IDATA
C                     PRINT *,'TEXT        ',N,KPRM,KDATA(N,KPRM)
C                     PRINT 1701,2,KDATA(N,KPRM),N,KPRM,NBITS
                  END IF
C                 WRITE (6,1800)N,(KDATA(N,I),I=KPRS,KPRM)
C1800             FORMAT (2X,I4,2X,3A4)
 1900         CONTINUE

              IPTR(24)  = IPTR(24) + IPTR(40) / 4 - 1
              IF (MOD(IPTR(40),4).NE.0) IPTR(24) = IPTR(24) + 1
          END IF
          IPTR(18)  = 0
C  ---------------------------
      ELSE IF (LX.EQ.6) THEN
C                          SKIP NEXT DESCRIPTOR
C              SET TO PASS OVER DESCRIPTOR AND DATA
C                     IF DESCRIPTOR NOT IN TABLE B
          IPTR(36)   = LY
C         PRINT *,'SET TO SKIP',LY,' BIT FIELD'
          IPTR(31)       = IPTR(31) + 1
          KPRM           = IPTR(31) + IPTR(24)
          MSTACK(1,KPRM) = 34304 + LY
          MSTACK(2,KPRM) = 0
      ELSE
          IPTR(1)   = 5
      ENDIF
      RETURN
      END
C> @brief Process queue descriptor.
C> @author Bill Cavanaugh @date 1988-09-01

C> Substitute descriptor queue for queue descriptor.
C>
C> Program history log:
C> - Bill Cavanaugh 1988-09-01
C> - Bill Cavanaugh 1991-04-17 Improved handling of nested queue descriptors
C> - Bill Cavanaugh 1991-05-28 Improved handling of nested queue descriptors
C> based on tests with live data.
C>
C> @param[in] IWORK Working descriptor list
C> @param[in] IPTR See w3fi88 routine docblock
C> @param[in] ITBLD+ITBLD2 Array containing descriptor queues
C> @param[in] JDESC Queue descriptor to be expanded
C> @param KPTRD
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8807(IPTR,IWORK,ITBLD,ITBLD2,JDESC,KPTRD)

C  ..................................................
C
C                       ANCILLARY TABLE D FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        ITBLD2(20,*)
C  ..................................................
C
C                       NEW BASE TABLE D
C
      INTEGER        ITBLD(20,*)
C  ..................................................
C
      INTEGER        IPTR(*),JDESC,KPTRD(*)
      INTEGER        IWORK(*),IHOLD(15000)
C
      SAVE
C     PRINT *,' FI8807  F3 ENTRY',IPTR(11),IPTR(12)
C                                   SET FOR BINARY SEARCH IN TABLE D
      JLO  = 1
      JHI  = IPTR(20)
C     PRINT *,'LOOKING FOR QUEUE DESCRIPTOR',JDESC,IPTR(11),IPTR(12)
C
      JMID = KPTRD(MOD(JDESC,16384))
      IF (JMID.LT.0) THEN
          IPTR(1)  = 4
          RETURN
      END IF
C                              HAVE TABLE D MATCH
C     PRINT *,'D ',(ITBLD(LL,JMID),LL=1,20)
C     PRINT *,'TABLE D TO IHOLD'
      IK       = 0
      JK       = 0
      DO 200 KI = 2, 20
          IF (ITBLD(KI,JMID).NE.0) THEN
              IK        = IK + 1
              IHOLD(IK) = ITBLD(KI,JMID)
C             PRINT *,IK,IHOLD(IK)
          ELSE
              GO TO 300
          END IF
  200 CONTINUE
  300 CONTINUE
      KK       = IPTR(11)
      IF (KK.GT.IPTR(12)) THEN
C                          NOTHING MORE TO APPEND
C         PRINT *,'NOTHING MORE TO APPEND'
      ELSE
C                          APPEND TRAILING IWORK TO IHOLD
C         PRINT *,'APPEND FROM ',KK,' TO',IPTR(12)
          DO 500 I = KK, IPTR(12)
              IK  = IK + 1
              IHOLD(IK) = IWORK(I)
  500     CONTINUE
      END IF
C                                RESET IHOLD TO IWORK
C     PRINT *,' RESET IWORK STACK'
      KK       = IPTR(11) - 2
      DO 1000 I = 1, IK
          KK        = KK + 1
          IWORK(KK) = IHOLD(I)
 1000 CONTINUE
      IPTR(12)   = KK
C     PRINT *,' FI8807  F3 EXIT ',IPTR(11),IPTR(12)
C     DO 2000 I = 1, IPTR(12)
C         PRINT *,'EXIT  IWORK',I,IWORK(I)
C2000 CONTINUE
C                                RESET POINTERS
      IPTR(11)   = IPTR(11) - 1
      RETURN
      END
C> @brief
C> @author Bill Cavanaugh @date 1988-09-01

C>
C> Program history log:
C> - Bill Cavanaugh 1988-09-01
C>
C> @param[inout] IPTR See w3fi88 routine docblock
C> @param[in] IWORK Working descriptor list
C> @param LF
C> @param LX
C> @param LY
C> @param JDESC
C>
C> @author Bill Cavanaugh @date 1988-09-01
      SUBROUTINE FI8808(IPTR,IWORK,LF,LX,LY,JDESC)

      INTEGER       IPTR(*),IWORK(*),LF,LX,LY,JDESC
      SAVE
C
C     PRINT *,' FI8808 NEW DESCRIPTOR PICKUP'
      JDESC    = IWORK(IPTR(11))
      LY       = MOD(JDESC,256)
      IPTR(34) = LY
      LX       = MOD((JDESC/256),64)
      IPTR(33) = LX
      LF       = JDESC / 16384
      IPTR(32) = LF
C     PRINT *,' TEST DESCRIPTOR',LF,LX,LY,' AT',IPTR(11)
      IPTR(11) = IPTR(11) + 1
      RETURN
      END
C> @brief Reformat profiler w hgt increments
C> @author Bill Cavanaugh @date 1990-02-14

C> Reformat decoded profiler data to show heights instead of
C> height increments.
C>
C> Program history log:
C> - Bill Cavanaugh 1990-02-14
C>
C> @param[in] IDENT Array contains message information extracted from BUFR message
C> - IDENT(1) - Edition number (byte 4, section 1)
C> - IDENT(2) - Originating center (bytes 5-6, section 1)
C> - IDENT(3) - Update sequence (byte 7, section 1)
C> - IDENT(4) - (byte 8, section 1)
C> - IDENT(5) - Bufr message type (byte 9, section 1)
C> - IDENT(6) - Bufr msg sub-type (byte 10, section 1)
C> - IDENT(7) - (bytes 11-12, section 1)
C> - IDENT(8) - Year of century (byte 13, section 1)
C> - IDENT(9) - Month of year (byte 14, section 1)
C> - IDENT(10) - Day of month (byte 15, section 1)
C> - IDENT(11) - Hour of day (byte 16, section 1)
C> - IDENT(12) - Minute of hour (byte 17, section 1)
C> - IDENT(13) - Rsvd by adp centers (byte 18, section 1)
C> - IDENT(14) - Nr of data subsets (byte 5-6, section 3)
C> - IDENT(15) - Observed flag (byte 7, bit 1, section 3)
C> - IDENT(16) - Compression flag (byte 7, bit 2, section 3)
C> @param[in] MSTACK Working descriptor list and scaling factor
C> @param[in] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C>  maxr and parameter number limited to value of input
C>  argument maxd)
C> @param[in] IPTR See w3fi88
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C>
C> @author Bill Cavanaugh @date 1990-02-14
      SUBROUTINE FI8809(IDENT,MSTACK,KDATA,IPTR,MAXR,MAXD)

C  ----------------------------------------------------------------
C
      INTEGER        ISW
      INTEGER        IDENT(*),KDATA(MAXR,MAXD)
      INTEGER        MSTACK(2,MAXD),IPTR(*)
      INTEGER        KPROFL(1700)
      INTEGER        KPROF2(1700)
      INTEGER        KSET2(1700)
C
C  ----------------------------------------------------------
      SAVE
C     PRINT *,'FI8809'
C                                LOOP FOR NUMBER OF SUBSETS/REPORTS
      DO 3000 I = 1, IDENT(14)
C                                INIT FOR DATA INPUT ARRAY
          MK         = 1
C                                INIT FOR DESC OUTPUT ARRAY
          JK         = 0
C                                LOCATION
          ISW        = 0
          DO 200 J = 1, 3
C                                    LATITUDE
              IF (MSTACK(1,MK).EQ.1282) THEN
                  ISW   = ISW + 1
                  GO TO 100
C                                    LONGITUDE
              ELSE IF (MSTACK(1,MK).EQ.1538) THEN
                  ISW   = ISW + 2
                  GO TO 100
C                                    HEIGHT ABOVE SEA LEVEL
              ELSE IF (MSTACK(1,MK).EQ.1793) THEN
                  IHGT  = KDATA(I,MK)
                  ISW   = ISW + 4
                  GO TO 100
              END IF
              GO TO 200
  100         CONTINUE
              JK             = JK + 1
C                                     SAVE DESCRIPTOR
              KPROFL(JK)     = MSTACK(1,MK)
C                                     SAVE SCALE
              KPROF2(JK)     = MSTACK(2,MK)
C                                      SAVE DATA
              KSET2(JK)  = KDATA(I,MK)
              MK         = MK + 1
  200     CONTINUE
          IF (ISW.NE.7) THEN
              PRINT *,'LOCATION ERROR PROCESSING PROFILER'
              IPTR(1)    = 200
              RETURN
          END IF
C  TIME
          ISW        = 0
          DO 400 J = 1, 7
C                                    YEAR
              IF (MSTACK(1,MK).EQ.1025) THEN
                  ISW   = ISW + 1
                  GO TO 300
C                                    MONTH
              ELSE IF (MSTACK(1,MK).EQ.1026) THEN
                  ISW   = ISW + 2
                  GO TO 300
C                                    DAY
              ELSE IF (MSTACK(1,MK).EQ.1027) THEN
                  ISW   = ISW + 4
                  GO TO 300
C                                    HOUR
              ELSE IF (MSTACK(1,MK).EQ.1028) THEN
                  ISW   = ISW + 8
                  GO TO 300
C                                    MINUTE
              ELSE IF (MSTACK(1,MK).EQ.1029) THEN
                  ISW   = ISW + 16
                  GO TO 300
C                                    TIME SIGNIFICANCE
              ELSE IF (MSTACK(1,MK).EQ.2069) THEN
                  ISW   = ISW + 32
                  GO TO 300
              ELSE IF (MSTACK(1,MK).EQ.1049) THEN
                  ISW   = ISW + 64
                  GO TO 300
              END IF
              GO TO 400
  300         CONTINUE
              JK         = JK + 1
C                             SAVE DESCRIPTOR
              KPROFL(JK) = MSTACK(1,MK)
C                                     SAVE SCALE
              KPROF2(JK)     = MSTACK(2,MK)
C                             SAVE DATA
              KSET2(JK)  = KDATA(I,MK)
              MK         = MK + 1
  400     CONTINUE
          IF (ISW.NE.127) THEN
              PRINT *,'TIME ERROR PROCESSING PROFILER',ISW
              IPTR(1)    = 201
              RETURN
          END IF
C  SURFACE DATA
          KRG        = 0
          ISW        = 0
          DO 600 J = 1, 10
C                                    WIND SPEED
              IF (MSTACK(1,MK).EQ.2818) THEN
                  ISW   = ISW + 1
                  GO TO 500
C                                    WIND DIRECTION
              ELSE IF (MSTACK(1,MK).EQ.2817) THEN
                  ISW   = ISW + 2
                  GO TO 500
C                                    PRESS REDUCED TO MSL
              ELSE IF (MSTACK(1,MK).EQ.2611) THEN
                  ISW   = ISW + 4
                  GO TO 500
C                                    TEMPERATURE
              ELSE IF (MSTACK(1,MK).EQ.3073) THEN
                  ISW   = ISW + 8
                  GO TO 500
C                                    RAINFALL RATE
              ELSE IF (MSTACK(1,MK).EQ.3342) THEN
                  ISW   = ISW + 16
                  GO TO 500
C                                    RELATIVE HUMIDITY
              ELSE IF (MSTACK(1,MK).EQ.3331) THEN
                  ISW   = ISW + 32
                  GO TO 500
C                                    1ST RANGE GATE OFFSET
              ELSE IF (MSTACK(1,MK).EQ.1982.OR.
     *                    MSTACK(1,MK).EQ.1983) THEN
C       CANNOT USE NORMAL PROCESSING FOR FIRST RANGE GATE, MUST SAVE
C        VALUE FOR LATER USE
                  IF (MSTACK(1,MK).EQ.1983) THEN
                      IHGT   = KDATA(I,MK)
                      MK     = MK + 1
                      KRG   = 1
                  ELSE
                      IF (KRG.EQ.0) THEN
                          INCRHT             = KDATA(I,MK)
                          MK    = MK + 1
                          KRG   = 1
C                         PRINT *,'INITIAL INCR =',INCRHT
                      ELSE
                          LHGT  = 500 + IHGT - KDATA(I,MK)
                          ISW   = ISW + 64
C                         PRINT *,'BASE HEIGHT=',LHGT,' INCR=',INCRHT
                      END IF
                  END IF
C                                    MODE #1
              ELSE IF (MSTACK(1,MK).EQ.8128) THEN
                  ISW   = ISW + 128
                  GO TO 500
C                                    MODE #2
              ELSE IF (MSTACK(1,MK).EQ.8129) THEN
                  ISW   = ISW + 256
                  GO TO 500
              END IF
              GO TO 600
  500         CONTINUE
C                              SAVE DESCRIPTOR
              JK             = JK + 1
              KPROFL(JK)     = MSTACK(1,MK)
C                                     SAVE SCALE
              KPROF2(JK)     = MSTACK(2,MK)
C                              SAVE DATA
              KSET2(JK)      = KDATA(I,MK)
C             IF (I.EQ.1) THEN
C                 PRINT *,'   ',JK,KPROFL(JK),KSET2(JK)
C             END IF
              MK             = MK + 1
  600     CONTINUE
          IF (ISW.NE.511) THEN
              PRINT *,'SURFACE ERROR PROCESSING PROFILER',ISW
              IPTR(1)    = 202
              RETURN
          END IF
C  43 LEVELS
          DO 2000 L = 1, 43
 2020         CONTINUE
              ISW        = 0
C                                    HEIGHT INCREMENT
              IF (MSTACK(1,MK).EQ.1982) THEN
C                 PRINT *,'NEW HEIGHT INCREMENT',KDATA(I,MK)
                  INCRHT  = KDATA(I,MK)
                  MK      = MK + 1
                  IF (LHGT.LT.(9250+IHGT)) THEN
                      LHGT    = IHGT + 500 - INCRHT
                  ELSE
                      LHGT    = IHGT + 9250 - INCRHT
                  END IF
              END IF
C    MUST ENTER HEIGHT OF THIS LEVEL - DESCRIPTOR AND DATA
C          AT THIS POINT     - HEIGHT + INCREMENT + BASE VALUE
              LHGT       = LHGT + INCRHT
C             PRINT *,'LEVEL ',L,LHGT
              IF (L.EQ.37) THEN
                  LHGT       = LHGT + INCRHT
              END IF
              JK          = JK + 1
C                              SAVE DESCRIPTOR
              KPROFL(JK)  = 1798
C                                     SAVE SCALE
              KPROF2(JK)     = 0
C                              SAVE DATA
              KSET2(JK)  = LHGT
C             IF (I.EQ.10) THEN
C                 PRINT *,' '
C                 PRINT *,'HGT',JK,KPROFL(JK),KSET2(JK)
C             END IF
              ISW   = 0
              DO 800 J = 1, 9
  750             CONTINUE
                  IF (MSTACK(1,MK).EQ.1982) THEN
                      GO TO 2020
C                                    U VECTOR VALUE
                  ELSE IF (MSTACK(1,MK).EQ.3008) THEN
                      ISW   = ISW + 1
                      IF (KDATA(I,MK).GE.2047) THEN
                          VECTU              = 32767
                      ELSE
                          VECTU              = KDATA(I,MK)
                      END IF
                      MK    = MK + 1
                      GO TO 800
C                                    V VECTOR VALUE
                  ELSE IF (MSTACK(1,MK).EQ.3009) THEN
                      ISW   = ISW + 2
                      IF (KDATA(I,MK).GE.2047) THEN
                          VECTV              = 32767
                      ELSE
                          VECTV              = KDATA(I,MK)
                      END IF
                      MK    = MK + 1
C IF U VALUE IS ALSO AVAILABLE THEN GENERATE DDFFF
C     DESCRIPTORS AND DATA
                      IF (IAND(ISW,1).NE.0) THEN
                          IF (VECTU.EQ.32767.OR.VECTV.EQ.32767) THEN
C                                               SAVE DD DESCRIPTOR
                              JK             = JK + 1
                              KPROFL(JK)     = 2817
C                                     SAVE SCALE
                              KPROF2(JK)     = 0
C                                               SAVE DD DATA
                              KSET2(JK)      = 32767
C                                               SAVE FFF DESCRIPTOR
                              JK             = JK + 1
                              KPROFL(JK)     = 2818
C                                     SAVE SCALE
                              KPROF2(JK)     = 1
C                                               SAVE FFF DATA
                              KSET2(JK)      = 32767
                          ELSE
C                                               GENERATE DDFFF
                              CALL W3FC05 (VECTU,VECTV,DIR,SPD)
                              NDIR         = DIR
                              SPD          = SPD
                              NSPD         = SPD
C                             PRINT *,'                ',NDIR,NSPD
C                                               SAVE DD DESCRIPTOR
                              JK             = JK + 1
                              KPROFL(JK)     = 2817
C                                     SAVE SCALE
                              KPROF2(JK)     = 0
C                                               SAVE DD DATA
                              KSET2(JK)          = DIR
C                             IF (I.EQ.1) THEN
C                                 PRINT *,'DD ',JK,KPROFL(JK),KSET2(JK)
C                             END IF
C                                               SAVE FFF DESCRIPTOR
                              JK             = JK + 1
                              KPROFL(JK)     = 2818
C                                     SAVE SCALE
                              KPROF2(JK)     = 1
C                                               SAVE FFF DATA
                              KSET2(JK)          = SPD
C                             IF (I.EQ.1) THEN
C                                 PRINT *,'FFF',JK,KPROFL(JK),KSET2(JK)
C                             END IF
                          END IF
                      END IF
                      GO TO 800
C                                    W VECTOR VALUE
                  ELSE IF (MSTACK(1,MK).EQ.3010) THEN
                      ISW   = ISW + 4
                      GO TO 700
C                                    Q/C TEST RESULTS
                  ELSE IF (MSTACK(1,MK).EQ.8130) THEN
                      ISW   = ISW + 8
                      GO TO 700
C                                    U,V QUALITY IND
                ELSE IF(IAND(ISW,16).EQ.0.AND.MSTACK(1,MK).EQ.2070) THEN
                      ISW   = ISW + 16
                      GO TO 700
C                                    W QUALITY IND
                ELSE IF(IAND(ISW,32).EQ.0.AND.MSTACK(1,MK).EQ.2070) THEN
                      ISW   = ISW + 32
                      GO TO 700
C                                    SPECTRAL PEAK POWER
                  ELSE IF (MSTACK(1,MK).EQ.5568) THEN
                      ISW   = ISW + 64
                      GO TO 700
C                                    U,V VARIABILITY
                  ELSE IF (MSTACK(1,MK).EQ.3011) THEN
                      ISW   = ISW + 128
                      GO TO 700
C                                    W VARIABILITY
                  ELSE IF (MSTACK(1,MK).EQ.3013) THEN
                      ISW   = ISW + 256
                      GO TO 700
                  ELSE IF ((MSTACK(1,MK)/16384).NE.0) THEN
                      MK     = MK + 1
                      GO TO 750
                  END IF
                  GO TO 800
  700             CONTINUE
                  JK         = JK + 1
C                                      SAVE DESCRIPTOR
                  KPROFL(JK) = MSTACK(1,MK)
C                                     SAVE SCALE
                  KPROF2(JK)     = MSTACK(2,MK)
C                                      SAVE DATA
                  KSET2(JK)          = KDATA(I,MK)
                  MK         = MK + 1
C                 IF (I.EQ.1) THEN
C                     PRINT *,'   ',JK,KPROFL(JK),KSET2(JK)
C                 END IF
  800         CONTINUE
              IF (ISW.NE.511) THEN
                  PRINT *,'LEVEL ERROR PROCESSING PROFILER',ISW
                  IPTR(1)    = 203
                  RETURN
              END IF
 2000     CONTINUE
C                        MOVE DATA BACK INTO KDATA ARRAY
          DO 4000 LL = 1, JK
              KDATA(I,LL) = KSET2(LL)
 4000     CONTINUE
 3000 CONTINUE
C     PRINT *,'REBUILT ARRAY'
      DO 5000 LL = 1, JK
C                           DESCRIPTOR
          MSTACK(1,LL)  = KPROFL(LL)
C                           SCALE
          MSTACK(2,LL)  = KPROF2(LL)
C         PRINT *,LL,MSTACK(1,LL),(KDATA(I,LL),I=1,7)
 5000 CONTINUE
C                        MOVE REFORMATTED DESCRIPTORS TO MSTACK ARRAY
      IPTR(31) = JK
      RETURN
      END
C> @brief Reformat profiler edition 2 data
C> @author Bill Cavanaugh @date 1993-01-27

C> Reformat profiler data in edition 2
C>
C> Program history log:
C> - Bill Cavanaugh 1993-01-27
C> - Dennis Keyser 1995-06-07 A correction was made to prevent
C> unnecessary looping when all requested
C> descriptors are missing.
C>
C> @param[in] IDENT    - ARRAY CONTAINS MESSAGE INFORMATION EXTRACTED FROM BUFR MESSAGE -
C> - IDENT(1)  - Edition number (byte 4, section 1)
C> - IDENT(2)  - Originating center (bytes 5-6, section 1)
C> - IDENT(3)  - Update sequence (byte 7, section 1)
C> - IDENT(4)  - (byte 8, section 1)
C> - IDENT(5)  - Bufr message type (byte 9, section 1)
C> - IDENT(6)  - Bufr msg sub-type (byte 10, section 1)
C> - IDENT(7)  - (bytes 11-12, section 1)
C> - IDENT(8)  - Year of century (byte 13, section 1)
C> - IDENT(9)  - Month of year (byte 14, section 1)
C> - IDENT(10) - Day of month (byte 15, section 1)
C> - IDENT(11) - Hour of day (byte 16, section 1)
C> - IDENT(12) - Minute of hour (byte 17, section 1)
C> - IDENT(13) - Rsvd by adp centers(byte 18, section 1)
C> - IDENT(14) - Nr of data subsets (byte 5-6, section 3)
C> - IDENT(15) - Observed flag (byte 7, bit 1, section 3)
C> - IDENT(16) - Compression flag (byte 7, bit 2, section 3)
C> @param[in] MSTACK Working descriptor list and scaling factor
C> @param[in] KDATA Array containing decoded reports from bufr message.
C> KDATA(Report number,parameter number)
C> (report number limited to value of input argument
C> maxr and parameter number limited to value of input
C> argument maxd)
C> @param[in] IPTR See w3fi88
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C>
C> @author Bill Cavanaugh @date 1993-01-27
      SUBROUTINE FI8810(IDENT,MSTACK,KDATA,IPTR,MAXR,MAXD)

      INTEGER        ISW
      INTEGER        IDENT(*),KDATA(MAXR,MAXD)
      INTEGER        MSTACK(2,MAXD),IPTR(*)
      INTEGER        KPROFL(1700)
      INTEGER        KPROF2(1700)
      INTEGER        KSET2(1700)
C
      SAVE
C                             LOOP FOR NUMBER OF SUBSETS
      DO 3000 I = 1, IDENT(14)
          MK  = 1
          JK  = 0
          ISW  = 0
C         PRINT *,'IDENTIFICATION'
          DO 200 J = 1, 5
              IF (MSTACK(1,MK).EQ.257) THEN
C                            BLOCK NUMBER
                  ISW  = ISW + 1
              ELSE IF (MSTACK(1,MK).EQ.258) THEN
C                           STATION NUMBER
                  ISW  = ISW + 2
              ELSE IF (MSTACK(1,MK).EQ.1282) THEN
C                           LATITUDE
                  ISW  = ISW + 4
              ELSE IF (MSTACK(1,MK).EQ.1538) THEN
C                           LONGITUDE
                  ISW  = ISW + 8
              ELSE IF (MSTACK(1,MK).EQ.1793) THEN
C                           HEIGHT OF STATION
                  ISW  = ISW + 16
                  IHGT  = KDATA(I,MK)
              ELSE
                  MK  = MK + 1
                  GO TO 200
              END IF
              JK  = JK + 1
              KPROFL(JK)  = MSTACK(1,MK)
              KPROF2(JK)  = MSTACK(2,MK)
              KSET2(JK)   = KDATA(I,MK)
C             PRINT *,JK,KPROFL(JK),KSET2(JK)
              MK  = MK + 1
  200     CONTINUE
C         PRINT *,'LOCATION ',ISW
          IF (ISW.NE.31) THEN
              PRINT *,'LOCATION ERROR PROCESSING PROFILER'
              IPTR(10)  = 200
              RETURN
          END IF
C                      PROCESS TIME ELEMENTS
          ISW  = 0
          DO 400 J  = 1, 7
              IF (MSTACK(1,MK).EQ.1025) THEN
C                           YEAR
                  ISW  = ISW + 1
              ELSE IF (MSTACK(1,MK).EQ.1026) THEN
C                           MONTH
                  ISW  = ISW + 2
              ELSE IF (MSTACK(1,MK).EQ.1027) THEN
C                           DAY
                  ISW  = ISW + 4
              ELSE IF (MSTACK(1,MK).EQ.1028) THEN
C                           HOUR
                  ISW  = ISW + 8
              ELSE IF (MSTACK(1,MK).EQ.1029) THEN
C                           MINUTE
                  ISW  = ISW + 16
              ELSE IF (MSTACK(1,MK).EQ.2069) THEN
C                           TIME SIGNIFICANCE
                  ISW  = ISW + 32
              ELSE IF (MSTACK(1,MK).EQ.1049) THEN
C                           TIME DISPLACEMENT
                  ISW  = ISW + 64
              ELSE
                  MK  = MK + 1
                  GO TO 400
              END IF
              JK  = JK + 1
              KPROFL(JK)  = MSTACK(1,MK)
              KPROF2(JK)  = MSTACK(2,MK)
              KSET2(JK)   = KDATA(I,MK)
C             PRINT *,JK,KPROFL(JK),KSET2(JK)
              MK  = MK + 1
  400     CONTINUE
C         PRINT *,'TIME ',ISW
          IF (ISW.NE.127) THEN
              PRINT *,'TIME ERROR PROCESSING PROFILER'
              IPTR(1)  = 201
              RETURN
          END IF
C                  SURFACE DATA
          ISW  = 0
C         PRINT *,'SURFACE'
          DO 600 K  = 1, 8
C             PRINT *,MK,MSTACK(1,MK),JK,ISW
              IF (MSTACK(1,MK).EQ.2817) THEN
                  ISW  = ISW + 1
              ELSE IF (MSTACK(1,MK).EQ.2818) THEN
                  ISW  = ISW + 2
              ELSE IF (MSTACK(1,MK).EQ.2611) THEN
                  ISW  = ISW + 4
              ELSE IF (MSTACK(1,MK).EQ.3073) THEN
                  ISW  = ISW + 8
              ELSE IF (MSTACK(1,MK).EQ.3342) THEN
                  ISW  = ISW + 16
              ELSE IF (MSTACK(1,MK).EQ.3331) THEN
                  ISW  = ISW + 32
              ELSE IF (MSTACK(1,MK).EQ.1797) THEN
                  INCRHT  = KDATA(I,MK)
                  ISW  = ISW + 64
C                 PRINT *,'INITIAL INCREMENT = ',INCRHT
                  MK  = MK + 1
C                 PRINT *,JK,KPROFL(JK),KSET2(JK),' ISW=',ISW
                  GO TO 600
              ELSE IF (MSTACK(1,MK).EQ.6433) THEN
                  ISW  = ISW + 128
              END IF
              JK  = JK + 1
              KPROFL(JK)  = MSTACK(1,MK)
              KPROF2(JK)  = MSTACK(2,MK)
              KSET2(JK)   = KDATA(I,MK)
C             PRINT *,JK,KPROFL(JK),KSET2(JK),'ISW=',ISW
              MK  = MK + 1
  600     CONTINUE
          IF (ISW.NE.255) THEN
              PRINT *,'ERROR PROCESSING PROFILER',ISW
              IPTR(1)  = 204
              RETURN
          END IF
          IF (MSTACK(1,MK).NE.1797) THEN
              PRINT *,'ERROR PROCESSING HEIGHT INCREMENT IN PROFILER'
              IPTR(1)  = 205
              RETURN
          END IF
C                   MUST SAVE THIS HEIGHT VALUE
          LHGT  = 500 + IHGT - KDATA(I,MK)
C         PRINT *,'BASE HEIGHT = ',LHGT,' INCR = ',INCRHT
          MK  = MK + 1
          IF (MSTACK(1,MK).GE.16384) THEN
              MK  = MK + 1
          END IF
C              PROCESS LEVEL DATA
C         PRINT *,'LEVEL DATA'
          DO 2000 L = 1, 43
 2020         CONTINUE
C             PRINT *,'DESC',MK,MSTACK(1,MK),JK
              ISW  = 0
C                       HEIGHT INCREMENT
              IF (MSTACK(1,MK).EQ.1797) THEN
                  INCRHT  = KDATA(I,MK)
C                 PRINT *,'NEW HEIGHT INCREMENT = ',INCRHT
                  MK  = MK + 1
C                 IF (LHGT.LT.(9250+IHGT)) THEN
C                     LHGT  = IHGT + 500 - INCRHT
C                 ELSE
C                     LHGT  = IHGT + 9250 -INCRHT
C                 END IF
              END IF
C                    MUST ENTER HEIGHT OF THIS LEVEL - DESCRIPTOR AND DA
C                     AT THIS POINT
              LHGT  = LHGT + INCRHT
C             PRINT *,'LEVEL ',L,LHGT
C             IF (L.EQ.37) THEN
C                 LHGT  = LHGT + INCRHT
C             END IF
              JK  = JK + 1
C                        SAVE DESCRIPTOR
              KPROFL(JK)  = 1798
C                       SAVE SCALE
              KPROF2(JK)  = 0
C                        SAVE DATA
              KSET2(JK)   = LHGT
C             PRINT *,KPROFL(JK),KSET2(JK),JK
              ISW  = 0
              ICON  = 1
              DO 800 J = 1, 10
750               CONTINUE
                  IF (MSTACK(1,MK).EQ.1797) THEN
                        GO TO 2020
                  ELSE IF (MSTACK(1,MK).EQ.6432) THEN
C                               HI/LO MODE
                      ISW  = ISW + 1
                  ELSE IF (MSTACK(1,MK).EQ.6434) THEN
C                               Q/C TEST
                      ISW  = ISW + 2
                  ELSE IF (MSTACK(1,MK).EQ.2070) THEN
                      IF (ICON.EQ.1) THEN
C                               FIRST PASS - U,V CONSENSUS
                           ISW  = ISW + 4
                           ICON  = ICON + 1
                      ELSE
C                               SECOND PASS - W CONSENSUS
                           ISW  = ISW + 64
                      END IF
                  ELSE IF (MSTACK(1,MK).EQ.2819) THEN
C                               U VECTOR VALUE
                      ISW  = ISW + 8
                      IF (KDATA(I,MK).GE.2047) THEN
                          VECTU  = 32767
                      ELSE
                          VECTU  = KDATA(I,MK)
                      END IF
                      MK  = MK + 1
                      GO TO 800
                  ELSE IF (MSTACK(1,MK).EQ.2820) THEN
C                               V VECTOR VALUE
                      ISW  = ISW + 16
                      IF (KDATA(I,MK).GE.2047) THEN
                          VECTV  = 32767
                      ELSE
                          VECTV  = KDATA(I,MK)
                      END IF
                      IF (IAND(ISW,1).NE.0) THEN
                          IF (VECTU.EQ.32767.OR.VECTV.EQ.32767) THEN
C                               SAVE DD DESCRIPTOR
                              JK  = JK + 1
                              KPROFL(JK)  = 2817
                              KPROF2(JK)  = 0
                              KSET2(JK)   = 32767
C                               SAVE FFF DESCRIPTOR
                              JK  = JK + 1
                              KPROFL(JK)  = 2818
                              KPROF2(JK)  = 1
                              KSET2(JK)   = 32767
                          ELSE
                              CALL W3FC05 (VECTU,VECTV,DIR,SPD)
                              NDIR  = DIR
                              SPD   = SPD
                              NSPD  = SPD
C                             PRINT *,'     ',NDIR,NSPD
C                               SAVE DD DESCRIPTOR
                              JK  = JK + 1
                              KPROFL(JK)  = 2817
                              KPROF2(JK)  = 0
                              KSET2(JK)   = NDIR
C                             IF (I.EQ.1) THEN
C                                 PRINT *,'DD ',JK,KPROFL(JK),KSET2(JK)
C                             ENDIF
C                            SAVE FFF DESCRIPTOR
                              JK  = JK + 1
                              KPROFL(JK)  = 2818
                              KPROF2(JK)  = 1
                              KSET2(JK)   = NSPD
C                             IF (I.EQ.1) THEN
C                                 PRINT *,'FFF',JK,KPROFL(JK),KSET2(JK)
C                             ENDIF
                          END IF
                          MK  = MK + 1
                          GO TO 800
                      END IF
                  ELSE IF (MSTACK(1,MK).EQ.2866) THEN
C                               SPEED STD DEVIATION
                      ISW  = ISW + 32
C -- A CHANGE BY KEYSER : POWER DESCR. BACK TO 5568
                  ELSE IF (MSTACK(1,MK).EQ.5568) THEN
C                               SIGNAL POWER
                      ISW  = ISW + 128
                  ELSE IF (MSTACK(1,MK).EQ.2822) THEN
C                               W COMPONENT
                      ISW  = ISW + 256
                  ELSE IF (MSTACK(1,MK).EQ.2867) THEN
C                              VERT STD DEVIATION
                      ISW  = ISW + 512
CVVVVVCHANGE#1 FIX BY KEYSER  -- 12/06/1994
C   NOTE: THIS FIX PREVENTS UNNECESSARY LOOPING WHEN ALL REQ. DESCR.
C         ARE MISSING.  WOULD GO INTO INFINITE LOOP EXCEPT EVENTUALLY
C         MSTACK ARRAY SIZE IS EXCEEDED AND GET FORTRAN ERROR INTERRUPT
CDAK              ELSE
                  ELSE IF ((MSTACK(1,MK)/16384).NE.0) THEN
CAAAAACHANGE#1 FIX BY KEYSER  -- 12/06/1994
                      MK  = MK + 1
                      GO TO 750
                  END IF
                  JK  = JK + 1
C                               SAVE DESCRIPTOR
                  KPROFL(JK)  = MSTACK(1,MK)
C                              SAVE SCALE
                  KPROF2(JK)  = MSTACK(2,MK)
C                               SAVE DATA
                  KSET2(JK)   = KDATA(I,MK)
                  MK  = MK + 1
C                 PRINT *,L,'TEST   ',JK,KPROFL(JK),KSET2(JK)
  800         CONTINUE
              IF (ISW.NE.1023) THEN
                  PRINT *,'LEVEL ERROR PROCESSING PROFILER',ISW
                  IPTR(1)  = 202
                  RETURN
              END IF
 2000     CONTINUE
C                           MOVE DATA BACK INTO KDATA ARRAY
          DO 5000 LL = 1, JK
C                           DATA
              KDATA(I,LL)   = KSET2(LL)
 5000     CONTINUE
 3000 CONTINUE
      DO 5005 LL = 1, JK
C                         DESCRIPTOR
          MSTACK(1,LL)  = KPROFL(LL)
C                          SCALE
          MSTACK(2,LL)  = KPROF2(LL)
C -- A CHANGE BY KEYSER : PRINT STATEMNT SHOULD BE HERE NOT IN 5000 LOOP
C             PRINT *,LL,MSTACK(1,LL),MSTACK(2,LL),(KDATA(I,LL),I=1,4)
 5005 CONTINUE
      IPTR(31)  = JK
      RETURN
      END
C> @brief Expand data/descriptor replication
C> @author Bill Cavanaugh @date 1993-05-12

C> Expand data and descriptor strings
C>
C> Program history log:
C> - Bill Cavanaugh 1993-05-12
C>
C> @param[in] IPTR See w3fi88 routine docblock
C> @param[in] IDENT See w3fi88 routine docblock
C> @param[in] MAXR Maximum number of reports/subsets that may be
C> contained in a bufr message
C> @param[in] MAXD Maximum number of descriptor combinations that
C> may be processed; upper air data and some satellite
C> data require a value for maxd of 1700, but for most
C> other data a value for maxd of 500 will suffice
C> @param[inout] KDATA Array containing decoded reports from bufr message.
C> kdata(report number,parameter number)
C> (report number limited to value of input argument
C> maxr and parameter number limited to value of input
C> argument maxd)
C> @param[inout] MSTACK List of descriptors and scale values
C> @param KNR
C> @param LDATA
C> @param LSTACK
C>
C> Error return:
C> - IPTR(1)
C>
C> @author Bill Cavanaugh @date 1993-05-12
      SUBROUTINE  FI8811(IPTR,IDENT,MSTACK,KDATA,KNR,
     *                  LDATA,LSTACK,MAXD,MAXR)

      INTEGER        IPTR(*)
      INTEGER        KNR(MAXR)
      INTEGER        KDATA(MAXR,MAXD),LDATA(MAXD)
      INTEGER        MSTACK(2,MAXD),LSTACK(2,MAXD)
      INTEGER        IDENT(*)
C
      SAVE
C
C     PRINT *,' DATA/DESCRIPTOR REPLICATION '
      DO 1000 I = 1, KNR(1)
C                           IF NOT REPLICATION DESCRIPTOR
          IF ((MSTACK(1,I)/16384).NE.1) THEN
              GO TO 1000
          END IF
C                           IF DELAYED REPLICATION DESCRIPTOR
          IF (MOD(MSTACK(1,I),256).EQ.0) THEN
C                               SAVE KX VALUE (NR DESC'S TO REPLICATE)
              KX = MOD((MSTACK(1,I)/256),64)
C                               IF NEXT DESC IS NOT 7947 OR 7948
C                                 (I.E., 0 31 011  OR 0 31 012)
              IF (MSTACK(1,I+1).NE.7947.AND.MSTACK(1,I+1).NE.7948) THEN
C                                    SKIP IT
                  GO TO 1000
              END IF
C                               GET NR REPS FROM KDATA
              NRREPS  = KDATA(1,I+1)
              LAST  = I + 1 + KX
C                               SAVE OFF TRAILING DESCS AND DATA
              KTRAIL = KNR(1) - I - 1 - KX
              DO 100 L = 1, KTRAIL
                  NX  = I + L + KX + 1
                  LDATA(L) = KDATA(1,NX)
                  LSTACK(1,L) = MSTACK(1,NX)
                  LSTACK(2,L) = MSTACK(2,NX)
  100         CONTINUE
C                              INSERT FX DESCS/DATA NR REPS TIMES
              LAST  = I + 1
              DO 400 J = 1, NRREPS
                  NX  = I + 2
                  DO 300 K = 1, KX
                      LAST  = LAST + 1
                      KDATA(1,LAST)  = KDATA(1,NX)
                      MSTACK(1,LAST) = MSTACK(1,NX)
                      MSTACK(2,LAST) = MSTACK(2,NX)
                      NX  = NX + 1
  300             CONTINUE

  400         CONTINUE
C                               RESTORE TRAILING DATA/DESCS
              DO 500 L = 1, KTRAIL
                  LAST  = LAST + 1
                  KDATA(1,LAST) = LDATA(L)
                  MSTACK(1,LAST)  = LSTACK(1,L)
                  MSTACK(2,LAST)  = LSTACK(2,L)
  500         CONTINUE
C                               RESET KNR(1)
              KNR(1)  = LAST
          END IF
 1000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8812(IPTR,IUNITB,IUNITD,ISTACK,NRDESC,KPTRB,KPTRD,
     *                     IRF1SW,NEWREF,ITBLD,ITBLD2,
     *                     KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
     *                     KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8812      BUILD TABLE B SUBSET BASED ON BUFR SEC 3
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-12-23
C
C ABSTRACT: BUILD A SUBSET OF TABLE B ENTRIES THAT CORRESPOND TO
C           THE DESCRIPTORS NEEDED FOR THIS MESSAGE
C
C PROGRAM HISTORY LOG:
C   93-05-12  CAVANAUGH
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8812(IPTR,IUNITB,IUNITD,ISTACK,NRDESC,KPTRB,KPTRD,
C    *      IRF1SW,NEWREF,ITBLD,ITBLD2,
C    *      KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
C    *      KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2)
C   INPUT ARGUMENT LIST:
C     IPTR     - SEE W3FI88 ROUTINE DOCBLOCK
C     IDENT    - SEE W3FI88 ROUTINE DOCBLOCK
C     ISTACK   - LIST OF DESCRIPTORS AND SCALE VALUES
C     IUNITB   -
C     IUNITD   -
C     ISTACK   -
C     NRDESC   -
C     KFXY2    -
C     ANAME2   -
C     AUNIT2   -
C     ISCAL2   -
C     IRFVL2   -
C     IWIDE2   -
C     IRF1SW   -
C     NEWREF   -
C     ITBLD2   -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     KDATA    - ARRAY CONTAINING DECODED REPORTS FROM BUFR MESSAGE.
C                KDATA(REPORT NUMBER,PARAMETER NUMBER)
C                (REPORT NUMBER LIMITED TO VALUE OF INPUT ARGUMENT
C                 MAXR AND PARAMETER NUMBER LIMITED TO VALUE OF INPUT
C                 ARGUMENT MAXD)
C     MSTACK   - LIST OF DESCRIPTORS AND SCALE VALUES
C     KFXY1    -
C     ANAME1   -
C     AUNIT1   -
C     ISCAL1   -
C     IRFVL1   -
C     IWIDE1   -
C     ITBLD    -
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3LIB    -
C
C REMARKS: ERROR RETURN:
C    IPTR(1) =
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
      INTEGER        KFXY1(*),ISCAL1(*),IRFVL1(3,*),IWIDE1(*)
      CHARACTER*40   ANAME1(*)
      CHARACTER*24   AUNIT1(*)
C  ..................................................
C
C        NEW            ANCILLARY TABLE B FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        KFXY2(*),ISCAL2(*),IRFVL2(*),IWIDE2(*)
      CHARACTER*64   ANAME2(*)
      CHARACTER*24   AUNIT2(*)
C  ..................................................
C
C                       ANCILLARY TABLE D FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        ITBLD2(20,*)
C  ..................................................
C
C                       NEW BASE TABLE D
C
      INTEGER        ITBLD(20,*)
C  ..................................................
      INTEGER        IPTR(*),ISTACK(*),NRDESC,NWLIST(200)
      INTEGER        NEWREF(*),KPTRB(*),KPTRD(*)
      INTEGER        IUNITB,IUNITD,ICOPY(20000),NRCOPY,IELEM,IPOS
      CHARACTER*64   AHLD64
      CHARACTER*24   AHLD24
C
      SAVE
C
C     SCAN AND DISCARD REPLICATION AND OPERATOR DESCRIPTORS
C     REPLACING SEQUENCE DESCRIPTORS WITH THEIR CORRESPONDING
C     SET OF DESCRIPTORS ALSO ELIMINATING DUPLICATES.
C
C-----------------------------------------------------------
C     PRINT *,'ENTER FI8812'
C
      DO 10 I = 1, 16384
          KPTRB(I) = -1
   10 CONTINUE
C
C
C
      IF (IPTR(14).NE.0) THEN
          DO I = 1, IPTR(14)
              KPTRB(KFXY1(I)) = I
          ENDDO
          GO TO 9000
      END IF
C
C                        READ IN TABLE B
      PRINT *,'FI8812 - READING TABLE B'
      REWIND IUNITB
      I  = 1
 4000 CONTINUE
C
      READ(UNIT=IUNITB,FMT=20,ERR=9999,END=9000)MF,
     *                    MX,MY,
     *                    (ANAME1(I)(K:K),K=1,40),
     *                    (AUNIT1(I)(K:K),K=1,24),
     *                    ISCAL1(I),IRFVL1(1,I),IWIDE1(I)
   20 FORMAT(I1,I2,I3,40A1,24A1,I5,I15,1X,I4)
      KFXY1(I)  = MF*16384 + MX*256 + MY
C     PRINT *,MF,MX,MY,KFXY1(I)
 5000 CONTINUE
      KPTRB(KFXY1(I)) = I
      IPTR(14) = I
C     PRINT *,I
C     WRITE(6,21) MF,MX,MY,KFXY1(I),
C    *                    (ANAME1(I)(K:K),K=1,40),
C    *                    (AUNIT1(I)(K:K),K=1,24),
C    *                    ISCAL1(I),IRFVL1(1,I),IWIDE1(I)
   21 FORMAT(1X,I1,I2,I3,1X,I6,1X,40A1,
     *                                 2X,24A1,2X,I5,2X,I15,1X,I4)
      I = I + 1
      GO TO 4000
C ======================================================
 9999 CONTINUE
C                        ERROR READING TABLE B
      PRINT *,'FI8812 -  ERROR READING TABLE B - RECORD ',I
      IPTR(1)  = 9
 9000 CONTINUE
      IPTR(21) =  IPTR(14)
C     PRINT *,'EXIT FI8812 - IPTR(21) =',IPTR(21),' IPTR(1) =',IPTR(1)
      RETURN
      END
      SUBROUTINE FI8813 (IPTR,MAXR,MAXD,MSTACK,KDATA,IDENT,KPTRD,KPTRB,
     *         ITBLD,ANAME1,AUNIT1,KFXY1,ISCAL1,IRFVL1,IWIDE1,IUNITB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8813      EXTRACT TABLE A, TABLE B, TABLE D ENTRIES
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-03-04
C
C ABSTRACT: EXTRACT TABLE A, TABLE B, TABLE D ENTRIES FROM A
C   DECODED BUFR MESSAGE.
C
C PROGRAM HISTORY LOG:
C   94-03-04  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8813 (IPTR,MAXR,MAXD,MSTACK,KDATA,IDENT,KPTRD,
C    *   KPTRB,ITBLD,ANAME1,AUNIT1,KFXY1,ISCAL1,IRFVL1,IWIDE1,IUNITB)
C   INPUT ARGUMENT LIST:
C     IPTR
C     MAXR
C     MAXD
C     MSTACK
C     KDATA
C     IDENT
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:
C     IUNITB
C     ITBLD1
C     ANAME1
C     AUNIT1
C     KFXY1
C     ISCAL1
C     IRFVL1
C     IWIDE1
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  NAS, CYBER, WHATEVER
C
C$$$
C  ..................................................
C
C        NEW            ANCILLARY TABLE B FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        KFXY1(*),ISCAL1(*),IRFVL1(*),IWIDE1(*)
      CHARACTER*40   ANAME1(*)
      CHARACTER*24   AUNIT1(*)
C  ..................................................
C
C                        TABLE D
C
      INTEGER        ITBLD(20,*)
C  ..................................................
      CHARACTER*32      SPACES
      CHARACTER*8       ASCCHR
      CHARACTER*32      AAAA
C
      INTEGER           I1(20),I2(20),I3(20),KPTRB(*)
      INTEGER           IPTR(*),MAXR,MAXD,MSTACK(2,MAXD)
      INTEGER           IXA, IXB, IXD, KDATA(MAXR,MAXD)
      INTEGER           IEXTRA,KPTRD(*)
      INTEGER           KEYSET,ISCSGN(200),IRFSGN(200)
      INTEGER           IDENT(*),IHOLD,JHOLD(8),IUNITB
      EQUIVALENCE       (IHOLD,ASCCHR),(JHOLD,AAAA)
      SAVE
      DATA              SPACES/'                                '/
      DATA              IEXTRA/0/
      DATA              KEYSET/0/

C  ==============================================================
C     PRINT *,'FI8813',IPTR(41),IPTR(42),IPTR(31),IPTR(21)
C                                 BUILD SPACE CONSTANT
C                                INITIALIZE ENTRY COUNTS
      IXA  = 0
C                       NUMBER IN TABLE B
      IXB  = IPTR(21)
C
C
C                           SET FOR COMPRESSED OR NON COMPRESSED
C                                     PROCESSING
C
C     PRINT *,'FI8813 - 2',IDENT(16),IDENT(14)
      IF (IDENT(16).EQ.0) THEN
          JK  = 1
      ELSE
          JK  = IDENT(14)
      END IF
C     PRINT *,'FI8813 - 3,  JK=',JK
C
C
C                              START PROCESSING ENTRIES
C     PRINT *,'START PROCESSING ENTRIES'
C
C     DO 995 I = 1, IPTR(31)
C         IF (IPTR(45).EQ.4) THEN
C             PRINT 9958,I,MSTACK(1,I),KDATA(1,I),KDATA(1,I)
C9958         FORMAT (1X,I5,2X,I5,2X,Z8,2X,A4)
C         ELSE
C             PRINT 9959,I,MSTACK(1,I),KDATA(1,I),KDATA(1,I)
C9959         FORMAT (1X,I5,2X,I5,2X,Z16,2X,A8)
C         END IF
C 995 CONTINUE
C     PRINT *,' '
      I       = 0
      IEXTRA  = 0
 1000 CONTINUE
C
C                               SET POINTER TO CORRECT DATA POSITION
C                                 I IS THE NUMBER OF DESCRIPTORS
C                                 IEXTRA IS THE NUMBER OF WORDS ADDED
C                                       FOR TEXT DATA
C
      I  = I + 1
      IF (I.GT.IPTR(31)) THEN
C                                 RETURN IF COMPLETED SEARCH
          GO TO 9000
      END IF
      KLK  = I + IEXTRA
C     PRINT *,'ENTRY',KLK,I,IPTR(31),IEXTRA,MSTACK(1,KLK)
C
C                               IF TABLE A ENTRY OR EDITION NUMBER
C                                OR IF DESCRIPTOR IS NOT IN CLASS 0
C                                    SKIP OVER
C
      IF (MSTACK(1,KLK).EQ.1) THEN
C         PRINT *,'A ENTRY'
          GO TO 1000
      ELSE IF (MSTACK(1,KLK).EQ.2) THEN
C         PRINT *,'A ENTRY LINE 1'
          IEXTRA  = IEXTRA + 32 / IPTR(45) - 1
          GO TO 1000
      ELSE IF (MSTACK(1,KLK).EQ.3) THEN
C         PRINT *,'A ENTRY LINE 2'
          IEXTRA  = IEXTRA + 32 / IPTR(45) - 1
          GO TO 1000
      ELSE IF (MSTACK(1,KLK).GE.34048.AND.MSTACK(1,KLK).LE.34303) THEN
          LY = MOD(MSTACK(1,KLK),256)
C         PRINT *,'CLASS C - HAVE',LY,' BYTES OF TEXT'
          IF (MOD(LY,IPTR(45)).EQ.0) THEN
              IWDS  = LY / IPTR(45)
          ELSE
              IWDS  = LY / IPTR(45) + 1
          END IF
          IEXTRA  = IEXTRA + IWDS - 1
          GO TO 1000
      ELSE IF (MSTACK(1,KLK).LT.10.OR.MSTACK(1,KLK).GT.255) THEN
C         PRINT *,MSTACK(1,KLK),'          NOT CLASS 0'
          GO TO 1000
      END IF
C
C                              MUST FIND F X Y KEY FOR TABLE B
C                              OR TABLE D ENTRY
C
      IZ  = 1
      KEYSET  = 0
   10 CONTINUE
      IF (I.GT.IPTR(31)) THEN
          GO TO 9000
      END IF
      KLK  = I + IEXTRA
      IF (MSTACK(1,KLK).GE.34048.AND.MSTACK(1,KLK).LE.34303) THEN
          LY = MOD(MSTACK(1,KLK),256)
C         PRINT *,'TABLE C - HAVE',LY,' TEXT BYTES'
          IF (MOD(LY,4).EQ.0) THEN
              IWDS  = LY / IPTR(45)
          ELSE
              IWDS  = LY / IPTR(45) + 1
          END IF
          IEXTRA  = IEXTRA + IWDS - 1
          I  = I + 1
          GO TO 10
      ELSE IF (MSTACK(1,KLK)/16384.NE.0) THEN
          IF (MOD(MSTACK(1,KLK),256).EQ.0) THEN
              I  = I + 1
          END IF
          I  = I + 1
          GO TO 10
      END IF
      IF (MSTACK(1,KLK).GE.10.AND.MSTACK(1,KLK).LE.12) THEN
C         PRINT *,'FIND KEY'
C
C                  MUST INCLUDE PROCESSING FOR COMPRESSED DATA
C
C                               BUILD DESCRIPTOR SEGMENT
C
          IF (MSTACK(1,KLK).EQ.10) THEN
              CALL FI8814 (KDATA(IZ,KLK),1,MF,IERR,IPTR)
C             PRINT *,'F =',MF,KDATA(IZ,KLK),IPTR(31),I,IEXTRA
              KEYSET  = IOR(KEYSET,4)
          ELSE IF (MSTACK(1,KLK).EQ.11) THEN
              CALL FI8814 (KDATA(IZ,KLK),2,MX,IERR,IPTR)
C             PRINT *,'X =',MX,KDATA(IZ1,KLK)
              KEYSET  = IOR(KEYSET,2)
          ELSE IF (MSTACK(1,KLK).EQ.12) THEN
              CALL FI8814 (KDATA(IZ,KLK),3,MY,IERR,IPTR)
C             PRINT *,'Y =',MY,KDATA(IZ,KLK)
              KEYSET  = IOR(KEYSET,1)
          END IF
C         PRINT *,'          KEYSET =',KEYSET
          I = I + 1
          GO TO 10
      END IF
      IF (KEYSET.EQ.7) THEN
C         PRINT *,'HAVE KEY DESCRIPTOR',MF,MX,MY
C
C                               TEST NEXT DESCRIPTOR FOR TABLE B
C                               OR TABLE D ENTRY, PROCESS ACCORDINGLY
C
          KLK  = I + IEXTRA
C         PRINT *,'DESC ',MSTACK(1,KLK),KLK,I,IEXTRA,KDATA(1,KLK)
          IF (MSTACK(1,KLK).EQ.30) THEN
              IXD  = IPTR(20) + 1
              ITBLD(1,IXD) =16384 * MF + 256 * MX + MY
C             PRINT *,'SEQUENCE DESCRIPTOR',MF,MX,MY,ITBLD(1,IXD)
              GO TO 300
          ELSE IF (MSTACK(1,KLK).GE.13.AND.MSTACK(1,KLK).LE.20) THEN
              KFXY1(IXB+IZ) = 16384 * MF + 256 * MX + MY
C             PRINT *,'ELEMENT DESCRIPTOR',MF,MX,MY,KFXY1(IXB+IZ),IXB+IZ
              KPTRB(KFXY1(IXB+IZ)) = IXB+IZ
              GO TO 200
          ELSE
          END IF
C         I = I + 1
C         IF (I.GT.IPTR(31)) THEN
C             GO TO 9000
C         END IF
C         GO TO 10
      END IF
      GO TO 1000
C  ==================================================================
  200 CONTINUE
      IBFLAG   = 1
   20 CONTINUE
      KLK  = I + IEXTRA
C     PRINT *,'ZZZ',KLK,I,IEXTRA,MSTACK(1,KLK),KDATA(IZ,KLK)
      IF (MSTACK(1,KLK).LT.13.OR.MSTACK(1,KLK).GT.20) THEN
              PRINT *,'IMPROPER SEQUENCE OF DESCRIPTORS IN LIST'
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.13) THEN
C         PRINT *,'13      NAME',KLK
C
C                              ELEMENT NAME PART 1 - 32 BYTES
C                                    FOR THIS PARAMETER
          JJ  = IEXTRA
          DO 21 LL = 1, 32, IPTR(45)
              LLL    = LL + IPTR(45) - 1
              KQK = I + JJ
              IHOLD  = KDATA(IZ,KQK)
              IF (IPTR(37).EQ.0) THEN
C                 CALL W3AI39 (IDATA,IPTR(45))
              END IF
              ANAME1(IXB+IZ)(LL:LLL) = ASCCHR
              JJ  = JJ + 1
   21     CONTINUE
          IEXTRA  = IEXTRA + (32 / IPTR(45)) - 1
          IBFLAG   = IOR(IBFLAG,64)
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.14) THEN
C         PRINT *,'14      NAME2',KLK
C
C                              ELEMENT NAME PART 2 - 32 BYTES
C
C                                    FOR THIS PARAMETER
          JJ  = IEXTRA
          DO 22 LL = 33, 64, IPTR(45)
              LLL    = LL + IPTR(45) - 1
              KQK = I + JJ
              IHOLD  = KDATA(IZ,KQK)
              IF (IPTR(37).EQ.0) THEN
C                 CALL W3AI39 (ASCCHR,IPTR(45))
              END IF
              ANAME1(IXB+IZ)(LL:LLL) = ASCCHR
              JJ  = JJ + 1
   22     CONTINUE
          IEXTRA  = IEXTRA + (32 / IPTR(45)) - 1
          IBFLAG   = IOR(IBFLAG,32)
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.15) THEN
C         PRINT *,'15      UNITS',KLK
C
C                              UNITS NAME - 24 BYTES
C
C                                    FOR THIS PARAMETER
          JJ  = IEXTRA
          DO 23 LL = 1, 24, IPTR(45)
              LLL    = LL + IPTR(45) - 1
              KQK = I + JJ
              IHOLD  = KDATA(IZ,KQK)
              IF (IPTR(37).EQ.0) THEN
C                 CALL W3AI39 (ASCCHR,IPTR(45))
              END IF
              AUNIT1(IXB+IZ)(LL:LLL) = ASCCHR
              JJ  = JJ + 1
   23     CONTINUE
          IEXTRA  = IEXTRA + (24 / IPTR(45)) - 1
          IBFLAG   = IOR(IBFLAG,16)
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.16) THEN
C         PRINT *,'16      SCALE SIGN'
C
C                               SCALE SIGN - 1 BYTE
C                              0 = POS, 1 = NEG
          IHOLD = KDATA(IZ,KLK)
          KLK  = I + IEXTRA
          IF (INDEX(ASCCHR,'-').EQ.0) THEN
              ISCSGN(IZ)  = 1
          ELSE
              ISCSGN(IZ)  = -1
          END IF
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.17) THEN
C         PRINT *,'17      SCALE',KLK
C
C                               SCALE - 3 BYTES
C
          KLK = I + IEXTRA
          CALL FI8814(KDATA(IZ,KLK),3,ISCAL1(IXB+IZ),IERR,IPTR)
          IF (IERR.NE.0) THEN
              PRINT *,'NON-NUMERIC CHAR - CANNOT CONVERT'
              IPTR(1) = 888
              GO TO 9000
          END IF
          ISCAL1(IXB+IZ) = ISCAL1(IXB+IZ) * ISCSGN(IZ)
          IBFLAG   = IOR(IBFLAG,8)
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.18) THEN
C         PRINT *,'18      REFERENCE SCALE',KLK
C
C                              REFERENCE SIGN - 1 BYTE
C                              0 = POS, 1 = NEG
C
          KLK  = I + IEXTRA
          IHOLD  = KDATA(IZ,KLK)
          IF (INDEX(ASCCHR,'-').EQ.0) THEN
              IRFSGN(IZ) =  1
          ELSE
              IRFSGN(IZ) = -1
          END IF
C  ===============================================================
      ELSE IF (MSTACK(1,KLK).EQ.19) THEN
C         PRINT *,'19      REFERENCE VALUE',KLK
C
C                              REFERENCE VALUE - 10 BYTES/ 3 WDS
C
          JJ  = IEXTRA
          KQK  = I + JJ
          KM  = 0
          DO 26 LL = 1, 12, IPTR(45)
              KQK = I + JJ
              KM  = KM + 1
              JHOLD(KM)  = KDATA(IZ,KQK)
              JJ  = JJ + 1
   26     CONTINUE
          CALL FI8814(AAAA,10,IRFVL1(IXB+IZ),IERR,IPTR)
          IF (IERR.NE.0) THEN
              PRINT *,'NON-NUMERIC CHARACTER-CANNOT CONVERT'
              IPTR(1)  = 888
              GO TO 9000
          END IF
          IRFVL1(IXB+IZ) = IRFVL1(IXB+IZ) * IRFSGN(IZ)
          IEXTRA  = IEXTRA + 10 / IPTR(45)
C         DO 261 IZ = 1, JK
C             PRINT *,'RFVAL',IXB+IZ,JK,IRFVL1(IXB+IZ)
C 261     CONTINUE
          IBFLAG   = IOR(IBFLAG,4)
C  ===============================================================
      ELSE
C         PRINT *,'20      WIDTH',KLK
C
C                              ELEMENT DATA WIDTH - 3 BYTES
C
C         DO 27 LL = 1, 24, IPTR(45)
              KLK = I + IEXTRA
C             DO 270 IZ = 1, JK
                  CALL FI8814(KDATA(IZ,KLK),3,IWIDE1(IXB+IZ),IERR,IPTR)
                  IF (IERR.NE.0) THEN
                      PRINT *,'NON-NUMERIC CHAR - CANNOT CONVERT'
                      IPTR(1) = 888
                      GO TO 9000
                  END IF
                  IF (IWIDE1(IXB+IZ).LT.1) THEN
                      IPTR(1) = 890
C                    PRINT *,'CLASS 0 DESCRIPTOR, WIDTH=0',KFXY1(IXB+IZ)
                      GO TO 9000
                  END IF
C 270         CONTINUE
C  27     CONTINUE
          IBFLAG   = IOR(IBFLAG,2)
      END IF
C                                       NO, IT ISN'T
C
C                             IF THERE ARE ENOUGH OF THE ELEMENTS
C                             NECESSARY TO ACCEPT A TABLE B ENTRY
C
C         PRINT *,'                      IBFLAG =',IBFLAG
      IF (IBFLAG.EQ.127) THEN
C         PRINT *,'COMPLETE TABLE B ENTRY'
C                  HAVE A COMPLETE TABLE B ENTRY
          IXB  = IXB + 1
C         PRINT *,'B',IXB,JK,KFXY1(IXB),ANAME1(IXB)
C         PRINT *,'     ',AUNIT1(IXB),ISCAL1(IXB),
C    *    IRFVL1(IXB),IWIDE1(IXB)
          IPTR(21) = IXB
          GO TO 1000
      END IF
      I  = I + 1
C
C                             CHECK NEXT DESCRIPTOR
C
      IF (I.GT.IPTR(31)) THEN
C                                 RETURN IF COMPLETED SEARCH
          GO TO 9000
      END IF
      GO TO 20
C  ==================================================================
  300 CONTINUE
      ISEQ  = 0
      IJK  = IPTR(20) + 1
C     PRINT *,'SEQUENCE DESCRIPTOR',MF,MX,MY,ITBLD(1,IXD),' FOR',IJK
   30 CONTINUE
      KLK  = I + IEXTRA
C     PRINT *,'HAVE A SEQUENCE DESCRIPTOR',KLK,KDATA(IZ,KLK)
      IF (MSTACK(1,KLK).EQ.30) THEN
C                             FROM TEXT FIELD (6 BYTES/2 WDS)
C                             STRIP OUT NEXT DESCRIPTOR IN SEQUENCE
C
C                             F - EXTRACT AND CONVERT TO DECIMAL
          JJ  = IEXTRA
          KK  = 0
          DO 351 LL = 1, 6, IPTR(45)
              KQK  = I + JJ
              KK = KK + 1
              JHOLD(KK)  = KDATA(1,KQK)
              JJ  = JJ + 1
              IF (LL.GT.1) IEXTRA  = IEXTRA + 1
  351     CONTINUE
C         PRINT 349,KDATA(1,KQK)
  349     FORMAT (6X,Z24)
C                                     CONVERT TO INTEGER
          CALL FI8814(AAAA,6,IHOLD,IERR,IPTR)
C         PRINT *,'                         ',IHOLD
          IF (IERR.NE.0) THEN
              PRINT *,'NON NUMERIC CHARACTER FOUND IN F X Y'
              IPTR(1)  = 888
              GO TO 9000
          END IF
C                                CONSTRUCT SEQUENCE DESCRIPTOR
          IFF  = IHOLD / 100000
          IXX  = MOD((IHOLD/1300),100)
          IYY  = MOD(IHOLD,1300)
C                                 INSERT IN PROPER SEQUENCE
          ITBLD(ISEQ+2,IJK) = 16384 * IFF + 256 * IXX + IYY
C         PRINT *,'    SEQUENCE',IZ,AAAA,IHOLD,ITBLD(ISEQ+2,IJK),
C    *                     IFF,IXX,IYY
          ISEQ  = ISEQ + 1
          IF (ISEQ.GT.18) THEN
              IPTR(1) = 30
              RETURN
          END IF
C                                SET TO LOOK AT NEXT DESCRIPTOR
          I  = I + 1
C         IF (IPTR(45).LT.6) THEN
C             IEXTRA  = IEXTRA + 1
C         END IF
          GO TO 30
      ELSE
C                     NEXT DESCRIPTOR IS NOT A SEQUENCE DESCRIPTOR
          IF (ISEQ.GE.1) THEN
C                           HAVE COMPLETE TABLE D ENTRY
              IPTR(20) = IPTR(20) + 1
C             PRINT *,'                        INTO LOCATION ',IPTR(20)
              LZ       = ITBLD(1,IJK)
              MZ       = MOD(LZ,16384)
              KPTRD(MZ) = IJK
              I  = I - 1
          END IF
      END IF
C                              GO TEST  NEXT DESCRIPTOR
      GO TO 1000
C  ==================================================================
 9000 CONTINUE
C     PRINT *,IPTR(21),' ENTRIES IN ANCILLARY TABLE B'
C     PRINT *,IPTR(20),' ENTRIES IN ANCILLARY TABLE D'
C     DO 9050 L = 1, 16384
C         IF (KPTRD(L).GT.0) PRINT *,' D',L+32768, KPTRD(L)
C9050 CONTINUE
C     IF (I.GE.IPTR(31)) THEN
C
C       FILE FOR MODIFIED TABLE B OUTPUT
          NUMNUT = IUNITB + 1
          REWIND NUMNUT
C
C             PRINT *,' HERE IS THE NEW TABLE B',IPTR(21)
              DO 2000 KB = 1, IPTR(21)
                  JF  = KFXY1(KB) / 16384
                  JX  = MOD((KFXY1(KB) / 256),64)
                  JY  = MOD(KFXY1(KB),256)
C                 WRITE (6,2001)JF,JX,JY,ANAME1(KB),
C    *                    AUNIT1(KB),ISCAL1(KB),IRFVL1(KB),IWIDE1(KB)
                  WRITE (NUMNUT,5000)JF,JX,JY,ANAME1(KB)(1:40),
     *               AUNIT1(KB)(1:24),ISCAL1(KB),IRFVL1(KB),IWIDE1(KB)
 5000             FORMAT(I1,I2,I3,A40,A24,I5,I15,I5)
 2000         CONTINUE
 2001         FORMAT (1X,I1,1X,I2,1X,I3,2X,A40,3X,A24,2X,I5,2X,I12,
     *                     2X,I4)
C
              ENDFILE NUMNUT
C
          IF (IPTR(20).NE.0) THEN
C                                  PRINT OUT TABLE
C             PRINT *,' HERE IS THE UPGRADED TABLE D'
C             DO 3000 KB = 1, IPTR(20)
C                 PRINT 3001,KB,(ITBLD(K,KB),K=1,15)
C3000         CONTINUE
C3001         FORMAT (16(1X,I5))
          END IF
C                                    EXIT ROUTINE, ALL DONE WITH PASS
C     END IF
      RETURN
      END
      SUBROUTINE FI8814 (ASCCHR,NPOS,NEWVAL,IERR,IPTR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8814      CONVERT TEXT TO INTEGER
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-03-04
C
C ABSTRACT: CONVERT TEXT CHARACTERS TO INTEGER VALUE
C
C PROGRAM HISTORY LOG:
C   94-03-04  CAVANAUGH
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8814 (ASCCHR,NPOS,NEWVAL,IERR,IPTR)
C   INPUT ARGUMENT LIST:
C     ASCCHR   -
C     NPOS     -
C     NEWVAL   -
C     IERR     -
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C     DDNAME1  - GENERIC NAME & CONTENT
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C     DDNAME2  - GENERIC NAME & CONTENT AS ABOVE
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  NAS, CYBER, WHATEVER
C
C$$$
      INTEGER      IERR, IHOLD, IPTR(*)
      CHARACTER*8  AHOLD
      CHARACTER*64 ASCCHR
      EQUIVALENCE  (IHOLD,AHOLD)

      SAVE
C  ----------------------------------------------------------
      IERR    = 0
      NEWVAL  = 0
      IFLAG  = 0
C
      DO 1000 I = 1, NPOS
          IHOLD  = 0
          AHOLD(IPTR(45):IPTR(45)) = ASCCHR(I:I)
          IF (IPTR(37).EQ.1) THEN
              IF (IHOLD.EQ.32) THEN
                  IF (IFLAG.EQ.0) GO TO 1000
                  GO TO 2000
              ELSE IF (IHOLD.LT.48.OR.IHOLD.GT.57) THEN
C                 PRINT*,' ASCII IHOLD =',IHOLD
                  IERR       = 1
                  RETURN
              ELSE
                  IFLAG  = 1
                  NEWVAL     = NEWVAL * 10 + IHOLD - 48
              END IF
          ELSE
              IF (IHOLD.EQ.64) THEN
                  IF (IFLAG.EQ.0) GO TO 1000
                  GO TO 2000
              ELSE IF (IHOLD.LT.240.OR.IHOLD.GT.249) THEN
C                 PRINT*,' EBCIDIC IHOLD =',IHOLD
                  IERR       = 1
                  RETURN
              ELSE
                  IFLAG  = 1
                  NEWVAL     = NEWVAL * 10 + IHOLD - 240
              END IF
          END IF
 1000 CONTINUE
 2000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8815(IPTR,IDENT,JDESC,KDATA,KFXY3,MAXR,MAXD,
     *                                ANAME3,AUNIT3,
     *                                ISCAL3,IRFVL3,IWIDE3,
     *                                KEYSET,IBFLAG,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8815      EXTRACT TABLE A, TABLE B, TABLE D ENTRIES
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 94-03-04
C
C ABSTRACT: EXTRACT TABLE A, TABLE B, ENTRIES FROM ACTIVE BUFR MESSAGE
C   TO BE RETAINED FOR USE DURING THE DECODING OF ACTIVE BUFR MESSAGE.
C   THESE WILL BE DISCARDED WHEN DECODING OF CURRENT MESSAGE IS COMPLETE
C
C PROGRAM HISTORY LOG:
C   94-03-04  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8815(IPTR,IDENT,JDESC,KDATA,KFXY3,MAXR,MAXD,
C    *                                ANAME3,AUNIT3,
C    *                                ISCAL3,IRFVL3,IWIDE3,
C    *                                KEYSET,IBFLAG,IERR)
C   INPUT ARGUMENT LIST:
C     IPTR     -
C     MAXR     -
C     MAXD     -
C     MSTACK   -
C     KDATA    -
C     IDENT    -
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:
C     ANAME3   -
C     AUNIT3   -
C     KFXY3    -
C     ISCAL3   -
C     IRFVL3   -
C     IWIDE3   -
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  NAS, CYBER
C
C$$$
      CHARACTER*64      ANAME3(*),SPACES
      CHARACTER*24      AUNIT3(*)
C
      INTEGER           IPTR(*),MAXR,MAXD,JDESC
      INTEGER           IXA, IXB, IXD, KDATA(MAXR,MAXD)
      INTEGER           IEXTRA
      INTEGER           KEYSET
      INTEGER           KFXY3(*),IDENT(*)
      INTEGER           ISCAL3(*),ISCSGN(150)
      INTEGER           IRFVL3(*),IRFSGN(150)
      INTEGER           IWIDE3(*)

      SAVE
C  ==============================================================
C     PRINT *,'FI8815'
      IEXTRA  = 0
C                                 BUILD SPACE CONSTANT
      DO 1 I = 1, 64
          SPACES(I:I)  = ' '
    1 CONTINUE
C                                INITIALIZE ENTRY COUNTS
      IXA  = 0
      IXB  = 0
      IXD  = 0
C
C                           SET FOR COMPRESSED OR NON COMPRESSED
C                                     PROCESSING
C
      IF (IDENT(16).EQ.0) THEN
          JK  = 1
      ELSE
          JK  = IDENT(14)
      END IF
C
C                           CLEAR NECESSARY ENTRIES
C
      DO 2 IY = 1, JK
C
C                           CLEAR NEXT TABLE B ENTRY
C
          KFXY3(IXB+IY)   = 0
          ANAME3(IXB+IY)(1:64)  = SPACES(1:64)
          AUNIT3(IXB+IY)(1:24)  = SPACES(1:24)
          ISCAL3(IXB+IY)  = 0
          IRFVL3(IXB+IY)  = 0
          IWIDE3(IXB+IY)  = 0
          ISCSGN(IY)      = 1
          IRFSGN(IY)      = 1
    2 CONTINUE
C
C                              START PROCESSING ENTRIES
C
      I  = 0
 1000 CONTINUE
C
C                               SET POINTER TO CORRECT DATA POSITION
C
      K  = I + IEXTRA
C
C                              MUST FIND F X Y KEY FOR TABLE B
C                              OR TABLE D ENTRY
C
      IF (JDESC.GE.10.AND.JDESC.LE.12) THEN
   10     CONTINUE
C
C                               BUILD DESCRIPTOR SEGMENT
C
          DO 20 LY  = 1,JK
              IF (JDESC.EQ.10) THEN
                  KFXY3(IXB+LY) = KDATA(K,1) * 16384 + KFXY3(IXB+LY)
                  KEYSET  = IOR(KEYSET,4)
                  I = I + 1
                  GO TO 10
              ELSE IF (JDESC.EQ.11) THEN
                  KFXY3(IXB+LY) = KDATA(K,1) * 256 + KFXY3(IXB+LY)
                  KEYSET  = IOR(KEYSET,2)
                  I = I + 1
                  GO TO 10
              ELSE IF (JDESC.EQ.12) THEN
                  KFXY3(IXB+LY) = KDATA(K,1) + KFXY3(IXB+LY)
                  KEYSET  = IOR(KEYSET,1)
              END IF
   20     CONTINUE
C  ==================================================================
      ELSE IF (JDESC.GE.13.AND.JDESC.LE.20) THEN
          DO 250 IZ = 1, JK
              IF (JDESC.EQ.13) THEN
C
C                              ELEMENT NAME PART 1 - 32 BYTES/8 WDS
C
                  CALL GBYTES (ANAME3(IXB+IZ),KDATA(K,IZ),0,32,0,8)
                  IBFLAG   = IOR(IBFLAG,16)
              ELSE IF (JDESC.EQ.14) THEN
C
C                              ELEMENT NAME PART 2 - 32 BYTES/8 WDS
C
                 CALL GBYTES(ANAME3(IXB+IZ)(33:33),KDATA(K,IZ),0,32,0,8)
              ELSE IF (JDESC.EQ.15) THEN
C
C                              UNITS NAME - 24 BYTES/6 WDS
C
                  CALL GBYTES (AUNIT3(IXB+IZ)(1:1),KDATA(K,IZ),0,32,0,6)
                  IBFLAG   = IOR(IBFLAG,8)
              ELSE IF (JDESC.EQ.16) THEN
C
C                              UNITS SCALE SIGN - 1 BYTE/ 1 WD
C                              0 = POS, 1 = NEG
                  IF (KDATA(K,1).NE.48) THEN
                      ISCSGN(IZ)  = -1
                  ELSE
                      ISCSGN(IZ)  = 1
                  END IF
              ELSE IF (JDESC.EQ.17) THEN
C
C                              UNITS SCALE - 3 BYTES/ 1 WD
C
                  CALL FI8814(KDATA(K,IZ),3,ISCAL3(IXB+IZ),IERR,IPTR)
                  IF (IERR.NE.0) THEN
                      PRINT *,'NON-NUMERIC CHARACTER - CANNOT CONVERT'
                      IPTR(1) = 888
                      RETURN
                  END IF
                  IBFLAG   = IOR(IBFLAG,4)
              ELSE IF (JDESC.EQ.18) THEN
C
C                              UNITS REFERENCE SIGN - 1 BYTE/ 1 WD
C                              0 = POS, 1 = NEG
C
                  IF (KDATA(K,1).EQ.48) THEN
                      IRFSGN(IZ) =  1
                  ELSE
                      IRFSGN(IZ) = -1
                  END IF
              ELSE IF (JDESC.EQ.19) THEN
C
C                              UNITS REFERENCE VALUE - 10 BYTES/ 3 WDS
C
                  CALL FI8814(KDATA(K,IZ),10,IRFVL3(IXB+IZ),IERR,IPTR)
                  IF (IERR.NE.0) THEN
                      PRINT *,'NON-NUMERIC CHARACTER-CANNOT CONVERT'
                      IPTR(1)  = 888
                      RETURN
                  END IF
                  IBFLAG   = IOR(IBFLAG,2)
              ELSE
C
C                              ELEMENT DATA WIDTH - 3 BYTES/ 1 WD
C
                  CALL FI8814(KDATA(K,1),3,IWIDE3(IXB+1),IERR,IPTR)
                  IF (IERR.NE.0) THEN
                      PRINT *,'NON-NUMERIC CHARACTER-CANNOT CONVERT'
                      IPTR(1)  = 888
                      RETURN
                  END IF
                  IBFLAG   = IOR(IBFLAG,1)
              END IF
  250     CONTINUE
      END IF
C  ==================================================================
 9000 RETURN
      END
      SUBROUTINE FI8818(IPTR,
     *                KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
     *                KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2,
     *                KPTRB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8818      MERGE ANCILLARY & STANDARD B ENTRIES
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: YY-MM-DD
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  SEE NMC HANDBOOK SECTION 3.1.1. FOR DETAILS
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8818(IPTR,
C    *                KFXY1,ANAME1,AUNIT1,ISCAL1,IRFVL1,IWIDE1,
C    *                KFXY2,ANAME2,AUNIT2,ISCAL2,IRFVL2,IWIDE2,KPTRB)
C   INPUT ARGUMENT LIST:
C     IPTR     -
C     KFXY1    -
C     ANAME1   -
C     AUNIT1   -
C     ISCAL1   -
C     IRFVL1   -
C     IWIDE1   -
C     KFXY2    -
C     ANAME2   -
C     AUNIT2   -
C     ISCAL2   -
C     IRFVL2   -
C     IWIDE2   -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IPTR     -
C     KFXY1    -
C     ANAME1   -
C     AUNIT1   -
C     ISCAL1   -
C     IRFVL1   -
C     IWIDE1   -
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  NAS, CYBER, WHATEVER
C
C$$$
C  ..................................................
C
C     NEW               BASE TABLE B
C                            MAY BE A COMBINATION OF MASTER TABLE B
C                            AND ANCILLARY TABLE B
C
      INTEGER        KFXY1(*),ISCAL1(*),IRFVL1(3,*),IWIDE1(*)
      CHARACTER*40   ANAME1(*)
      CHARACTER*24   AUNIT1(*)
C  ..................................................
C
C        NEW            ANCILLARY TABLE B FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        KFXY2(*),ISCAL2(*),IRFVL2(*),IWIDE2(*)
      CHARACTER*64   ANAME2(*)
      CHARACTER*24   AUNIT2(*)
C  ..................................................
      INTEGER           IPTR(*),KPTRB(*)

      SAVE
C
C                       SET UP POINTERS
C     PRINT *,'FI8818-A',IPTR(21),IPTR(41)
      KAB   = 1
      KB    = 1
 1000 CONTINUE
C     PRINT *,KB,KAB,KFXY1(KB),KFXY2(KAB),IPTR(21)
      IF (KB.GT.IPTR(21)) THEN
C                                 NO MORE MASTER ENTRIES
C         PRINT *,'NO MORE MASTER ENTRIES'
          IF (KAB.GT.IPTR(41)) THEN
               GO TO 5000
          END IF
C                                  APPEND ANCILLARY ENTRY
          GO TO 2000
      ELSE IF (KB.LE.IPTR(21)) THEN
C                                  HAVE MORE MASTER ENTRIES
          IF (KAB.GT.IPTR(41)) THEN
C                                  NO MORE ANCILLARY ENTRIES
              GO TO 5000
          END IF
          IF (KFXY2(KAB).EQ.KFXY1(KB)) THEN
C                                  REPLACE MASTER ENTRY
              GO TO 3000
          ELSE IF (KFXY2(KAB).LT.KFXY1(KB)) THEN
C                                  INSERT ANCILLARY ENTRY
              GO TO 2000
          ELSE IF (KFXY2(KAB).GT.KFXY1(KB)) THEN
C                                  SKIP MASTER ENTRY
              KB  = KB + 1
          END IF
      END IF
      GO TO 1000
 2000 CONTINUE
      IPTR(21)  = IPTR(21) + 1
      KPTRB(KFXY2(KAB)) = IPTR(21)
C                                  APPEND ANCILLARY ENTRY
      KFXY1(IPTR(21))        = KFXY2(KAB)
      ANAME1(IPTR(21))(1:40) = ANAME2(KAB)(1:40)
      AUNIT1(IPTR(21))       = AUNIT2(KAB)
      ISCAL1(IPTR(21))       = ISCAL2(KAB)
      IRFVL1(1,IPTR(21))       = IRFVL2(KAB)
      IWIDE1(IPTR(21))       = IWIDE2(KAB)
C     PRINT *,IPTR(21),KFXY1(IPTR(21)),' APPENDED'
      KAB                   = KAB + 1
      GO TO 1000
 3000 CONTINUE
C                                  REPLACE MASTER ENTRY
      KFXY1(KB)       = KFXY2(KAB)
      ANAME1(KB)      = ANAME2(KAB)(1:40)
      AUNIT1(KB)      = AUNIT2(KAB)
      ISCAL1(KB)      = ISCAL2(KAB)
      IRFVL1(1,KB)    = IRFVL2(KAB)
      IWIDE1(KB)      = IWIDE2(KAB)
C     PRINT *,KB,KFXY1(KB),'REPLACED',IWIDE1(KB)
      KAB             = KAB + 1
      KB              = KB + 1
      GO TO 1000
 5000 CONTINUE
      IPTR(41)  = 0
C                                  PROCESSING COMPLETE
C     PRINT *,'FI8818-B',IPTR(21),IPTR(41)
C     DO 6000 I = 1, IPTR(21)
C         PRINT *,'FI8818-C',I,KFXY1(I),IWIDE1(I)
C6000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8819(IPTR,ITBLD,ITBLD2,KPTRD)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8819      MERGE ANCILLARY & MASTER TABLE D
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: YY-MM-DD
C
C ABSTRACT: MERGE TABLE D ENTRIES WITH THE ENTRIES FROM THE STANDARD
C   TABLE D. ASSURE THAT ENTRIES ARE SEQUENTIAL.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8819(IPTR,ITBLD,ITBLD2,KPTRD)
C   INPUT ARGUMENT LIST:
C     IPTR     -
C     ITBLD    -
C     ITBLD2   -
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     IPTR     -
C     ITBLD    -
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS, CYBER
C
C$$$
C  ..................................................
C
C                       ANCILLARY TABLE D FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        ITBLD2(20,*)
C  ..................................................
C
C                       NEW BASE TABLE D
C
      INTEGER        ITBLD(20,*)
C  ..................................................
      INTEGER           IPTR(*),KPTRD(*)

      SAVE
C     PRINT *,'FI8819-A',IPTR(20),IPTR(42)
C                       SET UP POINTERS
      DO 1000 I = 1, IPTR(42)
          IPTR(20) = IPTR(20) + 1
          DO 500 J = 1, 20
               ITBLD(J,IPTR(20)) = ITBLD2(J,I)
               MPTRD = MOD(ITBLD(J,IPTR(20)),16384)
               KPTRD(MPTRD) = IPTR(20)
  500     CONTINUE
 1000 CONTINUE
C  =======================================================
      IPTR(42) = 0
C     PRINT *,'MERGED TABLE D -- FI8819-B',IPTR(20),IPTR(42)
C     DO 6000 I = 1, IPTR(20)
C         WRITE (6,6001)I,(ITBLD(J,I),J=1,20)
C6001     FORMAT(15(1X,I5))
C6000 CONTINUE
      RETURN
      END
      SUBROUTINE FI8820 (ITBLD,IUNITD,IPTR,ITBLD2,KPTRD)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FI8820      READ IN BUFR TABLE D
C   PRGMMR: CAVANAUGH        ORG: W/NMC42    DATE: 93-05-06
C
C ABSTRACT: READ IN BUFR TABLE D
C
C PROGRAM HISTORY LOG:
C   93-05-06  CAVANAUGH
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C
C USAGE:    CALL FI8820 (ITBLD,IUNITD,IPTR,ITBLD2,KPTRD)
C   INPUT ARGUMENT LIST:
C     IUNITD   - UNIT NUMBER FOR TABLE D INPUT
C     IPTR     - ARRAY OF WORKING VALUES
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     ITBLD    - ARRAY TO CONTAIN TABLE D
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  NAS
C
C$$$
C  ..................................................
C
C                       ANCILLARY TABLE D FROM TYPE 11 BUFR MESSAGE
C
      INTEGER        ITBLD2(20,*)
C  ..................................................
C
C                       NEW BASE TABLE D
C
      INTEGER        ITBLD(20,*)
C  ..................................................
C
      INTEGER       IHOLD(33),IPTR(*),KPTRD(*)
      LOGICAL       MORE

      SAVE
C
      MORE  = .TRUE.
      I  = 0
C
C                                  READ IN TABLE D, BUT JUST ONCE
C     PRINT *,'TABLE D SWITCH=',IPTR(20),' ANCILLARY D SW=',IPTR(42)
      IF (IPTR(20).EQ.0) THEN
          DO 1000 MM = 1, 16384
              KPTRD(MM) = -1
 1000     CONTINUE
          IERR  = 0
          PRINT *,'FI8820 - READING TABLE D'
          KEY = 0
  100     CONTINUE
C                                 READ NEXT TABLE D ENTRY
          READ(IUNITD,15,ERR=9998,END=9000)(IHOLD(M),M=1,33)
   15     FORMAT(11(I1,I2,I3,1X),3X)
C                                  BUILD KEY FROM MASTER D ENTRY
C                             INSERT NEW MASTER INTO TABLE B
          I  = I + 1
          IPTR(20)  = IPTR(20) + 1
          DO 25 JJ = 1, 41, 3
              KK = (JJ/3) + 1
              IF (JJ.LE.31) THEN
                  ITBLD(KK,I) = IHOLD(JJ)*16384 +
     *                                 IHOLD(JJ+1)*256 + IHOLD(JJ+2)
                   IF (ITBLD(KK,I).LT.1.OR.ITBLD(KK,I).GT.65535) THEN
                       ITBLD(KK,I) = 0
                       GO TO 25
                   END IF
              ELSE
                  ITBLD(KK,I)  = 0
              END IF
   25     CONTINUE
          MPTRD = MOD(ITBLD(1,I),16384)
          KPTRD(MPTRD) = I
   50     CONTINUE
C         WRITE (6,51)I,(ITBLD(L,I),L=1,15)
   51     FORMAT (7H TABLED,16(1X,I5))
          GO TO 100
      ELSE
C         PRINT *,'TABLE D IS IN PLACE'
      END IF
      GO TO 9999
 9000 CONTINUE
      CLOSE(UNIT=IUNITD,STATUS='KEEP')
      GO TO 9999
 9998 CONTINUE
      IPTR(1)  = 8
C
 9999 CONTINUE
C     PRINT *,'THERE ARE',IPTR(20),' ENTRIES IN TABLE D'
      RETURN
      END
