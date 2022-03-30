C> @file
C> @brief Generate bufr message
C> @author Bill Cavanaugh @date 1993-09-29

C> Using information available in supplied arrays, generate
C> a bufr message (wmo code fm94).  there may be  a section 2
C> included in the bufr message if the user follows proper procedure.
C> messages are constructed in accordance with bufr edition 2. entries
C> for section 1 must be passed to this routine in the isect1 array.
C> entries for section 3 must be passed to this routine in isect3.
C>
C>
C> In the event that the user requests a reduction of reports
C> in a bufr message if a particular message becomes oversized, the
C> possibility exists of the last block of data producing an oversized
C> message. the user must verify that isect3(6) does in fact equal
C> zero to assure that all of the data has been included as output.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-09-29
C> - J. Hoppa 1994-03-22 Corrected an error when writing the
C> descriptors into the bufr message
C> - J. Hoppa 1994-03-31 Added the subset number to the parameter list
C> of subroutine fi8501()
C> - J. Hoppa 1994-04-15 Added kbufr to the parameter list of
C> subroutine fi8502()
C> - J. Hoppa 1994-04-20 Added the kdata parameter counter to the
C> parameter list of subroutine fi8501()
C> - J. Hoppa 1995-04-29 Changed nq and n to kary(2) changed jk to kary(11)
C> added an assignment to kary(2) so have something to pass to subroutines
C> deleted jk and ll from call to fi8501()
C>
C> @param[in] ISTEP Key for selection of processing step
C> - 1  = Process integer/text array into kdata.
C> - 2  = Process real/text array into kdata.
C> - 3  = Construct bufr message.
C> @param[in] IUNITB Unit number of device containing table b
C> @param[in] IUNITD Unit number of device containing table d
C> @param[in] IBFSIZ Size in bytes of bufr message array (kbufr)
C> should be a multiple of word size.
C> @param[in] ISECT1 Contains information to enter into section 1
C> (1) Edition number
C> (2) Bufr master table number
C>         0 = meteorological
C>         others not yet defined
C> (3) Originating center - subcenter number
C> (4) Originating center number
C> (5) Update sequence number
C> (6) Optional section flag should be set to zero unless user write
C> additional code to enter local information into section 3
C> (7) Bufr message type
C> (8) Bufr message sub_type
C> (9) Master table version number
C> (10) Local table version number
C> (11) Year of century - representative of data
C> (12) Month - representative of data
C> (13) Day - representative of data
C> (14) Hour - representative of data
C> (15) Minute - representative of data
C> (16)-(20) Unused
C> @param[in] ISECT3 Values to be inserted into section 3, and to control
C> report reduction for oversized messages
C> - (1) Number of subsets
C> Defines the number of subsets being passed to the encoder routine for
C> inclusion into a bufr message. If the user has specified the use of the
C> subset/report reduction activation switch, then a part of those subsets may
C> be used for the current message and the remainder retained for a subsequent
C> message.
C> - (2) Observed flag
C>  - 0 = observed data
C>  - 1 = other data
C> - (3) Compressed flag
C>  - 0 = noncompressed
C>  - 1 = compressed
C> - (4) Subset/report reduction activation switch used to control the number
C> of reports entered into a bufr message when maximum message size is exceeded
C>  - 0 = option not active
C>  - 1 = option is active. unused subsets will be shifted to low order
C> positions of entry array.
C>  - 2 = option is active. unused subsets will remain in entry positions.
C> @note If this flag is set to any other values, program will be terminated
C> with an error condition.
C> - (5) Number of reports to decrement by, if oversized message
C> (minimum value = one). If zero is entered, it will
C> be replaced by one.
C> - (6) Number of unused reports returned to user
C> - (7) Number of reports included in message
C> - (8) Number of table b entries available to decoder
C> - (9) Number of table d entries available to decoder
C> - (10) Text input flag
C>  - 0  = ASCII input
C>  - 1  = EBCIDIC input
C> @param[in] JIF JDESC input format flag
C> - 0  = F X Y
C> - 1  = Decimal format
C> @param[in] JDESC List of descriptors to go into section 3
C> Each descriptor = F * 16384 + X * 256 + Y
C> They may or may not be an exact match of the working descriptor list in kdesc.
C> This set of descriptors may contain sequence descriptors to provide additional
C> compression within the bufr message. There may be as few as one sequence
C> descriptor, or as many descriptors as there are in kdesc.
C> @param[in] NEWNR NR of descriptors in JDESC
C> @param[in] IDATA Integer array dimensioned by the number of descriptors to
C> be used
C> @param[in] RDATA Real array dimensioned by the number of descriptors to be
C> used
C> @param[in] ATEXT Array containing all text data associated with a specific
C> report. All data identified as text data must be in ASCII.
C> @param[in] KASSOC Integer array dimensioned by the number of descriptors
C> to be used, containing the associated field values for any entry in the
C> descriptor list.
C> @param[in] KIF KDESC input format flag
C> - 0  = F X Y
C> - 1  = DECIMAL FORMAT
C> @param[in] KDESC List of descriptors to go into section 3 fully expanded set of working
C> descriptors. there should be an element descriptor for every data entry, but
C> there should be no sequence descriptors.
C> @param[in] NRDESC NR of descriptors in kdesc
C> @param[in] ISEC2D Data or text to be entered into section 2
C> @param[in] ISEC2B Number of bytes of data in isec2d
C> @param[out] KDATA Source data array . a 2-dimension integer array where
C> kdata(subset,param) subset = subset number param  = parameter number.
C> @param[out] KARY Working array for message under construction
C> - (1) unused
C> - (2) parameter pointer
C> - (3) message bit pointer
C> - (4) delayed replication flag
C>  - 0 = no delayed replication
C>  - 1 = contains delayed replication
C> - (5) bit pointer for start of section 4
C> - (6) unused
C> - (7) nr of bits for parameter/data packing
C> - (8) total bits for ascii data
C> - (9) scale change value
C> - (10) indicator (used in w3fi85)
C>  - 1 = numeric data
C>  - 2 = text data
C> - (11) pointer to current pos in kdesc
C> - (12) unused
C> - (13) unused
C> - (14) unused
C> - (15) data type
C> - (16) unused
C> - (17) unused
C> - (18) words added for text or associated fields
C> - (19) location for total byte count
C> - (20) size of section 0
C> - (21) size of section 1
C> - (22) size of section 2
C> - (23) size of section 3
C> - (24) size of section 4
C> - (25) size of section 5
C> - (26) nr bits added by table c operator
C> - (27) bit width of associated field
C> - (28) jdesc input form flag
C>  - 0 = Descriptor in f x y form
C>   - F in JDESC(1,I)
C>   - X in JDESC(2,I)
C>   - Y in JDESC(3,I)
C>  - 1 = DEscriptor in decimal form in jdesc(1,i)
C> - (29) kdesc input form flag
C>  - 0 = Descriptor in F X Y form
C>   - F in KDESC(1,I)
C>   - X in KDESC(2,I)
C>   - Y in KDESC(3,I)
C>  - 1 = Descriptor in decimal form in kdesc(1,i)
C> - (30) bufr message total byte count
C> @param[out] KBUFR Array to contain completed bufr message
C> @param[out] IERRTN Error return flag
C>
C> IERRTN:
C> - = 0 Normal return, bufr message resides in kbufr
C>  - if isect3(4)= 0, all reports have been processed into a bufr message
C>  - if isect3(4)= 1, a bufr message has been generated with all or part of
C> the data passed to this routine. isect3(6) contains the number of reports
C> that were not used but are being held for the next message.
C> - = 1 bufr message construction was halted because contents exceeded maximum size
C> (only when isect3(4) = 0)
C> - = 2 bufr message construction was halted because of encounter with a
C> descriptor not found in table b.
C> - = 3 routine was called with no subsets
C> - = 4 error occured while reading table b
C> - = 5 an attempt was made to expand jdesc into kdesc, but a descriptor indicating
C> delayed replication was encountered
C> - = 6 error occured while reading table d
C> - = 7 data value could not be contained in specified bit width
C> - = 8 delayed replication not permitted in compressed data format
C> - = 9 an operator descriptor 2 04 yyy opening an associated field (yyy not eq zero)
C> was not followed by the defining descriptor 0 31 021 (7957 decimal).
C> - = 10 delayed replication descriptor was not followed by descriptor for delayed
C> replication factor.
C>  - 0 31 001
C>  - 0 31 002
C>  - 0 31 011
C>  - 0 31 012
C> - = 11 encountered a reference value that forced a data element to become negative
C> - = 12 no matching table d entry for sequence descriptor.
C> - = 13 encountered a non-acceptable data entry flag. isect3(6) should be 0 or 1.
C> - = 14 converting descriptors fxy->decimal, number to convert = 0
C> - = 15 no descriptors specified for section 3
C> - = 16 incomplete table b, number of descriptors in table b does not match number of
C> descriptors needed to construct bufr message
C> - = 20 incorrect entry of replication or sequence descriptor in list of reference
C> value changes
C> - = 21 incorrect operator descriptor in list of reference value changes
C> - = 22 attempting to enter new reference value into table b, but descriptor
C> does not exist in current modified table b
C>
C> @author Bill Cavanaugh @date 1993-09-29
      SUBROUTINE W3FI85(ISTEP,IUNITB,IUNITD,IBFSIZ,ISECT1,ISECT3,
     *    JIF,JDESC,NEWNR,IDATA,RDATA,ATEXT,KASSOC,
     *    KIF,KDESC,NRDESC,ISEC2D,ISEC2B,
     *    KDATA,KARY,KBUFR,IERRTN)
C
      REAL           RDATA(*)
C
      INTEGER        IDATA(*),LOWEST,MAXVAL,JSTART
      INTEGER        KARY(*),MISG,LL
      INTEGER        KDESC(3,*),KASSOC(*)
      INTEGER        IBITS(32)
      INTEGER        ZEROS(255)
      INTEGER        INDEXB(16383)
      CHARACTER*9    CCITT
      CHARACTER*4    AHOLD(2)
      CHARACTER*1    ATEXT(*)
      LOGICAL*1      TEXT
      LOGICAL*1      MSGFLG,DUPFLG
C  =====================================
C      INFORMATION REQUIRED FOR CONSTRUCTION OF BUFR MESSAGE
      INTEGER        ISECT1(*)
      INTEGER        ISEC2B,ISEC2D(255)
      INTEGER        ISECT3(*)
      INTEGER        JDESC(3,*)
      INTEGER        NEWNR
      INTEGER        KDATA(500,*)
      INTEGER        KBUFR(*)
C  =====================================
C                   TABLE B INFORMATION
      INTEGER        LDESC(800),KT(800)
      INTEGER        KSCALE(800)
      INTEGER        KRFVAL(800),KRFVSW(800),NEWRFV(800)
      INTEGER        KWIDTH(800)
      CHARACTER*40   ANAME(800)
      CHARACTER*25   AUNITS(800)
C  =====================================
C                   TABLE D INFORMATION
      INTEGER        KSEQ(300),KNUM(300)
      INTEGER        KLIST(300,10)
C  =====================================
      SAVE
C
      DATA  CCITT /'CCITT IA5'/
      DATA  IBITS /         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
      DATA  LL    /0/
      DATA  MISG  /99999/
      DATA  ZEROS /255*0/
C  =====================================
C                               THERE MUST BE DESCRIPTORS IN JDESC
C                               AND A COUNT IN NEWNR
C  =====================================
      IF (NEWNR.EQ.0) THEN
          IERRTN  = 15
          RETURN
      END IF
C  =====================================
C     IF INPUT FORM IS F X Y SEGMENTS THEN
C                      CONVERT INPUT FORM OF JDESC FROM FXY TO DECIMAL
C  =====================================
      IF (JIF.EQ.0) THEN
C                            CONVERT TO DECIMAL
          CALL FI8505(JIF,JDESC,NEWNR,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
      END IF
C  =====================================
C     IF PROCESSING DELAYED REPLICATION, MUST RELOAD
C            KDESC FROM JDESC
C  =====================================
      IF (KARY(4).NE.0) THEN
          NRDESC  = 0
      END IF
C  =====================================
C     IF ONLY HAVE JDESC, NEWNR CREATE KDESC, NRDESC
C  =====================================
C                      IF ONLY HAVE JDESC, NEWNR CREATE KDESC, NRDESC
      IF (NRDESC.EQ.0) THEN
          DO 50 I = 1, NEWNR
              KDESC(1,I)  = JDESC(1,I)
   50     CONTINUE
          NRDESC  = NEWNR
          KIF     = 1
      ELSE IF (NRDESC.NE.0) THEN
C                      KDESC ALL READY EXISTS
          IF (KIF.EQ.0) THEN
C                      CONVERT INPUT FORM OF KDESC FROM FXY TO DECIMAL
              CALL FI8505(KIF,KDESC,NRDESC,IERRTN)
              IF (IERRTN.NE.0) THEN
                  RETURN
              END IF
          END IF
      END IF
C  =====================================
C     READ IN TABLE B SUBSET, IF NOT ALL READY IN PLACE
C  =====================================
      IF (ISECT3(8).EQ.0) THEN
          CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
          IF (IERRTN.NE.0) GO TO 9000
      END IF
C  =====================================
C     ROUTE TO SELECTED PROCESSING
C  =====================================
      KSUB  = ISECT3(1)
      IF (ISTEP.EQ.1) THEN
C                          PROCESSING INTEGER DATA INPUT
          CALL FI8508(ISTEP,IUNITB,IDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
          RETURN
      ELSE IF (ISTEP.EQ.2) THEN
C                          PROCESSING REAL DATA INPUT
          CALL FI8509(ISTEP,IUNITB,RDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)
          RETURN
      ELSE IF (ISTEP.NE.3) THEN
          IERRTN  = 20
          RETURN
      END IF
C  =====================================
C     IF INDICATING ZERO SUBSETS, HAVE AN ERROR CONDITION
C  =====================================
      IF (ISECT3(1).LE.0) THEN
          IERRTN  = 3
          RETURN
      END IF
C  =====================================
C     SET FOR BUFR MESSAGE
C  =====================================
C
C                                 CLEAR OUTPUT AREA
C                      BYTES IN EACH FULL WORD
      KWORD  = 4
C
C                               GET NUMBER OF SUBSETS
C
      MXRPTS    = ISECT3(1)
      ISECT3(7) = ISECT3(1)
      ISECT3(6) = ISECT3(1)
C
C                       RE-START POINT FOR PACKING FEWER SUBSETS ?
C
    5 CONTINUE
C
      KARY(18) = 0
      KARY(26) = 0
C  =====================================
C     ENTER 'BUFR'          -  SECTION 0
C                      CONSTRUCT UNDER RULES OF EDITION 2
C  =====================================
      KARY(3)        = 0
      NBUFR          = 1112884818
      CALL SBYTE (KBUFR,NBUFR,KARY(3),32)
      KARY(3)        = KARY(3) + 32
C                             SAVE POINTER FOR TOTAL BYTE COUNT
C                                   IN MESSAGE
      KARY(19)       = KARY(3)
      KARY(3)        = KARY(3) + 24
C                    SET EDITION NR IN PLACE
      CALL SBYTE (KBUFR,2,KARY(3),8)
      KARY(3)        = KARY(3) + 8
      KARY(20)       = 8
C     PRINT *,'SECTION 0'
C  =====================================
C     COMPLETE ENTRIES FOR  -  SECTION 1
C  =====================================
C  ----- 1,3              SECTION COUNT
      KARY(21)       = 18
      CALL SBYTE (KBUFR,KARY(21),KARY(3),24)
      KARY(3)        = KARY(3) + 24
C  ----- 4                  RESERVED
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 5               ORIGINATING SUB-CENTER
      CALL SBYTE (KBUFR,ISECT1(3),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 6               ORIGINATING CENTER
      CALL SBYTE (KBUFR,ISECT1(4),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 7               UPDATE SEQUENCE NUMBER
      CALL SBYTE (KBUFR,ISECT1(5),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 8
C                        INDICATE NO SECTION 2
      CALL SBYTE (KBUFR,ISECT1(6),KARY(3),1)
      KARY(3)        = KARY(3) + 1
      CALL SBYTE (KBUFR,0,KARY(3),7)
      KARY(3)        = KARY(3) + 7
C  ----- 9            BUFR MESSAGE TYPE
      CALL SBYTE (KBUFR,ISECT1(7),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 10            BUFR MESSAGE SUB-TYPE
      CALL SBYTE (KBUFR,ISECT1(8),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 11            VERSION OF MASTER TABLE
      CALL SBYTE (KBUFR,ISECT1(9),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 12            VERSION OF LOCAL TABLE
      CALL SBYTE (KBUFR,ISECT1(10),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 13            YEAR
      CALL SBYTE (KBUFR,ISECT1(11),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 14            MONTH
      CALL SBYTE (KBUFR,ISECT1(12),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ---- 15             DAY
      CALL SBYTE (KBUFR,ISECT1(13),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 16            HOUR
      CALL SBYTE (KBUFR,ISECT1(14),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 17            MINUTE
      CALL SBYTE (KBUFR,ISECT1(15),KARY(3),8)
      KARY(3)        = KARY(3) + 8
C  ----- 18            FILL
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C     PRINT *,'SECTION 1'
C  =====================================
C     SKIP                  -  SECTION 2
C  =====================================
      IF (ISECT1(6).NE.0) THEN
C                  BUILD SECTION COUNT
          KARY(22) = 4 + ISEC2B
          IF (MOD(KARY(22),2).NE.0) KARY(22) = KARY(22) + 1
C                        INSERT SECTION COUNT
          CALL SBYTE (KBUFR,KARY(22),KARY(3),24)
          KARY(3)  = KARY(3) + 24
C                        INSERT RESERVED POSITION
          CALL SBYTE (KBUFR,0,KARY(3),8)
          KARY(3)  = KARY(3) + 8
C                        INSERT SECTION 2 DATA
          CALL SBYTES(KBUFR,ISEC2D,KARY(3),8,0,ISEC2B)
          KARY(3)  = KARY(3) + (ISEC2B * 8)
          IF (MOD(ISEC2B,2).NE.0) THEN
              CALL SBYTE (KBUFR,0,KARY(3),8)
              KARY(3)  = KARY(3) + 8
          END IF
      ELSE
          KARY(22)       = 0
      END IF
C  =====================================
C     MAKE PREPARATIONS FOR SECTION 3 DESCRIPTORS
C  =====================================
      KARY(23)        = 7 + NEWNR*2 + 1
C                             SECTION 3 SIZE
      CALL SBYTE (KBUFR,KARY(23),KARY(3),24)
      KARY(3)         = KARY(3) + 24
C                             RESERVED BYTE
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)         = KARY(3) + 8
C                           NUMBER OF SUBSETS
      CALL SBYTE (KBUFR,ISECT3(1),KARY(3),16)
      KARY(3)         = KARY(3) + 16
C                          SET OBSERVED DATA SWITCH
      CALL SBYTE (KBUFR,ISECT3(2),KARY(3),1)
      KARY(3)         = KARY(3) + 1
C                          SET COMPRESSED DATA SWITCH
      CALL SBYTE (KBUFR,ISECT3(3),KARY(3),1)
      KARY(3)         = KARY(3) + 1
      CALL SBYTE (KBUFR,0,KARY(3),6)
      KARY(3)         = KARY(3) + 6
C  =====================================
C     DESCRIPTORS         -  SECTION 3
C  =====================================
      DO 37 KH = 1, NEWNR
C         PRINT *,'INSERTING',JDESC(1,KH),' INTO SECTION 3'
          CALL SBYTE (KBUFR,JDESC(1,KH),KARY(3),16)
          KARY(3)         = KARY(3) + 16
   37 CONTINUE
C                          FILL TO TWO BYTE BOUNDARY
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C     PRINT *,'SECTION 3'
C  =====================================
C     INITIALIZE FOR        -  SECTION 4
C  =====================================
C                              SAVE POINTER TO COUNT POSITION
C     PRINT *,'START OF SECTION 4',KARY(3)
      KARY(5)        = KARY(3)
      KARY(3)        = KARY(3) + 24
      CALL SBYTE (KBUFR,0,KARY(3),8)
      KARY(3)        = KARY(3) + 8
C                              SKIP TO FIRST DATA POSITION
C  =====================================
C     BIT PATTERNS          -  SECTION 4
C  =====================================
      KEND4  = IBFSIZ * 8 - 32
C                          PACK ALL DATA INTO BUFR MESSAGE
C
      IF (ISECT3(3).EQ.0) THEN
C                        **********************************************
C                        *                                            *
C                        *      PROCESS AS NON-COMPRESSED MESSAGE     *
C                        *                                            *
C                        **********************************************
          CALL FI8506(ISTEP,ISECT3,KARY,JDESC,NEWNR,KDESC,NRDESC,
     *           LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,NEWRFV,
     *           KSEQ,KNUM,KLIST,IBFSIZ,
     *           KDATA,KBUFR,IERRTN,INDEXB)
          IF (IERRTN.NE.0) THEN
              IF (IERRTN.EQ.1) GO TO 5500
              RETURN
          END IF
      ELSE
C                        **********************************************
C                        *                                            *
C                        *      PROCESS AS COMPRESSED MESSAGE         *
C                        *                                            *
C                        **********************************************
          KARY(18)  = 0
C                          MUST LOOK AT EVERY DESCRIPTOR IN KDESC
          KARY(11)  = 1
 3000     CONTINUE
          IF (KARY(11).GT.NRDESC) THEN
              GO TO 5200
          ELSE
C         DO 5000 JK = 1, NRDESC
C                              RE-ENTRY POINT FOR INSERTION OF
C                              REPLICATION OR SEQUENCES
 4000         CONTINUE
C                             ISOLATE TABLE
              KFUNC      = KDESC(1,KARY(11)) / 16384
C                             ISOLATE CLASS
              KCLASS     = MOD(KDESC(1,KARY(11)),16384) / 256
              KSEG       = MOD(KDESC(1,KARY(11)),256)
              KARY(2) = KARY(11) + KARY(18)
              IF (KFUNC.EQ.1) THEN
C                            DELAYED REPLICATION NOT ALLOWED
C                            IN COMPRESSED MESSAGE
                  IF (KSEG.EQ.0) THEN
                      IERRTN  = 8
                      RETURN
                  END IF
C                            REPLICATION DESCRIPTOR
                  CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,LL,KDESC,NRDESC,IERRTN)
C                 GO TO 4000
              ELSE IF (KFUNC.EQ.2) THEN
                  CALL FI8502(*4000,KBUFR,KCLASS,KSEG,
     *                     KDESC,NRDESC,I,ISTEP,
     *            KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 5000
              ELSE IF (KFUNC.EQ.3) THEN
                  CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 4000
              END IF
C                      FALL THRU WITH ELEMENT DESCRIPTOR
C                      POINT TO CORRECT TABLE B ENTRY
              L  = INDEXB(KDESC(1,KARY(11)))
              IF (L.LT.0) THEN
                  IERRTN  = 2
C                 PRINT *,'W3FI85 - IERRTN = 2'
                  RETURN
              END IF
C
              IF (AUNITS(L)(1:9).EQ.CCITT) THEN
                  TEXT  = .TRUE.
              ELSE
                  TEXT  = .FALSE.
              END IF
              KARY(7) = KWIDTH(L)
C
              IF (TEXT) THEN
C                                     PROCESS TEXT DATA
                  KBZ = KARY(3) + (ISECT3(1) + 1) * KARY(7) + 6
                  IF (KBZ.GT.KEND4) THEN
                      GO TO 5500
                  END IF
C                             NBINC IS NUMBER OF CHARS
                  NBINC     = KARY(7) / 8
C                              LOWEST = 0
                  CALL SBYTES(KBUFR,ZEROS,KARY(3),8,0,NBINC)
                  KARY(3)   = KARY(3) + KARY(7)
                  CALL SBYTE (KBUFR,NBINC,KARY(3),6)
                  KARY(3)   = KARY(3) + 6
C                               HOW MANY FULL WORDS
                  NKPASS    = KARY(7) / 32
C                           HOW MANY BYTES IN PARTIAL WORD
                  KREM      = MOD(KARY(7),32)
C                 KSKIP     = KARY(7) - 32
                  DO 4080 NSS = 1, ISECT3(1)
C                            POINT TO TEXT FOR THIS SUBSET
                      KARY(2)   = KARY(11) + KARY(18)
                      IF (NKPASS.GE.1) THEN
C                          PROCESS TEXT IN A SUBSET
                          DO 4070 NPP = 1, NKPASS
C                                 PROCESS FULL WORDS
                              IF (ISECT3(10).EQ.1) THEN
                                  CALL W3AI38 (KDATA(NSS,KARY(2)),4)
                              END IF
                              CALL SBYTE (KBUFR,KDATA(NSS,KARY(2)),
     *                               KARY(3),32)
                              KARY(3)  = KARY(3) + 32
C                                 POINT TO NEXT DATA WORD FOR MORE TEXT
                              KARY(2) = KARY(2) + 1
 4070                     CONTINUE
                      END IF
C                          PROCESS PARTIALS - LESS THAN 4 BYTES
                      IF (KREM.GT.0) THEN
                          IF (ISECT3(10).EQ.1) THEN
                              CALL W3AI38 (KDATA(NSS,KARY(2)),4)
                          END IF
                          CALL SBYTE (KBUFR,KDATA(NSS,KARY(2)),
     *                               KARY(3),KREM)
                          KARY(3)  = KARY(3) + KREM
                      END IF
 4080             CONTINUE
C                              ADJUST EXTRA WORD COUNT
                  IF (KREM.GT.0) THEN
                      KARY(18)  = KARY(18) + NKPASS
                  ELSE
                      KARY(18)  = KARY(18) + NKPASS - 1
                  END IF
C  -------------------------------------------------------------
                  GO TO 5000
              ELSE
                  KARY(2) = KARY(11) + KARY(18)
                  KARY(7) = KWIDTH(L) + KARY(26)
C
C                               NON TEXT/NUMERIC DATA
C
C                             PROCESS ASSOCIATED FIELD DATA
                  IF (KARY(27).GT.0.AND.KDESC(1,KARY(11)).NE.7957) THEN
                      DUPFLG  = .TRUE.
                      DO 4130 J = 2, ISECT3(1)
                          IF (KDATA(J,KARY(2)).NE.KDATA(1,KARY(2)))THEN
                              DUPFLG = .FALSE.
                              GO TO 4131
                          END IF
 4130                 CONTINUE
 4131                 CONTINUE
                      IF (DUPFLG) THEN
C                                  ALL VALUES ARE EQUAL
                          KBZ = KARY(3) + KARY(7) + 6
                          IF (KBZ.GT.KEND4) THEN
                              GO TO 5500
                          END IF
                          NBINC  = 0
C                                 ENTER COMMON VALUE
                          IF (KDATA(1,KARY(2)).EQ.MISG) THEN
                              CALL SBYTE(KBUFR,IBITS(KARY(7)),
     *                               KARY(3),KARY(27))
                          ELSE
                              CALL SBYTE(KBUFR,KDATA(1,KARY(2)),
     *                               KARY(3),KARY(27))
                          END IF
                          KARY(3)  = KARY(3) + KARY(27)
C                                       ENTER NBINC
                          CALL SBYTE (KBUFR,NBINC,KARY(3),6)
                          KARY(3)  = KARY(3) + 6
                      ELSE
C                               MIX OF MISSING AND VALUES
C                               GET LARGEST DIFFERENCE VALUE
                          MSGFLG = .FALSE.
                          DO 4132 J = 1, ISECT3(7)
                              IF (KDATA(J,KARY(2)).EQ.MISG) THEN
                                  MSGFLG = .TRUE.
                                  GO TO 4133
                              END IF
 4132                     CONTINUE
 4133                     CONTINUE
                          DO 4134 J = 1, ISECT3(7)
                              IF (KDATA(J,KARY(2)).LT.IBITS(KARY(27))
     *                                .AND.KDATA(J,KARY(2)).GE.0.AND.
     *                                KDATA(J,KARY(2)).NE.MISG) THEN
                                  LOWEST = KDATA(J,KARY(2))
                                  MAXVAL = KDATA(J,KARY(2))
                                  JSTART = J + 1
                                  GO TO 4135
                              END IF
 4134                     CONTINUE
 4135                     CONTINUE
                          DO 4136 J = JSTART, ISECT3(7)
                              IF (KDATA(J,KARY(2)).NE.MISG) THEN
                                 IF (KDATA(J,KARY(2)).LT.LOWEST) THEN
                                          LOWEST = KDATA(J,KARY(2))
                                 ELSE IF(KDATA(J,KARY(2)).GT.MAXVAL)THEN
                                          MAXVAL = KDATA(J,KARY(2))
                                 END IF
                              END IF
 4136                     CONTINUE
                          MXDIFF  = MAXVAL - LOWEST
C                              FIND NBINC
                          MXBITS  = KARY(27)
                          DO 4142 LJ = 1, MXBITS
                              NBINC = LJ
                              IF (MXDIFF.LT.IBITS(LJ)) THEN
                                  GO TO 4143
                              END IF
 4142                     CONTINUE
 4143                     CONTINUE
                          KBZ = KARY(3) + MXBITS + 6 + ISECT3(1) * NBINC
                          IF (KBZ.GT.KEND4) THEN
                              GO TO 5500
                          END IF
                          IF (NBINC.GT.MXBITS) THEN
                              IERRTN  = 3
                              RETURN
                          END IF
C                               ENTER LOWEST
                          CALL SBYTE(KBUFR,LOWEST,KARY(3),MXBITS)
                          KARY(3)  = KARY(3) + MXBITS
                          CALL SBYTE(KBUFR,NBINC,KARY(3),6)
                          KARY(3)  = KARY(3) + 6
C                               GET DIFFERENCE VALUES
                          IF (MSGFLG) THEN
                              DO 4144 M = 1, ISECT3(1)
                                  IF (KDATA(M,KARY(2)).EQ.MISG) THEN
                                      KT(M)  = IBITS(NBINC)
                                  ELSE
                                      KT(M)  = KDATA(M,KARY(2)) - LOWEST
                                  END IF
 4144                         CONTINUE
                          ELSE
                              DO 4146 M = 1, ISECT3(1)
                                  KT(M)  = KDATA(M,KARY(2)) - LOWEST
 4146                         CONTINUE
                          END IF
C                                ENTER DATA VALUES
                          CALL SBYTES(KBUFR,KT,KARY(3),NBINC,
     *                                          0,ISECT3(1))
                          KARY(3)  = KARY(3) + ISECT3(1) * NBINC
                      END IF
                      KARY(18)  = KARY(18) + 1
                  END IF
C  ---------------------------------------------------
C                            STANDARD DATA
C  ---------------------------------------------------
                  KARY(2) = KARY(11) + KARY(18)
                  MXBITS = KARY(7) + KARY(26)
                  DUPFLG = .TRUE.
                  DO 4030 J = 2, ISECT3(7)
                      IF (KDATA(J,KARY(2)).NE.KDATA(1,KARY(2))) THEN
                          DUPFLG = .FALSE.
                          GO TO 4031
                      END IF
 4030             CONTINUE
 4031             CONTINUE
                  IF (DUPFLG) THEN
C                                  ALL VALUES ARE EQUAL
                      KBZ = KARY(3) + KARY(7) + 6
                      IF (KBZ.GT.KEND4) THEN
                          GO TO 5500
                      END IF
                      NBINC  = 0
C                                 ENTER COMMON VALUE
                      IF (KDATA(1,KARY(2)).EQ.MISG) THEN
                          CALL SBYTE(KBUFR,IBITS(MXBITS),
     *                               KARY(3),MXBITS)
                      ELSE
                          CALL SBYTE(KBUFR,KDATA(1,KARY(2)),
     *                               KARY(3),MXBITS)
                      END IF
                      KARY(3)  = KARY(3) + KARY(7)
C                                       ENTER NBINC
                      CALL SBYTE (KBUFR,NBINC,KARY(3),6)
                      KARY(3)  = KARY(3) + 6
                  ELSE
C                               MIX OF MISSING AND VALUES
C                               GET LARGEST DIFFERENCE VALUE
                      MSGFLG = .FALSE.
                      DO 4032 J = 1, ISECT3(7)
                          IF (KDATA(J,KARY(2)).EQ.MISG) THEN
                              MSGFLG = .TRUE.
                              GO TO 4033
                          END IF
 4032                 CONTINUE
 4033                 CONTINUE
                      DO 4034 J = 1, ISECT3(7)
                          IF (KDATA(J,KARY(2)).NE.MISG) THEN
                              LOWEST = KDATA(J,KARY(2))
                              MAXVAL = KDATA(J,KARY(2))
C                             PRINT *,' '
C                             PRINT *,'START VALUES',LOWEST,MAXVAL,
C    *                            'J=',J,' KARY(2)=',KARY(2)
                              GO TO 4035
                          END IF
 4034                 CONTINUE
 4035                 CONTINUE
                      DO 4036 J = 1, ISECT3(1)
                          IF (KDATA(J,KARY(2)).NE.MISG) THEN
                             IF (KDATA(J,KARY(2)).LT.LOWEST) THEN
                                          LOWEST = KDATA(J,KARY(2))
C                                PRINT *,'NEW LOWEST=',LOWEST,J
                             ELSE IF (KDATA(J,KARY(2)).GT.MAXVAL) THEN
                                      MAXVAL = KDATA(J,KARY(2))
C                                PRINT *,'NEW MAXVAL=',MAXVAL,J
                             END IF
                          END IF
 4036                 CONTINUE
                      MXDIFF  = MAXVAL - LOWEST
C                              FIND NBINC
                      DO 4042 LJ = 1, MXBITS
                          NBINC = LJ
                          IF (MXDIFF.LT.IBITS(LJ)) GO TO 4043
                          IF (NBINC.EQ.MXBITS) GO TO 4043
 4042                 CONTINUE
 4043                 CONTINUE
                      KBZ = KARY(3) + MXBITS  + 38 + ISECT3(1) * NBINC
                      IF (KBZ.GT.KEND4) THEN
                          GO TO 5500
                      END IF
C                     PRINT 4444,KARY(11),KDESC(1,KARY(11)),LOWEST,
C    *                 MAXVAL,MXDIFF,KARY(7),NBINC,ISECT3(1),ISECT3(7)
C4444                 FORMAT(9(1X,I8))
C                               ENTER LOWEST
C                                 ADJUST WITH REFERENCE VALUE
                      IF (KRFVSW(L).EQ.0) THEN
                          JRV  = KRFVAL(L)
                      ELSE
                          JRV  = NEWRFV(L)
                      END IF
                      LVAL  = LOWEST - JRV
                      CALL SBYTE(KBUFR,LVAL,KARY(3),MXBITS)
                          KARY(3)  = KARY(3) + MXBITS
                      IF (NBINC.GT.MXBITS) THEN
                          IERRTN  = 3
                          RETURN
                      END IF
                      CALL SBYTE(KBUFR,NBINC,KARY(3),6)
                      KARY(3)  = KARY(3) + 6
C                                GET DIFFERENCE VALUES
                      IF (MSGFLG) THEN
                          DO 4044 M = 1, ISECT3(1)
                              IF (KDATA(M,KARY(2)).EQ.MISG) THEN
                                  KT(M)  = IBITS(NBINC)
                              ELSE
                                  KT(M)  = KDATA(M,KARY(2)) - LOWEST
                              END IF
 4044                     CONTINUE
                      ELSE
                          DO 4046 M = 1, ISECT3(1)
                              KT(M)  = KDATA(M,KARY(2)) - LOWEST
 4046                     CONTINUE
                      END IF
C                                ENTER DATA VALUES
                      CALL SBYTES(KBUFR,KT,KARY(3),NBINC,
     *                                             0,ISECT3(1))
                      KARY(3)  = KARY(3) + ISECT3(1) * NBINC
                  END IF
                  GO TO 5000
              END IF
C  -------------------------------------------------------------
 5000         CONTINUE
              KARY(11) = KARY(11) + 1
              GO TO 3000
          ENDIF
 5200     CONTINUE
      END IF
      ISECT3(6) = 0
      GO TO 6000
 5500 CONTINUE
C                       THE SEGMENT OF CODE BETWEEN STATEMENTS
C                       5500-6000 ARE ACTIVATED IF AND WHEN THE
C                       MAXIMUM MESSAGE SIZE HAS BEEN EXCEEDED
C
C           ARE WE REDUCING IF OVERSIZED  ???
      IF (ISECT3(4).NE.0) THEN
C                                   INCREMENT REDUCTION COUNT
          ISECT3(6)  = ISECT3(6) + ISECT3(5)
C                                   REDUCE NUMBER TO INCLUDE
          ISECT3(7)  = ISECT3(1) - ISECT3(5)
          ISECT3(1)  = ISECT3(7)
          PRINT *,'REDUCED BY ',ISECT3(5),' ON THIS PASS'
          GO TO 5
      ELSE
          IERRTN = 1
          RETURN
      END IF
 6000 CONTINUE
C  ---------------------------------------------------------------
C                                 FILL IN SECTION 4 OCTET COUNT
      NBUFR  = MOD((KARY(3) - KARY(5)),16)
C                                 MAY BE NECESSARY TO ADJUST COUNT
      IF (NBUFR.NE.0) THEN
          KARY(3)   = KARY(3) + 16 - NBUFR
      END IF
      KARY(24)  = (KARY(3) - KARY(5)) / 8
      CALL SBYTE (KBUFR,KARY(24),KARY(5),24)
C     PRINT *,'SECTION 4'
C  =====================================
C     ENDING KEY  '7777'    -  SECTION 5
C  =====================================
      KARY(25)    = 4
      NBUFR       = 926365495
      CALL SBYTE (KBUFR,NBUFR,KARY(3),32)
      KARY(3)     = KARY(3) + 32
C                  CONSTRUCT TOTAL BYTE COUNT FOR SECTION 0
      ITOTAL      = KARY(3) / 8
      CALL SBYTE (KBUFR,ITOTAL,32,24)
      KARY(30)    = ITOTAL
C     WRITE (6,8601) ITOTAL
 8601 FORMAT (1X,22HTHIS MESSAGE CONTAINS ,I10,6H BYTES)
C  =======================================
C                 KBUFR CONTAINS A COMPLETED MESSAGE
      IF (ISECT3(4).NE.0.AND.ISECT3(5).NE.0) THEN
C                 ADJUST KDATA ARRAY
          NR   = MXRPTS - ISECT3(1)
          ISECT3(7) = ISECT3(7) + 1
          DO 7500 I = 1, NR
              DO 7000 J = 1, NRDESC
                  KDATA(I,J)  = KDATA(ISECT3(7),J)
 7000         CONTINUE
              ISECT3(7) = ISECT3(7) + 1
 7500     CONTINUE
          KARY(14)  = NR
      ELSE
          ISECT3(7) = ISECT3(1)
      END IF
C  =======================================
      IERRTN = 0
 9000 CONTINUE
      RETURN
      END
C> @brief Perform replication of descriptors
C> @author Bill Cavanaugh @date 1993-12-03

C> Have encountered a replication descriptor. It may include
C> delayed replication or not. That decision should have been
C> made prior to calling this routine.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C> - J. Hoppa 1994-03-25 Added line to initialize nxtptr to correct
C> an error in the standard replication.
C> - J. Hoppa 1994-03-28 Corrected an error in the standard replication
C> that was adding extra zeros to the bufr message after the replicated data.
C> - J. Hoppa 1994-03-31 Added the subset number to the parameter list.
C> corrected the equation for the number of replications with delayed replication.
C> (istart and k don't exist)
C> - J. Hoppa 1994-04-19 Switched the variables next and nxtprt
C> - J. Hoppa 1994-04-20 Added the kdata parameter counter to the parameter
C> list. In the assignment of nreps when have delayed replication, changed index
C> in kdata from n to k.
C> - J. Hoppa 1994-04-29 Removed n and k from the input list changed n to
C> kary(11) and k to kary(2)
C>
C> @param[in] ISTEP
C> @param[in] KCLASS
C> @param[in] KSEG
C> @param[in] IDATA
C> @param[in] RDATA
C> @param[in] KDATA
C> @param[in] NSUB Current subset
C> @param[inout] KDESC (modified [out]) List of descriptors
C> @param[inout] NRDESC Number of (new [out]) descriptors in kdesc
C> @param[out] IERRTN Error return value
C> @param KARY
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,NSUB,KDESC,NRDESC,IERRTN)

C
      REAL         RDATA(*)
C
      INTEGER      IDATA(*),NREPS,KARY(*)
      INTEGER      KCLASS,KSEG
      INTEGER      KDESC(3,*),NRDESC,KDATA(500,*)
      INTEGER      IERRTN
      INTEGER      ITAIL(1600)
      INTEGER      IHOLD(1600),ISTEP
C
      SAVE
C
C                    TEST KFUNC FOR DESCRIPTOR TYPE
C                    DO REPLICATION
C  ****************************************************************
      IERRTN  = 0
C                            REPLICATION DESCRIPTOR
C                                 STANDARD REPLICATION WILL SIMPLY
C                                 BE PROCESSED FROM ITS DESCRIPTOR
C                                 PARTS
C
C                            DELAYED REPLICATION DESCRIPTOR
C                                 MUST BE FOLLOWED BY ONE OF THE
C                                 DESCRIPTORS FOR A DELAYED
C                                 REPLICATION FACTOR
C                                       0 31 001  (7937 DECIMAL)
C                                       0 31 002  (7938 DECIMAL)
C                                       0 31 011  (7947 DECIMAL)
C                                       0 31 012  (7948 DECIMAL)
      IF (KSEG.NE.0) THEN
C                       HAVE NUMBER OF REPLICATIONS AS KSEG
          NREPS  = KSEG
          IPUT   = KARY(11)
          NEXT   = IPUT + 1
          NXTPTR = IPUT + 1 + KCLASS
      ELSE IF (KSEG.EQ.0) THEN
          IF (KDESC(1,KARY(11)+1).EQ.7937.OR.
     *                    KDESC(1,KARY(11)+1).EQ.7938.OR.
     *                    KDESC(1,KARY(11)+1).EQ.7947.OR.
     *                    KDESC(1,KARY(11)+1).EQ.7948) THEN
C             PRINT *,'HAVE DELAYED REPLICATION'
              KARY(4)  = 1
C                                  MOVE REPLICATION DEFINITION
              KDESC(1,KARY(11))  = KDESC(1,KARY(11)+1)
C                                  MUST DETERMINE HOW MANY REPLICATIONS
              IF (ISTEP.EQ.1) THEN
                  NREPS = IDATA(KARY(11))
              ELSE IF (ISTEP.EQ.2) THEN
                  NREPS = RDATA(KARY(11))
              ELSE
                  NREPS = KDATA(NSUB,KARY(2))
              END IF
              IPUT      = KARY(11) + 1
              NXTPTR    = IPUT + KCLASS + 1
              NEXT      = IPUT + 1
C                              POINT TO REPLICATION DESCRIPTOR
          END IF
      ELSE
          IERRTN  = 10
          RETURN
      END IF
C                                 EXTRACT DESCRIPTORS TO BE REPLICATED
C                     IF NREPS = 0, THIS LIST OF DESCRIPTORS IS NOT TO
C                        BE USED IN DEFINING THE DATA,
C                     OTHERWISE
C                        IT WILL BE USED TO DEFINE THE DATA
      IF (NREPS.NE.0) THEN
          DO 1000 IJ = 1, KCLASS
              IHOLD(IJ)  = KDESC(1,NEXT)
              NEXT  = NEXT + 1
 1000     CONTINUE
C                      SKIP THE NUMBER OF DESCRIPTORS DEFINED BY KCLASS
      END IF
C                                   SAVE OFF TAIL OF DESC STREAM
C               START AT FIRST POSITION OF TAIL
      IGOT    = 0
      DO 1100 IJ = NXTPTR, NRDESC
          IGOT         = IGOT + 1
          ITAIL(IGOT)  = KDESC(1,IJ)
 1100 CONTINUE
C                                   INSERT ALL REPLICATED DESC'S
      IF (NREPS.NE.0) THEN
          DO 1300 KR = 1, NREPS
              DO 1200 KD = 1, KCLASS
                  KDESC(1,IPUT) = IHOLD(KD)
                  IPUT          = IPUT + 1
 1200         CONTINUE
 1300     CONTINUE
      END IF
C                                   RESTORE TAIL
      DO 1400 ITL = 1, IGOT
          KDESC(1,IPUT) = ITAIL(ITL)
          IPUT          = IPUT + 1
 1400 CONTINUE
C
C                               RESET NUMBER OF DESCRIPTORS IN KDESC
      NRDESC  = IPUT - 1
C  ****************************************************************
      RETURN
      END
C> @brief Process an operator descriptor.
C> @author Bill Cavanaugh @date 193-12-03

C> Have encountered an operator descriptor.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C> - J. Hoppa 1994-04-15 Added kbufr to input parameter list.
C> added block of data to correctly use sbyte when writing a 205yyy descriptor to the
C> bufr message. The previous way didn't work because kdata was getting incremeted
C> by the ksub value, not the param value.
C> - J. Hoppa 1994-04-29 Changed k to kary(2) removed a line that became obsolete with
C> above change
C> - J. Hoppa 1994-05-18 Added a kary(2) increment
C>
C> @param[in] KCLASS
C> @param[in] KSEG
C> @param[inout] KDESC
C> @param[inout] NRDESC
C> @param[in] I
C> @param[in] ISTEP
C> @param[inout] KARY
C> @param[out] IERRTN Error return value
C> @param KBUFR
C> @param KDATA
C> @param ISECT3
C> @param KRFVSW
C> @param NEWRFV
C> @param LDESC
C> @param INDEXB
C>
C> @author Bill Cavanaugh @date 193-12-03
      SUBROUTINE FI8502(*,KBUFR,KCLASS,KSEG,KDESC,NRDESC,I,ISTEP,
     *          KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)

C
      INTEGER      KCLASS,KSEG,ZEROES(255)
      INTEGER      KRFVSW(*),NEWRFV(*),LDESC(*)
      INTEGER      I,KDESC(3,*),KDATA(500,*),ISECT3(*)
      INTEGER      NRDESC
      INTEGER      KARY(*)
      INTEGER      IERRTN
      INTEGER      NLEFT
C
      SAVE
C
      DATA  ZEROES/255*0/
C
C  ****************************************************************
      IERRTN  = 0
C                            OPERATOR DESCRIPTOR
      IF (KCLASS.EQ.1) THEN
C                  BITS ADDED TO DESCRIPTOR WIDTH
          IF (ISTEP.EQ.3) THEN
              IF (KSEG.NE.0) THEN
                  KARY(26)  = KSEG - 128
              ELSE
                  KARY(26)  = 0
              END IF
          END IF
      ELSE IF (KCLASS.EQ.2) THEN
C                  NEW SCALE VALUE
          IF (ISTEP.EQ.3) THEN
              IF (KSEG.EQ.0) THEN
                  KARY(9)  = 0
              ELSE
                  KARY(9)  = KSEG - 128
              END IF
          END IF
      ELSE IF (KCLASS.EQ.3) THEN
C                  CHANGE REFERENCE VALUE
C                                  MUST ACCEPT INTO OUTPUT THE
C                                  REFERENCE VALUE CHANGE AND ACTIVATE
C                                  THE CHANGE WHILE PROCESSING
          IF (ISTEP.EQ.3) THEN
C                       HAVE OPERATOR DESCRIPTOR FOR REFERENCE VALUES
              IF (KSEG.EQ.0) THEN
                  DO 100 IQ = 1, ISECT3(8)
C                                RESET ALL NEW REFERENCE VALUES
                      KRFVSW(IQ)  = 0
  100             CONTINUE
              END IF
  200         CONTINUE
C                                GET NEXT DESCRIPTOR
              KARY(11)  = KARY(11) + 1
              IF (KDESC(1,KARY(11)).GT.16383) THEN
C                                NOT AN ELEMENT DESCRIPTOR
                  NFUNC  = KDESC(1,KARY(11)) / 16384
                  IF (NFUNC.EQ.1.OR.NFUNC.EQ.3) THEN
                      IERRTN  = 20
                      PRINT *,'INCORRECT ENTRY OF REPLICATION OR ',
     *                     'SEQUENCE DESCRIPTOR IN LIST OF ',
     *                     'REFERENCE VALUE CHANGES'
                      RETURN
                  END IF
                  NCLASS = (KDESC(1,KARY(11)) - NFUNC*16384) / 256
                  IF (NCLASS.EQ.3) THEN
                      NSEG  = MOD(KDESC(1,KARY(11)),256)
                      IF (NSEG.EQ.255) THEN
                          RETURN
                      END IF
                  END IF
                  IERRTN  = 21
                  PRINT *,'INCORRECT OPERATOR DESCRIPTOR ENTRY ',
     *                    'IN LIST OF REFERENCE VALUE CHANGES'
                  RETURN
              END IF
C                               ELEMENT DESCRIPTOR W/NEW REFERENCE VALUE
C                               FIND MATCH FOR CURRENT DESCRIPTOR
              IQ  = INDEXB(KDESC(1,KARY(11)))
              IF (IQ.LT.1) THEN
                  IERRTN  = 22
                  PRINT *,'ATTEMPTING TO ENTER NEW REFERENCE VALUE ',
     *                'INTO TABLE B, BUT DESCRIPTOR DOES NOT EXIST IN ',
     *                'CURRENT MODIFIED TABLE B'
                  RETURN
              END IF
          END IF
      ELSE IF (KCLASS.EQ.4) THEN
C                  SET/RESET ASSOCIATED FIELD WIDTH
          IF (ISTEP.EQ.3) THEN
              KARY(27)  = KSEG
          END IF
      ELSE IF (KCLASS.EQ.5) THEN
C                  SET TO PROCESS TEXT/ASCII DATA
C                                  SET TO TEXT
C                                      PROCESS TEXT

          KARY(2)  = KARY(11) + KARY(18)
          IF (ISTEP.EQ.3) THEN
C                            KSEG TELLS HOW MANY BYTES EACH ITERATION
              IF (MOD(KSEG,4).NE.0) THEN
                  ITER  = KSEG / 4 + 1
              ELSE
                  ITER   = KSEG / 4
              END IF
C                             POINT AT CORRECT KDATA WORD
              IF (ISECT3(3).NE.0) THEN
C                           COMPRESSED
C  ---------------------------------------------------
                  CALL SBYTES(KBUFR,ZEROES,KARY(3),32,0,ITER)
                  KARY(3)  = KARY(3) + KSEG * 8
C
                  CALL SBYTE (KBUFR,KSEG*8,KARY(3),6)
                  KARY(3)  = KARY(3) + 6
C                             TEXT ENTRY BY SUBSET
                  DO 2000 M = 1, ISECT3(1)
                      JAY  = KARY(3)
C                                 NUMBER OF SUBSETS
                      DO 1950 KL  = 1, ITER
C                                 NUMBER OF WORDS
                          KK  = KARY(2) + KL - 1
                          IF (ISECT3(10).EQ.1) THEN
                              CALL W3AI38(KDATA(M,KK),4)
                          END IF
                          CALL SBYTE (KBUFR,KDATA(M,KK),JAY,32)
                          JAY  = JAY + 32
 1950                 CONTINUE
                      KARY(3)  = KARY(3) + KSEG * 8
 2000             CONTINUE
C  ---------------------------------------------------
              ELSE
C                           NOT COMPRESSED

C       CALL SBYTE FOR EACH KDATA VALUE (4 CHARACTERS PER VALUE).
C        AN ADDITIONAL CALL IS DONE IF HAVE A VALUE WITH LESS THAN
C        4 CHARACTERS.
                  NBIT = 32
                  NLEFT = MOD(KSEG,4)
                  DO 3000 J=KARY(2),ITER+KARY(2)-1
                      IF((J.EQ.(ITER+KARY(2)-1)).AND.(NLEFT.NE.0))THEN
                          NBIT = 8 * NLEFT
                      ENDIF
                      IF (ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,J),4)
                      END IF
                      CALL SBYTE(KBUFR,KDATA(I,J),KARY(3),NBIT)
                      KARY(3) = KARY(3) + NBIT
 3000             CONTINUE

C                           ADJUST FOR EXTRA WORDS
                  KARY(18)  = KARY(18) + ITER - 1
              END IF
              KARY(2) = KARY(2) + ITER
          END IF
      ELSE IF (KCLASS.EQ.6) THEN
C                  SET TO SKIP PROCESSING OF NEXT DESCRIPTOR
C                  IF IT IS NOT IN BUFR TABLE B
C                  DURING THE ENCODING PROCESS, THIS HAS NO MEANING
C                  ELIMINATE IN PROCESSING
C                  MOVE DESCRIPTOR LIST UP ONE POSITION AND RESTART
C                  PROCESSING AT SAME LOCATION.
          KM  = I - 1
          DO 9000 KL = I+1, NRDESC
              KM         = KM + 1
              KDESC(1,KM)  = KDESC(1,KL)
 9000     CONTINUE
          NRDESC  = KM
          RETURN 1
      END IF
C  ****************************************************************
      RETURN
      END
C> @brief Expand sequence descriptor.
C> @author Bill Cavanaugh @date 1993-12-03

C> Have encountered a sequence descriptor. must perform proper replacment of
C> descriptors in line.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C>
C> @param[inout] I Current position in descriptor list
C> @param[inout] KDESC List (modified [out]) of descriptors
C> @param[inout] NRDESC Number (new [out]) of descriptors in kdesc
C> @param[in] IUNITD
C> @param[in] KSEQ
C> @param[in] KNUM
C> @param[in] KLIST
C> @param[out] IERRTN Error return value
C> @param ISECT3
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8503(I,KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)

C
      INTEGER      I
      INTEGER      KDESC(3,*)
      INTEGER      NRDESC
      INTEGER      ISECT3(*)
      INTEGER      IUNITD
      INTEGER      KSEQ(*)
      INTEGER      KNUM(*)
      INTEGER      KLIST(300,*)
      INTEGER      IERRTN
      INTEGER      ITAIL(1600)
C     INTEGER      IHOLD(200)
C
      SAVE
C
C  ****************************************************************
      IERRTN  = 0
C                            READ IN TABLE D IF NEEDED
      IF (ISECT3(9).EQ.0) THEN
          CALL FI8513 (IUNITD,ISECT3,KSEQ,
     *                                     KNUM,KLIST,IERRTN)
          IF (IERRTN.NE.0) THEN
C             PRINT *,'EXIT  FI8503A'
              RETURN
          END IF
      END IF
C                           HAVE TABLE D
C
C                    FIND MATCHING SEQUENCE DESCRIPTOR
      DO 100 L = 1, ISECT3(9)
          IF (KDESC(1,I).EQ.KSEQ(L)) THEN
C   JEN - DELETE NEXT PRINT LINE
C             PRINT *,'FOUND ',KDESC(1,I)
C                              HAVE A MATCH
              GO TO 200
          END IF
  100 CONTINUE
      IERRTN  = 12
      RETURN
  200 CONTINUE
C                    REPLACE SEQUENCE DESCRIPTOR WITH IN LINE SEQUENCE
      IPUT    = I
C                           SAVE TAIL
      ISTART  = I + 1
      KK  = 0
      DO 400 IJ  = ISTART, NRDESC
          KK  = KK + 1
          ITAIL(KK)  = KDESC(1,IJ)
  400 CONTINUE
C                           INSERT SEQUENCE OF DESCRIPTORS AT
C                                    CURRENT LOCATION
      KL  = 0
      DO 600 KQ = 1, KNUM(L)
          KDESC(1,IPUT)  = KLIST(L,KQ)
          IPUT = IPUT + 1
  600 CONTINUE

C                           RESTORE TAIL
      DO 800 KL = 1, KK
          KDESC(1,IPUT) = ITAIL(KL)
          IPUT  = IPUT + 1
  800 CONTINUE
C                            RESET NUMBER OF DESCRIPTORS IN KDESC
      NRDESC  = IPUT - 1
C  JEN - DELETE NEXT PRINT LINE
C     PRINT *,' NRDESC IS ',NRDESC

C                           RESET CURRENT POSITION & RETURN
      RETURN
      END
C> @brief Convert descriptors fxy to decimal
C> @author Bill Cavanaugh @date 1993-12-03

C> Construct decimal descriptor values from f x and y segments
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C>
C> @param[in] MIF input flag
C> @param[inout] MDESC list of descriptors in f x y (decimal [out]) form
C> @param[in] NR number of descriptors in mdesc
C> @param[out] IERRTN error return value
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8505(MIF,MDESC,NR,IERRTN)

C
      INTEGER      MDESC(3,*), NR
C
      SAVE
C
      IF (NR.EQ.0) THEN
          IERRTN  = 14
          RETURN
      END IF
C
      DO 100 I = 1, NR
          MDESC(1,I)  = MDESC(1,I) * 16384 + MDESC(2,I) * 256
     *                     + MDESC(3,I)
C   JEN - DELETE NEXT PRINT LINE
C     PRINT *,MDESC(2,I),MDESC(3,I),' BECOMES ',MDESC(1,I)
  100 CONTINUE
      MIF  = 1
      RETURN
      END
C> @brief Process data in non-compressed format
C> @author Bill Cavanaugh @date 1993-12-03

C> Process data into non-compressed format for inclusion into
C> section 4 of the bufr message
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C> - J. Hoppa 1994-03-24 Changed the inner loop from a do loop to a
C> goto loop so nrdesc isn't a set value.
C> corrected a value in the call to fi8503().
C> - J. Hoppa 1994-03-31 Corrected an error in sending the subset
C> number rather than the descriptor number
C> to subroutine fi8501(). Added the subset number to the fi8501() parameter list.
C> - J. Hoppa 1994-04015 Added line to keep the parameter pointer
C> kary(2) up to date.  this variable is used
C> in subroutine fi8502().
C> added kbufr to the parameter list in the call
C> to subroutine fi8502().
C> corrected an infinite loop when have an
C> operator descriptor that was caused by
C> a correction made 94-03-24
C> - J. Hoppa 1994-04-20 Added k to call to subroutine w3fi01
C> - J. Hoppa 1994-04-29 Changed n to kary(11) and k to kary(2)
C> removed k and n from the call to fi8501()
C> - J. Hoppa 1994-05-03 Added an increment to kary(11) to prevent
C> and infinite loop when have a missing value
C> - J. Hoppa 1994-05-18 Changed so increments kary(2) after each
C> call to sbyte and deleted
C> kary(2) = kary(11) + kary(18)
C>
C> @param[in] ISTEP
C> @param[in] ISECT3
C> @param[in] KARY
C> @param[in] JDESC
C> @param[in] NEWNR
C> @param[in] KDESC
C> @param[in] NRDESC
C> @param[in] LDESC
C> @param[in] ANAME
C> @param[in] AUNITS
C> @param[in] KSCALE
C> @param[in] KRFVAL
C> @param[in] KWIDTH
C> @param[in] KRFVSW
C> @param[in] NEWRFV
C> @param[in] KSEQ
C> @param[in] KNUM
C> @param[in] KLIST
C> @param[out] KDATA
C> @param[out] KBUFR
C> @param[out] IERRTN
C> @param IBFSIZ
C> @param INDEXB
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8506(ISTEP,ISECT3,KARY,JDESC,NEWNR,KDESC,NRDESC,
     *           LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,NEWRFV,
     *           KSEQ,KNUM,KLIST,IBFSIZ,
     *           KDATA,KBUFR,IERRTN,INDEXB)

C
C  -------------------------------------------------------------
      INTEGER        ISTEP,INDEXB(*)
      INTEGER        KBUFR(*)
      INTEGER        ISECT3(*)
      INTEGER        KARY(*)
      INTEGER        NRDESC,NEWNR,KDESC(3,*),JDESC(3,*)
      INTEGER        KDATA(500,*)
      INTEGER        KRFVSW(*),KSCALE(*),KRFVAL(*),KWIDTH(*),NEWRFV(*)
      INTEGER        IERRTN
      INTEGER        LDESC(*)
      INTEGER        IBITS(32)
      INTEGER        MISG
      INTEGER        KSEQ(*),KNUM(*),KLIST(300,*)
      CHARACTER*40   ANAME(*)
      CHARACTER*25   AUNITS(*)
      CHARACTER*9    CCITT
      LOGICAL        TEXT
C
      SAVE
C  -------------------------------------------------------------
      DATA  IBITS /         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
      DATA  CCITT /'CCITT IA5'/
      DATA  MISG /99999/
C  -------------------------------------------------------------
       KEND = IBFSIZ * 8 - 32
C                        **********************************************
C                        *                                            *
C                        *      PROCESS AS NON-COMPRESSED MESSAGE     *
C                        *                                            *
C                        *   I POINTS TO SUBSET                       *
C                        *   N POINTS TO DESCRIPTOR                   *
C                        *   K ADJUSTS N TO CORRECT DATA ENTRY        *
C                        *                                            *
C                        **********************************************
      DO 4500 I = 1, ISECT3(1)
C                                  OUTER LOOP FOR EACH SUBSET
C                              DO UNTIL ALL DESCRIPTORS HAVE
C                                  BEEN PROCESSED
C                              SET ADDED BIT FOR WIDTH TO 0
          KARY(26)  = 0
C                             SET ASSOCIATED FIELD WIDTH TO 0
          KARY(27)  = 0
          KARY(18)  = 0
C                    IF MESSAGE CONTAINS DELAYED REPLICATION
C                      WE NEED TO EXPAND THE ORIGINAL DESCRIPTOR LIST
C                      TO MATCH THE INPUT DATA.
C                      START WITH JDESC
          IF (KARY(4).NE.0) THEN
              DO 100 M = 1, NEWNR
                  KDESC(1,M) = JDESC(1,M)
  100         CONTINUE
              NRDESC  = NEWNR
          END IF
          KARY(11) = 1
          KARY(2) = 1
 4300     CONTINUE
          IF(KARY(11).GT.NRDESC) GOTO 4305
C                                  INNER LOOP FOR PARAMETER
 4200         CONTINUE
C             KARY(2) = KARY(11) + KARY(18)
C             PRINT *,'LOOKING AT DESCRIPTOR',KARY(11),
C    *                         KDESC(1,KARY(11)),
C    *                         KARY(2),KDATA(I,KARY(2))
C
C                                  PROCESS ONE DESCRIPTOR AT A TIME
C
C                             ISOLATE TABLE
C
              KFUNC      = KDESC(1,KARY(11)) / 16384
C                             ISOLATE CLASS
              KCLASS     = MOD(KDESC(1,KARY(11)),16384) / 256
              KSEG       = MOD(KDESC(1,KARY(11)),256)
              IF (KFUNC.EQ.1) THEN
C                            REPLICATION DESCRIPTOR
                  CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,I,KDESC,NRDESC,IERRTN)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 4200
              ELSE IF (KFUNC.EQ.2) THEN
C                            OPERATOR DESCRIPTOR
                  CALL FI8502(*4200,KBUFR,KCLASS,KSEG,
     *                                            KDESC,NRDESC,I,ISTEP,
     *            KARY,KDATA,ISECT3,KRFVSW,NEWRFV,LDESC,IERRTN,INDEXB)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  KARY(11) = KARY(11) + 1
                  GO TO 4300
              ELSE IF (KFUNC.EQ.3) THEN
C                            SEQUENCE DESCRIPTOR
                  CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
                  IF (IERRTN.NE.0) THEN
                      RETURN
                  END IF
                  GO TO 4200
              END IF
C                                 FALL THRU WITH ELEMENT DESCRIPTOR
C                                 FIND MATCHING TABLE B ENTRY
              LK  = INDEXB(KDESC(1,KARY(11)))
              IF (LK.LT.1) THEN
C                               FALL THRU WITH NO MATCHING B ENTRY
                  PRINT *,'FI8506 3800',KARY(11),KDESC(1,KARY(11)),
     *                     NRDESC,LK,LDESC(LK)
                  IERRTN  = 2
                  RETURN
              END IF
C
              IF (AUNITS(LK).EQ.CCITT) THEN
                  TEXT  = .TRUE.
              ELSE
                  TEXT  = .FALSE.
              END IF
C
              IF (TEXT) THEN
                  JWIDE  = KWIDTH(LK)
 3775             CONTINUE
                  IF (JWIDE.GT.32) THEN
                      IF(ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,KARY(2)),4)
                      END IF
                      IF ((KARY(3)+32).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),32)
                      KARY(3)  = KARY(3) + 32
C                                 ADD A WORD HERE ONLY
                      KARY(18)  = KARY(18) + 1
C                     KARY(2)  = KARY(11) + KARY(18)
                      KARY(2) = KARY(2) + 1
                      JWIDE  = JWIDE - 32
                      GO TO 3775
                  ELSE IF (JWIDE.EQ.32) THEN
                      IF(ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,KARY(2)),4)
                      END IF
                      IF ((KARY(3)+32).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),32)
                      KARY(3)  = KARY(3) + 32
                      KARY(2) = KARY(2) + 1
                      JWIDE  = JWIDE - 32
                  ELSE IF (JWIDE.GT.0) THEN
                      IF(ISECT3(10).NE.0) THEN
                          CALL W3AI38 (KDATA(I,KARY(2)),4)
                      END IF
                      IF ((KARY(3)+JWIDE).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),JWIDE)
                      KARY(3)  = KARY(3) + JWIDE
                      KARY(2) = KARY(2) + 1
                  END IF
              ELSE
C                               NOT TEXT
                  IF (KARY(27).NE.0.AND.KDESC(1,KARY(11)).NE.7957) THEN
C                                 ENTER ASSOCIATED FIELD
                      IF ((KARY(3)+KARY(27)).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),
     *                     KARY(27))
                      KARY(3)  = KARY(3) + KARY(27)
                      KARY(18)  = KARY(18) + 1
C                     KARY(2)  = KARY(11) + KARY(18)
                      KARY(2) = KARY(2) + 1
                  END IF
C
                  JWIDE  = KWIDTH(LK) + KARY(26)
                  IF (KDATA(I,KARY(2)).EQ.MISG) THEN
C                                 MISSING DATA, SET ALL BITS ON
                      IF ((KARY(3)+JWIDE).GT.KEND) THEN
                          IERRTN = 1
                          RETURN
                      END IF
                      CALL SBYTE (KBUFR,IBITS(JWIDE),KARY(3),JWIDE)
                      KARY(3)  = KARY(3) + JWIDE
                      KARY(2) = KARY(2) + 1
                      KARY(11) = KARY(11) + 1
                      GO TO 4300
                  END IF
C                                 CAN DATA BE CONTAINED IN SPECIFIED
C                                          BIT WIDTH, IF NOT - ERROR
                  IF (KDATA(I,KARY(2)).GT.IBITS(JWIDE)) THEN
                      IERRTN = 1
                      RETURN
                  END IF
C                                 ADJUST WITH REFERENCE VALUE
                  IF (KRFVSW(LK).EQ.0) THEN
                      JRV  = KRFVAL(LK)
                  ELSE
                      JRV  = NEWRFV(LK)
                  END IF
C
                  KDATA(I,KARY(2)) = KDATA(I,KARY(2)) - JRV
C                                      IF NEW VALUE IS NEGATIVE - ERROR
                  IF (KDATA(I,KARY(2)).LT.0) THEN
                      IERRTN  = 11
                      RETURN
                  END IF
C                                 PACK DATA INTO OUTPUT ARRAY
                  IF ((KARY(3)+JWIDE).GT.KEND) THEN
                      IERRTN = 1
                      RETURN
                  END IF
                  CALL SBYTE (KBUFR,KDATA(I,KARY(2)),KARY(3),JWIDE)
                  KARY(2) = KARY(2) + 1
                  KARY(3)  = KARY(3) + JWIDE
              END IF
          KARY(11) = KARY(11) + 1
          GOTO 4300
 4305     CONTINUE
C                              RESET ALL REFERENCE VALUES TO ORIGINAL
          DO 4310 LX = 1, ISECT3(8)
              KRFVSW(LX)  = 0
 4310     CONTINUE
 4500 CONTINUE
      RETURN
      END
C> @brief Combine integer/text data
C> @author Bill Cavanaugh @date 1993-12-03

C> Construct integer subset from real and text data
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C> - J. Hoppa 1994-03-31 added ksub to fi8501() parameter list.
C> - J. Hoppa 1994-04-18 added dummy variable idum to fi8502() parameter list.
C> - J. Hoppa 1994-04-20 added dummy variable ll to fi8501() parameter list.
C> - J. Hoppa 1994-04-29 changed i to kary(11) added a kary(2) assignment so have something
C> to pass to subroutines ** test this ** removed i and ll from call to fi8501()
C> - J. Hoppa 1994-05-13 added code to calculate kwords when kfunc=2
C> - J. Hoppa 1994-05-18 deleted kary(2) assignment
C>
C> @param[in] ISTEP
C> @param[in] IUNITB Unit number of device containing table b
C> @param[in] IDATA Integer working array
C> @param[in] KDESC Expanded descriptor set
C> @param[in] NRDESC Number of descriptors in kdesc
C> @param[in] ATEXT Text data for ccitt ia5 and text operator fields
C> @param[in] KSUB Subset number
C> @param[in] KARY Working array
C> @param[in] ISECT3
C> @param[out] KDATA Array containing integer subsets
C> @param[out] LDESC List of table b descriptors (decimal)
C> @param[out] ANAME List of descriptor names
C> @param[out] AUNITS Units for each descriptor
C> @param[out] KSCALE Base 10 scale factor for each descriptor
C> @param[out] KRFVAL Reference value for each descriptor
C> @param[out] KRFVSW
C> @param[out] KWIDTH Standard bit width to contain each value for specific descriptor
C> @param[out] KASSOC
C> @param[out] IERRTN Error return flag
C> @param IUNITD
C> @param KSEQ
C> @param KNUM
C> @param KLIST
C> @param INDEXB
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8508(ISTEP,IUNITB,IDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)

C                         TAKE EACH NON-TEXT ENTRY OF SECTION 2
C                               ACCEPT IT
C
C                         TAKE EACH TEXT ENTRY
C                               INSERT INTO INTEGER ARRAY,
C                               ADDING FULL WORDS AS NECESSARY
C                               MAKE SURE ANY LAST WORD HAS TEXT DATA
C                               RIGHT JUSTIFIED
C  ---------------------------------------------------------------------
C                          PASS BACK CONVERTED ENTRY TO LOCATION
C                          SPECIFIED BY USER
C
C                          REFERENCE VALUE WILL BE APPLIED DURING
C                          ENCODING OF MESSAGE
C  ---------------------------------------------------------------------
      INTEGER          IUNITB,IUNITD,KSEQ(*),KNUM(*),KLIST(300,*)
      INTEGER          KDESC(3,*),NRDESC,KASSOC(*)
      INTEGER          IDATA(*),ISTEP
      INTEGER          KDATA(500,*)
      INTEGER          KARY(*),INDEXB(*)
      INTEGER          KSUB,K
      INTEGER          LDESC(*)
      INTEGER          IBITS(32)
      INTEGER          KSCALE(*)
      INTEGER          KRFVAL(*)
      INTEGER          KRFVSW(*)
      INTEGER          KWIDTH(*)
      INTEGER          MISG
      INTEGER          MPTR,ISECT3(*)
      CHARACTER*1      ATEXT(*)
      CHARACTER*1      AHOLD1(256)
      INTEGER          IHOLD4(64)
      CHARACTER*25     AUNITS(*)
      CHARACTER*25     CCITT
      CHARACTER*40     ANAME(*)
C
      SAVE
C
      EQUIVALENCE      (AHOLD1,IHOLD4)
C
C  =====================================
      DATA  CCITT /'CCITT IA5                '/
      DATA  IBITS /         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
      DATA  MISG  /99999/
C
      IF (ISECT3(8).EQ.0) THEN
          CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
      END IF
C                         HAVE TABLE B AVAILABLE NOW
C
C                         LOOK AT EACH DATA ENTRY
C                              CONVERT NON TEXT
C                              MOVE TEXT
C
      KPOS  = 0
      MPTR  = 0
      KARY(11) = 0
 1000 CONTINUE
      KARY(11) = KARY(11) + 1
      IF (KARY(11).GT.NRDESC) GO TO 1500
C
C                  RE-ENTRY POINT FOR REPLICATION AND SEQUENCE DESCR'S
C
  500 CONTINUE
      KFUNC  = KDESC(1,KARY(11)) / 16384
      KL  = KDESC(1,KARY(11)) - 16384 * KFUNC
      KCLASS  = KL / 256
      KSEG    = MOD(KL,256)
C     KARY(2) = KARY(11) + KARY(18)
      IF (KFUNC.EQ.1) THEN
C                              REPLICATION DESCRIPTOR
          CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,KSUB,KDESC,NRDESC,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE IF (KFUNC.EQ.2) THEN
          IF (KCLASS.EQ.5) THEN
C                        HANDLE TEXT OPERATORS
CC
              KAVAIL  = IDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
CC
              JWIDE  = KSEG * 8
              GO TO 1200
          END IF
      ELSE IF (KFUNC.EQ.3) THEN
C                          SEQUENCE DESCRIPTOR - ERROR
          CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE
C
C                         FIND MATCHING DESCRIPTOR
C
          K  = INDEXB(KDESC(1,KARY(11)))
          IF (K.LT.1) THEN
              PRINT *,'FI8508-NOT FOUND',KARY(11),KDESC(1,KARY(11)),
     *           ISECT3(8),LDESC(K)
              IERRTN  = 2
              RETURN
          END IF
C                           HAVE MATCHING DESCRIPTOR
  200     CONTINUE
          IF (AUNITS(K)(1:9).NE.CCITT(1:9)) THEN
              IF (KARY(27).NE.0) THEN
                  IF (KDESC(1,KARY(11)).LT.7937.OR.
     *                           KDESC(1,KARY(11)).GT.8191) THEN
C                        ASSOC FLD FOR ALL BUT CLASS 31
                      KPOS  = KPOS + 1
                      IF (KASSOC(KARY(11)).EQ.IBITS(KARY(27))) THEN
                          KDATA(KSUB,KPOS)  = MISG
                      ELSE
                          KDATA(KSUB,KPOS)  = KASSOC(KARY(11))
                      END IF
                  END IF
              END IF
C                        IF NOT MISSING DATA
              IF (IDATA(KARY(11)).EQ.99999) THEN
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS)  = MISG
              ELSE
C                           PROCESS INTEGER VALUES
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS) = IDATA(KARY(11))
              END IF
          ELSE
C                         PROCESS TEXT
C                                 NUMBER OF BYTES REQUIRED BY TABLE B
              KREQ    = KWIDTH(K) / 8
C                                   NUMBER BYTES AVAILABLE IN ATEXT
              KAVAIL  = IDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
C                                 MOVE TEXT CHARACTERS TO KDATA
              JWIDE   = KWIDTH(K)
              GO TO 1200
          END IF
      END IF
      GO TO 1000
 1200 CONTINUE
  300 CONTINUE
      NPTR  = MPTR
      DO 400 IJ = 1, KWORDS
              KPOS  = KPOS + 1
              CALL GBYTE(ATEXT,KDATA(KSUB,KPOS),NPTR,32)
              NPTR  = NPTR + 32
  400     CONTINUE
      MPTR  = MPTR + JWIDE
      GO TO 1000
 1500 CONTINUE
      RETURN
      END
C> @brief Convert real/text input to integer
C> @author Bill Cavanaugh @date 1993-12-03

C> Construct integer subset from real and text data.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C> - J. Hoppa 1994-03-31 Added ksub to the fi8501 parameter list.
C> - J. Hoppa 1994-04-18 Added dummy variable idum to fi8502 parameter list.
C> - J. Hoppa 1994-04-20 Added dummy variable ll to fi8501 parameter list.
C> - J. Hoppa 1994-04-29 Changed i to kary(11) added a kary(2) assignment so have something
C> to pass to subroutines ** test this ** removed i and ll from call to fi8501
C> - J. Hoppa 1994-05-18 Deleted kary(2) assignment
C>
C> @param[in] IUNITB unit number of device containing table b
C> @param[in] RDATA real working array
C> @param[in] KDESC expanded descriptor set
C> @param[in] NRDESC number of descriptors in kdesc
C> @param[in] ATEXT text data for ccitt ia5 and text operator fields
C> @param[in] KSUB subset number
C> @param[in] KARY working array
C> @param[in] ISECT3
C> @param[in] IUNITD
C> @param[out] KDATA Array containing integer subsets
C> @param[out] LDESC List of table b descriptors (decimal)
C> @param[out] ANAME List of descriptor names
C> @param[out] AUNITS Units for each descriptor
C> @param[out] KSCALE Base 10 scale factor for each descriptor
C> @param[out] KRFVAL Reference value for each descriptor
C> @param[out] KRFVSW
C> @param[out] KASSOC
C> @param[out] KWIDTH Standard bit width to contain each value for specific descriptor
C> @param[out] IERRTN Error return flag
C> @param[out] KNUM
C> @param[out] KLIST
C> @param ISTEP
C> @param KSEQ
C> @param INDEXB
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8509(ISTEP,IUNITB,RDATA,KDESC,NRDESC,ATEXT,KSUB,KARY,
     *            KDATA,LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KRFVSW,ISECT3,
     *            KWIDTH,KASSOC,IUNITD,KSEQ,KNUM,KLIST,IERRTN,INDEXB)

C                         TAKE EACH NON-TEXT ENTRY OF SECTION 2
C                               SCALE IT
C                               ROUND IT
C                               CONVERT TO INTEGER
C
C                         TAKE EACH TEXT ENTRY
C                               INSERT INTO INTEGER ARRAY,
C                               ADDING FULL WORDS AS NECESSARY
C                               MAKE SURE ANY LAST WORD HAS TEXT DATA
C                               RIGHT JUSTIFIED
C                          PASS BACK CONVERTED ENTRY TO LOCATION
C                          SPECIFIED BY USER
C
C                          REFERENCE VALUE WILL BE APPLIED DURING
C                          ENCODING OF MESSAGE
C  ---------------------------------------------------------------------
      REAL              RDATA(*)
      INTEGER          IUNITB,IUNITD,KSEQ(*),KNUM(*),KLIST(300,*)
      INTEGER          IBITS(32),INDEXB(*)
      INTEGER          KDESC(3,*),ISTEP
      INTEGER          KDATA(500,*)
      INTEGER          KASSOC(*)
      INTEGER          KARY(*)
      INTEGER          KSUB,K
      INTEGER          LDESC(*)
      INTEGER          NRDESC
      INTEGER          IERRTN
      INTEGER          KSCALE(*)
      INTEGER          KRFVAL(*)
      INTEGER          KRFVSW(*)
      INTEGER          KWIDTH(*)
      INTEGER          MPTR,ISECT3(*)
      INTEGER          MISG
      CHARACTER*1      AHOLD1(256)
      INTEGER          IHOLD4(64)
      CHARACTER*1      ATEXT(*)
      CHARACTER*25     AUNITS(*)
      CHARACTER*25     CCITT
      CHARACTER*40     ANAME(*)
C
      SAVE
C  =====================================
      EQUIVALENCE      (AHOLD1,IHOLD4)
C
      DATA   IBITS/         1,          3,          7,         15,
     *                     31,         63,        127,        255,
     *                    511,       1023,       2047,       4095,
     *                   8191,      16383,      32767,      65535,
     *             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
     *             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
     *             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
     *             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
C
      DATA  CCITT /'CCITT IA5                '/
      DATA  MISG  /99999/
C  =====================================
C
      IF (ISECT3(8).EQ.0) THEN
          CALL FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
      END IF
C                         HAVE TABLE B AVAILABLE NOW
C
C                         LOOK AT EACH DATA ENTRY
C                              CONVERT NON TEXT
C                              MOVE TEXT
C
      KPOS  = 0
      MPTR  = 0
      KARY(11) = 0
 1000 CONTINUE
      KARY(11) = KARY(11) + 1
      IF (KARY(11).GT.NRDESC) GO TO 1500
C                         RE-ENRY POINT FOR REPLICATION AND
C                          SEQUENCE DESCRIPTORS
  500 CONTINUE
      KFUNC  = KDESC(1,KARY(11)) / 16384
      KL  = KDESC(1,KARY(11)) - 16384 * KFUNC
      KCLASS  = KL / 256
      KSEG    = MOD(KL,256)
C     KARY(2) = KARY(11) + KARY(18)
      IF (KFUNC.EQ.1) THEN
C                              REPLICATION DESCRIPTOR
          CALL FI8501(KARY,ISTEP,KCLASS,KSEG,IDATA,RDATA,
     *                  KDATA,KSUB,KDESC,NRDESC,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE IF (KFUNC.EQ.2) THEN
C                        HANDLE OPERATORS
          IF (KCLASS.EQ.5) THEN
C                                   NUMBER BYTES AVAILABLE IN ATEXT
              KAVAIL  = RDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
              JWIDE  = KSEG * 8
              GO TO 1200
          ELSE IF (KCLASS.EQ.2) THEN
              IF (KSEG.EQ.0) THEN
                  KARY(9) = 0
              ELSE
                  KARY(9) = KSEG - 128
              END IF
              GO TO 1200
          END IF
      ELSE IF (KFUNC.EQ.3) THEN
C                          SEQUENCE DESCRIPTOR - ERROR
          CALL FI8503(KARY(11),KDESC,NRDESC,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
          IF (IERRTN.NE.0) THEN
              RETURN
          END IF
          GO TO 500
      ELSE
C
C                         FIND MATCHING DESCRIPTOR
C
          K  = INDEXB(KDESC(1,KARY(11)))
          IF (K.LT.1) THEN
              IERRTN  = 2
C             PRINT *,'FI8509 - IERRTN = 2'
              RETURN
          END IF
C                           HAVE MATCHING DESCRIPTOR
  200     CONTINUE
          IF (AUNITS(K)(1:9).NE.CCITT(1:9)) THEN
              IF (KARY(27).NE.0) THEN
                  IF (KDESC(1,KARY(11)).LT.7937.OR.
     *                           KDESC(1,KARY(11)).GT.8191) THEN
C                        ASSOC FLD FOR ALL BUT CLASS 31
                      KPOS  = KPOS + 1
                      IF (KASSOC(KARY(11)).EQ.IBITS(KARY(27))) THEN
                          KDATA(KSUB,KPOS)  = MISG
                      ELSE
                          KDATA(KSUB,KPOS)  = KASSOC(KARY(11))
                      END IF
                  END IF
              END IF
C                        IF NOT MISSING DATA
              IF (RDATA(KARY(11)).EQ.99999.) THEN
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS)  = MISG
              ELSE
C                           PROCESS REAL VALUES
                  IF (KSCALE(K).NE.0) THEN
C                                    SCALING ALLOWING FOR CHANGE SCALE
                      SCALE  = 10. **(IABS(KSCALE(K)) + KARY(9))
                      IF (KSCALE(K).LT.0) THEN
                          RDATA(KARY(11)) = RDATA(KARY(11)) / SCALE
                      ELSE
                          RDATA(KARY(11)) = RDATA(KARY(11)) * SCALE
                      END IF
                  END IF
C                          PERFORM ROUNDING
                  RDATA(KARY(11)) = RDATA(KARY(11)) +
     *                              SIGN(0.5,RDATA(KARY(11)))
C                          CONVERT TO INTEGER
                  KPOS  = KPOS + 1
                  KDATA(KSUB,KPOS) = RDATA(KARY(11))
C
              END IF
          ELSE
C                       PROCESS TEXT
C                                NUMBER OF BYTES REQUIRED BY TABLE B
              KREQ    = KWIDTH(K) / 8
C                                   NUMBER BYTES AVAILABLE IN ATEXT
              KAVAIL  = RDATA(KARY(11))
C                                 UNUSED POSITIONS IN LAST WORD
              KREM  = MOD(KAVAIL,4)
              IF (KREM.NE.0) THEN
                  KWORDS  = KAVAIL / 4 + 1
              ELSE
                  KWORDS  = KAVAIL / 4
              END IF
C                                 MOVE TEXT CHARACTERS TO KDATA
              JWIDE   = KWIDTH(K)
              GO TO 1200
          END IF
      END IF
      GO TO 1000
 1200 CONTINUE
  300 CONTINUE
      NPTR  = MPTR
      DO 400 IJ = 1, KWORDS
          KPOS  = KPOS + 1
          CALL GBYTE(ATEXT,KDATA(KSUB,KPOS),NPTR,32)
          NPTR  = NPTR + 32
  400 CONTINUE
      MPTR  = MPTR + JWIDE
      GO TO 1000
 1500 CONTINUE
C     DO 2000 I = 1, KPOS
C2000 CONTINUE
      RETURN
      END
C> @brief Rebuild kdesc from jdesc
C> @author Bill Cavanaugh @date 1993-12-03

C> Construct working descriptor list from list of descriptors in section 3.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C>
C> @param[in] ISECT3
C> @param[in] KARY Utility - array see main routine
C> @param[in] JIF Descriptor input form flag
C> @param[in] JDESC List of descriptors for section 3
C> @param[in] NEWNR Number of descriptors in jdesc
C> @param[out] KIF Descriptor form
C> @param[out] KDESC Working list of descriptors
C> @param[out] NRDESC Number of descriptors in kdesc
C> @param[out] IERRTN Error return
C> - IERRTN = 0 Normal return
C> - IERRTN = 5 Found delayed replication during expansion
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8511(ISECT3,KARY,JIF,JDESC,NEWNR,
     *                        KIF,KDESC,NRDESC,IERRTN)

C
      INTEGER      JDESC(3,*), NEWNR, KDESC(3,*), NRDESC
      INTEGER      KARY(*),IERRTN,KIF,JIF
      INTEGER      ISECT3(*)
C
      SAVE
C
      IF (NEWNR.EQ.0) THEN
          IERRTN  = 3
          RETURN
      END IF
C
      NRDESC = NEWNR
      IF (JIF.EQ.0) THEN
          JIF    = 1
          DO  90 I = 1, NEWNR
             KDESC(1,I) = JDESC(1,I)*16384 + JDESC(2,I)*256 + JDESC(3,I)
             JDESC(1,I) = JDESC(1,I)*16384 + JDESC(2,I)*256 + JDESC(3,I)
   90     CONTINUE
      ELSE
          DO 100 I = 1, NEWNR
              KDESC(1,I)  = JDESC(1,I)
  100     CONTINUE
          NRDESC  = NEWNR
      END IF
      KIF    = 1
 9000 CONTINUE
      RETURN
      END
C> @brief Read in table B
C> @author Bill Cavanaugh @date 1993-12-03

C> Read in tailored set of table B descriptors.
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C> - J. Hoppa 1994-04-18 An error has been corrected to prevent later
C> searching table b if there are only operator
C> descriptors in the descriptor list.
C> - J. Hoppa 1994-05-17 Changed the loop for expanding sequence
C> descriptors from a do loop to a goto loop
C>
C> @param[in] IUNITB Unit where table b entries reside
C> @param[in] KDESC Working descriptor list
C> @param[in] NRDESC Number of descriptors in kdesc
C> @param[in] IUNITD Unit where table d entries reside
C> @param[out] KARY
C> @param[out] IERRTN
C> @param[out] LDESC Descriptors in table b (decimal values)
C> @param[out] ANAME Array containing names of descriptors
C> @param[out] AUNITS Array containing units of descriptors
C> @param[out] KSCALE Scale values for each descriptor
C> @param[out] KRFVAL Reference values for each descriptor
C> @param[out] KWIDTH Bit width of each descriptor
C> @param[out] KRFVSW New reference value switch
C> @param[out] KSEQ Sequence descriptor
C> @param[out] KNUM Number of descriptors in sequence
C> @param[out] KLIST Sequence of descriptors
C> @param ISECT3
C> @param INDEXB
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8512(IUNITB,ISECT3,KDESC,NRDESC,KARY,IERRTN,
     *               LDESC,ANAME,AUNITS,KSCALE,KRFVAL,KWIDTH,KRFVSW,
     *               IUNITD,KSEQ,KNUM,KLIST,INDEXB)

C
      INTEGER    KARY(*),LDESC(*),KSCALE(*),KRFVAL(*),KWIDTH(*)
      INTEGER    KDESC(3,*), NRDESC, IUNITB, IERRTN, KRFVSW(*)
      INTEGER    ISECT3(*),KEY(3,1600),INDEXB(*)
      INTEGER    IUNITD,KSEQ(*),KNUM(*),KLIST(300,*)
      CHARACTER*40   ANAME(*)
      CHARACTER*25   AUNITS(*)
C
      INTEGER    MDESC(800),MR,I,J
C
      SAVE
C
C  ===================================================================
      IERRTN  = 0
      DO 100 I = 1, 30
          KARY(I)  = 0
  100 CONTINUE
C INITIALIZE DESCRIPTOR POINTERS TO MISSING
      DO 105 I = 1, 16383
          INDEXB(I) = -1
  105 CONTINUE
C
C  ===================================================================
C                                MAKE A COPY OF THE DESCRIPTOR LIST
C                                   ELIMINATING REPLICATION/OPERATORS
      J  = 0
      DO 110 I = 1, NRDESC
          IF (KDESC(1,I).GE.49152.OR.KDESC(1,I).LT.16384) THEN
              J  = J + 1
              KEY(1,J)  = KDESC(1,I)
          END IF
  110 CONTINUE
      KCNT  = J
C  ===================================================================
C                                REPLACE ALL SEQUENCE DESCRIPTORS
C  JEN - FIXED NEXT BLOCK
C     DO 300 I = 1, KCNT
      I = 1
  300 IF(I.LE.KCNT)THEN
  200     CONTINUE
          IF (KEY(1,I).GE.49152) THEN
              CALL FI8503(I,KEY,KCNT,
     *                     ISECT3,IUNITD,KSEQ,KNUM,KLIST,IERRTN)
              IF (IERRTN.NE.0) THEN
                  RETURN
              END IF
              GO TO 200
          END IF
          I=I+1
          GOTO 300
      ENDIF
C 300 CONTINUE
C  ===================================================================
C                                ISOLATE SINGLE COPIES OF DESCRIPTORS
      MR  = 1
C        THE FOLLOWING LINE IS TO PREVENT LATER SEARCHING TABLE B WHEN
C        HAVE ONLY OPERATOR DESCRIPTORS
      IF(KCNT.EQ.0) GOTO 9000
      MDESC(MR)  = KEY(1,1)
      DO 500 I = 2, KCNT
          DO 400 J = 1, MR
              IF (KEY(1,I).EQ.MDESC(J)) THEN
                  GO TO 500
              END IF
  400     CONTINUE
          MR  = MR + 1
          MDESC(MR)  = KEY(1,I)
  500 CONTINUE
C  ===================================================================
C                                SORT INTO ASCENDING ORDER
C                                READ IN MATCHING ENTRIES FROM TABLE B
      DO 700 KCUR = 1, MR
          NEXT  = KCUR + 1
          IF (NEXT.LE.MR) THEN
              DO 600 LR  = NEXT, MR
                  IF (MDESC(KCUR).GT.MDESC(LR)) THEN
                      IHOLD       = MDESC(LR)
                      MDESC(LR)   = MDESC(KCUR)
                      MDESC(KCUR) = IHOLD
                  END IF
  600         CONTINUE
          END IF
  700 CONTINUE
C  ===================================================================
      REWIND IUNITB
C
C                             READ IN A MODIFIED TABLE B -
C                             MODIFIED TABLE B CONTAINS ONLY
C                             THOSE DESCRIPTORS ASSOCIATED WITH
C                             CURRENT DATA.
C
      KTRY  = 0
      DO 1500 NRTBLB = 1, MR
 1000     CONTINUE
 1001     FORMAT (I1,I2,I3,A40,A25,I4,8X,I7,I5)
          READ (IUNITB,1001,END=2000,ERR=8000)KF,KX,KY,ANAME(NRTBLB),
     *     AUNITS(NRTBLB),KSCALE(NRTBLB),KRFVAL(NRTBLB),KWIDTH(NRTBLB)
          KRFVSW(NRTBLB) = 0
          LDESC(NRTBLB)  = KX*256 + KY
C
          IF (LDESC(NRTBLB).EQ.MDESC(NRTBLB)) THEN
C             PRINT *,'1001',NRTBLB,LDESC(NRTBLB)
C             PRINT *,LDESC(NRTBLB),ANAME(NRTBLB),KSCALE(NRTBLB),
C    *               KRFVAL(NRTBLB),KWIDTH(NRTBLB)
              KTRY  = KTRY + 1
              INDEXB(LDESC(NRTBLB)) = KTRY
C             PRINT *,'INDEX(',LDESC(NRTBLB),' = ',KTRY
          ELSE IF (LDESC(NRTBLB).GT.MDESC(NRTBLB)) THEN
C             PRINT *,'FI8512 - IERRTN=2'
              IERRTN  = 2
              RETURN
          ELSE
              GO TO 1000
          END IF
 1500 CONTINUE
      IF (KTRY.NE.MR) THEN
          PRINT *,'DO NOT HAVE A COMPLETE SET OF TABLE B ENTRIES'
          IERRTN  = 2
          RETURN
      END IF
C     DO 1998 I = 1, 16383, 30
C         WRITE (6,1999) (INDEXB(I+J),J=0,23)
C1998 CONTINUE
C1999 FORMAT(30(1X,I3))
C
 2000 CONTINUE
      IERRTN     = 0
      ISECT3(8)  = MR
      GO TO 9000
 8000 CONTINUE
      IERRTN = 4
 9000 CONTINUE
      RETURN
      END
C> @brief Read in table D
C> @author Bill Cavanaugh @date 1993-12-03

C> Read in table D
C>
C> Program history log:
C> - Bill Cavanaugh 1993-12-03
C>
C> @param[in] IUNITD Unit number of input device
C> @param[out] KSEQ Key for sequence descriptors
C> @param[out] KNUM Number if descriptors in list
C> @param[out] KLIST Descriptors list
C> @param[out] IERRTN Error return flag
C> @param ISECT3
C>
C> @author Bill Cavanaugh @date 1993-12-03
      SUBROUTINE FI8513 (IUNITD,ISECT3,KSEQ,KNUM,KLIST,IERRTN)

C
      INTEGER      IUNITD, ISECT3(*)
      INTEGER      KSEQ(*),KNUM(*),KLIST(300,*)
      INTEGER      KKF(10),KKX(10),KKY(10),KF,KX,KY
C
      SAVE
C
      REWIND IUNITD
      J     = 0
      IERRTN  = 0
 1000 CONTINUE
      READ (IUNITD,1001,END=9000,ERR=8000)KF,KX,KY,
     *                KKF(1),KKX(1),KKY(1),
     *                KKF(2),KKX(2),KKY(2),
     *                KKF(3),KKX(3),KKY(3),
     *                KKF(4),KKX(4),KKY(4),
     *                KKF(5),KKX(5),KKY(5),
     *                KKF(6),KKX(6),KKY(6),
     *                KKF(7),KKX(7),KKY(7),
     *                KKF(8),KKX(8),KKY(8),
     *                KKF(9),KKX(9),KKY(9),
     *                KKF(10),KKX(10),KKY(10)
 1001 FORMAT (11(I1,I2,I3,1X),3X)
      J  = J + 1
C                             BUILD SEQUENCE KEY
      KSEQ(J)  = 16384*KF + 256*KX + KY
      DO 2000 LM = 1, 10
C                             BUILD KLIST
          KLIST(J,LM) = 16384*KKF(LM) + 256*KKX(LM) + KKY(LM)
          IF(KLIST(J,LM).NE.0) THEN
              KNUM(J)  = LM
          END IF
 2000 CONTINUE
      GO TO 1000
 8000 CONTINUE
      IERRTN  = 6
 9000 CONTINUE
      ISECT3(9) = J
      RETURN
      END
