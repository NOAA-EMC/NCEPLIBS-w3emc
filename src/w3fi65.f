C> @file
C> @brief NMC office note 29 report packer.
C> @author L. Marx @date 1990-01

C> Packs an array of upper-air reports into the format
C> described by NMC office note 29, or packs an array of surface
C> reports into the format described by NMC office note 124. Input
C> integer, real or character type as specified in the category
C> tables in the write-up for w3fi64() (the office note 29 report
C> packer) are converted to character data. Missing character data
C> are specified as strings of 9's except for that converted from
C> input character type which are generally specified as blanks.
C> This library is similar to w3ai03() except w3ai03() was written in
C> assembler.
C>
C> Program history log:
C> - L. Marx 1990-01 Converted code from assembler
C> to vs fortran.
C> - Dennis Keyser 1991-08-23 Use same arguments as w3ai03() ;
C> Streamlined code; Docblocked and commented.
C> - Dennis Keyser 1992-06-29 Convert to cray cft77 fortran.
C> - Dennis Keyser 1992-07-09 Checks the number of characters
C> used by each variable prior to conversion from
C> integer to character format; If this number is
C> greater than the number of characters allocated for
C> the variable the variable is packed as "missing"
C> (i.e., stores as all 9's).
C> - Dennis Keyser 1993-06-28 Initializes number of words in
C> report to 42 in case "strange" report with no data
C> in any category encountered (used to be zero, but
C> such "strange" reports caused code to fail).
C> - Dennis Keyser 1993-12-22 Corrected error which resulted
C> in storage of 0's in place of actual data in a
C> category when that category was the only one with
C> data.
C> - Dennis Keyser 1998-08-07 Fortran 90-compliant - split an
C> if statement into 2-parts to prevent f90 floating
C> point exception error that can now occur in some
C> cases (did not occur in f77).
C>
C> @param[in] LOCRPT Integer array containing one unpacked report.
C> LOCRPT must begin on a fullword boundary. Format
C> is mixed, user must equivalence real and character
C> arrays to this array (see w3fi64 write-up for
C> content).
C> @param[out] COCBUF CHARACTER*10 Array containing a packed report in
C> NMC office note 29/124 format.
C>
C> @note After first creating and writing out the office note 85
C> (first) date record, the user's fortran program begins a packing
C> loop as follows.. Each iteration of the packing loop consists of
C> a call first to w3fi65() to pack the report into COCBUF, then a call
C> to w3fi66() with the current value of 'NFLAG' (set to zero for first
C> call) to block the packed report into a record (see w3fi66() write-
C> up). if 'NFLAG' is -1 upon returning from w3fi66(), the remaining
C> portion of the record is not large enough to hold the current
C> packed report. The user should write out the record, set 'NFLAG'
C> to zero, call w3fi66() to write the packed report to the beginning
C> of the next record, and repeat the packing loop. If 'NFLAG' is
C> positive, a packed report has been blocked into the record and
C> the user should continue the packing loop.
C> When all reports have been packed and blocked, the user
C> should write out this last record (which is not full but contains
C> fill information supplied by w3fi66()). One final record containing
C> the string 'endof file' (sic) followed by blank fill must be
C> written out to signal the end of the data set.
C>
C> @note 1: The packed report will have the categories ordered as
C> follows:  1, 2, 3, 4, 5, 6, 7, 51, 52, 8, 9.
C> @note 2: The input unpacked report must be in the format spec-
C> ified in the w3fi64() office note 29 report unpacker write-up.
C> @note 3: The unused porion of cocbuf is not cleared.

C> @note Entry w3ai03() duplicates processing in w3fi65() since no
C> assembly language code in cray w3lib.
C>
C> @author L. Marx @date 1990-01
      SUBROUTINE W3FI65(LOCRPT,COCBUF)
C
      CHARACTER*12  HOLD
      CHARACTER*10  COCBUF(*),FILL
      CHARACTER*7   CNINES
      CHARACTER*4   COCRPT(10000)
      CHARACTER*2   KAT(11)
C
      INTEGER  LOCRPT(*),KATL(11),KATO(11),KATGC(20,11),KATGL(20,11),
     $ MOCRPT(5000),KATLL(11)
C
      REAL  ROCRPT(5000)
C
      EQUIVALENCE (ROCRPT,MOCRPT,COCRPT)
C
      SAVE
C
      DATA  KATL/6,4,4,4,6,6,3,20,15,3,1/,KATO/13,15,17,19,21,23,25,29,
     $ 31,27,33/,IMSG/99999/,FILL/'XXXXXXXXXX'/,KAT/'01','02','03','04',
     $'05','06','07','51','52','08','09'/,CNINES/'9999999'/,XMSG/99999./
      DATA KATGC/    5*2,4,14*0, 3*2,4,16*0, 3*2,4,16*0,   3*2,4,16*0,
     $  5*2,4,14*0,  5*2,4,14*0, 2*2,4,17*0, 8*2,4,10*1,2, 15*1,5*0,
     $  2*2,4,17*0,  4,19*0/
      DATA KATGL/    5,4,3*3,4,14*0, 5,4,2*3,16*0,   5,2*3,2,16*0,
     $ 5,2*3,2,16*0, 5,4,3*3,4,14*0, 5,4,3*3,4,14*0, 5,3,2,17*0,
     $ 2*5,2*3,4,3,2*4,5,2*3,7*2,1,3,
     $ 4,3,4,1,5*2,4,2*2,1,2,7,5*0,  5,3,2,17*0,     12,19*0/
      DATA  KATLL/6,4,4,4,6,6,3,21,15,3,3/
      DATA  LWFLAG/0/
C
           ENTRY      W3AI03(LOCRPT,COCBUF)
C
      IF (LWFLAG.EQ.0) THEN
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
      MI   = 43
      KK   =  0
      LVLS =  0
C DETERMINE THE TRUE NUMBER OF BYTES IN THE INPUT REPORT
      DO 100  NCAT = 1,11
         M = KATO(NCAT)
         IF(LOCRPT(M+1).GE.MI)  KK = NCAT
         MI = MAX(MI,LOCRPT(M+1))
  100 CONTINUE
      IF(KK.GT.0)  THEN
         M = KATO(KK)
         LVLS = LOCRPT(M)
      END IF
cvvvvvy2k
cdak  MBYTES = LW * ((MI - 1) + (LVLS * KATLL(KK)))
      MWORDS = (MI - 1) + (LVLS * KATLL(KK))
C TRANSFER LOCRPT TO MOCRPT IN ORDER TO EQUIVALENCE TO REAL AND CHAR.
cdak  CALL XMOVEX(MOCRPT,LOCRPT,MBYTES)
      MOCRPT(1:MWORDS) = LOCRPT(1:MWORDS)
caaaaay2k
C INITIALIZE REPORT ID AS MISSING OR NOT APPLICABLE
      COCBUF(1)       = '9999999999'
      COCBUF(2)(7:10) = '9999'
      COCBUF(3)(8:10) = '999'
      COCBUF(4)(1:7)  = '9999999'
C READ IN LATITUDE FROM WORD 1 (REAL)
C WRITE OUT IN FIRST 5 CHARACTERS OF WORD 1 (C*5)
      M = 1
      N = 1
      IF(ROCRPT(M).LT.XMSG)  THEN
         IF(INT(ROCRPT(M)).GE.0)  WRITE(COCBUF(N)(1:5),50)INT(ROCRPT(M))
         IF(INT(ROCRPT(M)).LT.0)  WRITE(COCBUF(N)(1:5),55)INT(ROCRPT(M))
      END IF
C READ IN LONGITUDE FROM WORD 2 (REAL)
C WRITE OUT IN LAST 5 CHARACTERS OF WORD 1 (C*5)
      M = 2
      IF(ROCRPT(M).LT.XMSG)  THEN
         IF(INT(ROCRPT(M)).GE.0) WRITE(COCBUF(N)(6:10),50)INT(ROCRPT(M))
         IF(INT(ROCRPT(M)).LT.0) WRITE(COCBUF(N)(6:10),55)INT(ROCRPT(M))
      END IF
C READ IN STATION ID FROM WORDS 11 AND 12 (C*8)
C (CHAR. 1-4 OF ID IN WORD 11, CHAR. 5-6 OF ID IN WORD 12, LEFT-JUSTIF.)
C WRITE OUT IN FIRST 6 CHARACTERS OF WORD 2 (C*6)
      M = 11
      N = N + 1
      COCBUF(N)(1:6) = COCRPT(LW2*M-LW1)(1:4)//
     $                 COCRPT(LW2*(M+1)-LW1)(1:2)
C READ IN OBSERVATION TIME FROM WORD 4 (REAL)
C WRITE OUT IN LAST 4 CHARACTERS OF WORD 2 (C*4)
      M = 4
      IF(ROCRPT(M).LT.XMSG)  WRITE(COCBUF(N)(7:10),40)  INT(ROCRPT(M))
C READ IN RESERVED CHARACTERS FROM WORDS 5 AND 6 (C*8)
C  (4 CHAR., LEFT-JUSTIF.)
C WRITE OUT IN FIRST 7 CHARACTERS OF WORD 3 (C*7)
      M = 5
      N = N + 1
      COCBUF(N)(1:7) =COCRPT(LW2*(M+1)-LW1)(1:2)//
     $                COCRPT(LW2*M-LW1)(1:4)//COCRPT(LW2*(M+1)-LW1)(3:3)
C READ IN OFFICE NOTE 29 REPORT TYPE FROM WORD 9 (INTEGER)
C WRITE OUT IN LAST 3 CHARACTERS OF WORD 3 (C*3)
      M = 9
      IF(MOCRPT(M).LT.IMSG)  WRITE(COCBUF(N)(8:10),30)  MOCRPT(M)
C READ IN STATION ELEVATION FROM WORD 7 (REAL)
C WRITE OUT IN FIRST 5 CHARACTERS OF WORD 4 (C*4)
      M = 7
      N = N + 1
      IF(ROCRPT(M).LT.XMSG)  THEN
         IF(INT(ROCRPT(M)).GE.0)  WRITE(COCBUF(N)(1:5),50)INT(ROCRPT(M))
         IF(INT(ROCRPT(M)).LT.0)  WRITE(COCBUF(N)(1:5),55)INT(ROCRPT(M))
      END IF
C READ IN INSTRUMENT TYPE FROM WORD 8 (INTEGER)
C WRITE OUT IN NEXT 2 CHARACTERS OF WORD 4 (C*2)
      M = 8
      IF(MOCRPT(M).LT.99)  WRITE(COCBUF(N)(6:7),20)  MOCRPT(M)
      NO = N
      N  = N + 1
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C        LOOP THROUGH ALL THE CATEGORIES WHICH HAVE VALID DATA
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      DO 3000  NCAT = 1,11
C 'M' IS THE WORD IN MOCRPT WHERE THE NO. OF LEVELS IS READ FROM
         M = KATO(NCAT)
         LVLS = MOCRPT(M)
C 'MI' IS THE STARTING LOCATION IN MOCRPT FOR READING DATA FROM THIS CAT
         MI = MOCRPT(M+1)
         IF(LVLS.EQ.0.OR.MI.EQ.0)  GO TO 3000
C CATEGORY WITH VALID CATEGORY ENCOUNTERED - WRITE OUT IN FIRST 2
C  CHARACTERS OF CATEGORY/COUNTER GROUP FOR THIS CATEGORY (C*2)
         COCBUF(N)(1:2) = KAT(NCAT)
C NUMBER OF LEVELS WRITTEN OUT TO CHAR. 6 & 7 OF CAT/CNTR GROUP (C*2)
         WRITE(COCBUF(N)(6:7),20)  LVLS
         NC = N
         N = N + 1
C NWDSC COUNTS THE NUMBER OF 10-CHAR. WORDS IN THIS CATEGORY
         NWDSC = 1
         I = 1
C***********************************************************************
C         LOOP THROUGH ALL THE LEVELS IN THE CURRENT CATEGORY
C***********************************************************************
         DO 2000  L = 1,LVLS
C NDG IS NO. OF INPUT PARAMETERS PER LEVEL IN THIS CATEGORY
            NDG = KATL(NCAT)
C-----------------------------------------------------------------------
C        LOOP THROUGH ALL THE PARAMETERS IN THE CURRENT LEVEL
C-----------------------------------------------------------------------
            DO 1800  K = 1,NDG
C 'LL' IS THE NUMBER OF OUTPUT CHARACTERS PER PARAMETER FOR THIS CAT.
               LL = KATGL(K,NCAT)
C KATGC IS AN INDICATOR FOR THE INPUT FORMAT OF EACH OUTPUT PARAMETER
C  (=2 - REAL, =1 - INTEGER, =4 - CHARACTER*8)
               IF(KATGC(K,NCAT).EQ.4)  GO TO 1500
C OUTPUT PARAMETER IS MISSING OR NOT APPLICABLE (BASED ON MISSING INPUT)
               IF(KATGC(K,NCAT).EQ.1)  THEN
                  IF(MOCRPT(MI).GE.IMSG)  THEN
                     HOLD(1:LL) = CNINES(1:LL)
C SPECIAL CASE FOR INPUT PARAMETER 15, CAT. 52 -- MISSING IS '0099999'
                     IF(K.EQ.15.AND.NCAT.EQ.9)  HOLD(1:7) = '0099999'
                     GO TO 1750
                  END IF
               ELSE  IF(KATGC(K,NCAT).EQ.2)  THEN
                  IF(ROCRPT(MI).GE.XMSG)  THEN
                     HOLD(1:LL) = CNINES(1:LL)
C SPECIAL CASE FOR INPUT PARAMETER 15, CAT. 52 -- MISSING IS '0099999'
                     IF(K.EQ.15.AND.NCAT.EQ.9)  HOLD(1:7) = '0099999'
                     GO TO 1750
                  END IF
               END IF
               IVALUE = MOCRPT(MI)
               IF(KATGC(K,NCAT).EQ.2)  IVALUE = INT(ROCRPT(MI))
C INITIALIZE ALL OUTPUT PARAMETERS HERE AS MISSING
C  (WILL REMAIN MISSING IF "IVALUE" SOMEHOW WOULD FILL-UP TOO
C   MANY CHARACTERS)
               HOLD(1:LL) = CNINES(1:LL)
               IF(LL.EQ.1)  THEN
C OUTPUT PARAMETER CONSISTS OF ONE CHARACTER
                  IF(IVALUE.LE.9.AND.IVALUE.GE.0)
     $               WRITE(HOLD(1:LL),10)  IVALUE
               ELSE  IF(LL.EQ.2)  THEN
C OUTPUT PARAMETER CONSISTS OF TWO CHARACTERS
                  IF(IVALUE.LE.99.AND.IVALUE.GE.-9)  THEN
                     IF(IVALUE.GE.0)  WRITE(HOLD(1:LL),20)  IVALUE
                     IF(IVALUE.LT.0)  WRITE(HOLD(1:LL),25)  IVALUE
                  END IF
               ELSE  IF(LL.EQ.3)  THEN
C OUTPUT PARAMETER CONSISTS OF THREE CHARACTERS
                  IF(IVALUE.LE.999.AND.IVALUE.GE.-99)  THEN
                     IF(IVALUE.GE.0)  WRITE(HOLD(1:LL),30)  IVALUE
                     IF(IVALUE.LT.0)  WRITE(HOLD(1:LL),35)  IVALUE
                  END IF
               ELSE  IF(LL.EQ.4)  THEN
C OUTPUT PARAMETER CONSISTS OF FOUR CHARACTERS
                  IF(IVALUE.LE.9999.AND.IVALUE.GE.-999)  THEN
                     IF(IVALUE.GE.0)  WRITE(HOLD(1:LL),40)  IVALUE
                     IF(IVALUE.LT.0)  WRITE(HOLD(1:LL),45)  IVALUE
                  END IF
               ELSE  IF(LL.EQ.5)  THEN
C OUTPUT PARAMETER CONSISTS OF FIVE CHARACTERS
                  IF(IVALUE.LE.99999.AND.IVALUE.GE.-9999)  THEN
                     IF(IVALUE.GE.0)  WRITE(HOLD(1:LL),50)  IVALUE
                     IF(IVALUE.LT.0)  WRITE(HOLD(1:LL),55)  IVALUE
                  END IF
               ELSE  IF(LL.EQ.6)  THEN
C OUTPUT PARAMETER CONSISTS OF SIX CHARACTERS
                  IF(IVALUE.LE.999999.AND.IVALUE.GE.-99999)  THEN
                     IF(IVALUE.GE.0)  WRITE(HOLD(1:LL),60)  IVALUE
                     IF(IVALUE.LT.0)  WRITE(HOLD(1:LL),65)  IVALUE
                  END IF
               ELSE  IF(LL.EQ.7)  THEN
C OUTPUT PARAMETER CONSISTS OF SEVEN CHARACTERS
                  IF(IVALUE.LE.9999999.AND.IVALUE.GE.-999999)  THEN
                     IF(IVALUE.GE.0)  WRITE(HOLD(1:LL),70)  IVALUE
                     IF(IVALUE.LT.0)  WRITE(HOLD(1:LL),75)  IVALUE
                  END IF
               END IF
               GO TO 1750
 1500          CONTINUE
C.......................................................................
C INPUT CHARACTER (MARKER) PROCESSING COMES HERE
               IF(LL.LE.4)  THEN
C THERE ARE BETWEEN ONE AND FOUR MARKERS IN OUTPUT PARAMETER
                  HOLD(1:LL) = COCRPT(LW2*MI-LW1)(1:LL)
               ELSE
C THERE ARE MORE THAN FOUR MARKERS IN OUTPUT PARAMETER
                  IP = 1
 1610             CONTINUE
                  JP = IP + 3
                  IF(JP.LT.LL)  THEN
C GET FIRST FOUR MARKERS FROM INPUT WORD
                     HOLD(IP:JP) = COCRPT(LW2*MI-LW1)(1:4)
                     MI = MI + 1
                     IP = JP + 1
                     GO TO 1610
                  ELSE  IF(JP.EQ.LL)  THEN
C GET FOUR REMAINING MARKERS FROM NEXT INPUT WORD
                     HOLD(IP:JP) = COCRPT(LW2*MI-LW1)(1:4)
                  ELSE
C GET ONE, TWO, OR THREE REMAINING MARKERS FROM NEXT INPUT WORD
                     HOLD(IP:LL) = COCRPT(LW2*MI-LW1)(1:LL-JP+4)
                  END IF
               END IF
C.......................................................................
 1750          CONTINUE
C 'I' IS POINTER FOR BEGINNING BYTE IN C*10 WORD FOR OUTPUT PARAMETER
C 'J' IS POINTER FOR ENDING    BYTE IN C*10 WORD FOR OUTPUT PARAMETER
               J = I + LL - 1
               IF(J.GT.10)  THEN
C COME HERE IF OUTPUT PARAMETER SPANS ACROSS TWO C*10 WORDS
                  COCBUF(N)(I:10)     = HOLD(1:11-I)
                  COCBUF(N+1)(1:J-10) = HOLD(12-I:LL)
                  N = N + 1
                  NWDSC = NWDSC + 1
                  I = J - 9
               ELSE
                  COCBUF(N)(I:J) = HOLD(1:LL)
                  I = J + 1
                  IF(I.GE.11)  THEN
                     N = N + 1
                     NWDSC = NWDSC + 1
                     I = 1
                  END IF
               END IF
C GO ON TO NEXT INPUT WORD IN THIS LEVEL
               MI = MI + 1
 1800       CONTINUE
C-----------------------------------------------------------------------
 2000    CONTINUE
C***********************************************************************
C FILL REMAINING PART OF LAST OUTPUT WORD IN THIS CATEGORY WITH X'S
         IF(I.GT.1)  COCBUF(N)(I:10) = FILL(I:10)
C TOTAL NO. CHARACTERS IN CATEGORY (EXCL. FILLS) (NCHAR) WRITTEN OUT TO
C  LAST 3 CHARACTERS OF CATEGORY/COUNTER GROUP (C*3)
         NCHAR = ((NWDSC - 1) * 10) + I - 1
         WRITE(COCBUF(NC)(8:10),30)  NCHAR
         IF(I.GT.1)  N = N + 1
C RELATIVE POSITION IN REPORT OF NEXT CAT/CNTR GROUP (N) WRITTEN OUT TO
C  CHAR. 3 - 5 OF CURRENT CATEGORY/COUNTER GROUP (C*3)
         WRITE(COCBUF(NC)(3:5),30)  N
C GO ON TO THE NEXT CATEGORY
 3000 CONTINUE
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C WRITE OUT THE TOTAL LENGTH OF THE REPORT -- NO. OF 10-CHARACTER WORDS
C  -- (N) IN LAST THREE CHARACTERS OF WORD 4 (C*3)
      WRITE(COCBUF(NO)(8:10),30)  N
C WRITE OUT 'END REPORT' TO LOCATE THE END OF THIS REPORT IN THE BLOCK
      COCBUF(N) = 'END REPORT'
      RETURN
   10 FORMAT(I1.1)
   15 FORMAT(I1.0)
   20 FORMAT(I2.2)
   25 FORMAT(I2.1)
   30 FORMAT(I3.3)
   35 FORMAT(I3.2)
   40 FORMAT(I4.4)
   45 FORMAT(I4.3)
   50 FORMAT(I5.5)
   55 FORMAT(I5.4)
   60 FORMAT(I6.6)
   65 FORMAT(I6.5)
   70 FORMAT(I7.7)
   75 FORMAT(I7.6)
      END
