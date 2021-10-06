C> @file
C> @brief Updating office note 85 date/time word.
C> @author Ralph Jones @date 1987-02-09

C> Updates or backdates a fullword date/time word (o.n. 84) by a specified
C> number of hours.
C>
C> ### Program History Log:
C> Date | Programmer | Comments
C> -----|------------|---------
C> Unknown | Robert Allard | Initial.
C> 1987-02-19 | Ralph Jones | Clean up code
C> 1987-02-19 | Ralph Jones | Change to microsoft fortran 4.10
C> 1989-05-12 | Ralph Jones | Correct order of bytes in date word for pc
C> 1989-08-04 | Ralph Jones | Clean up code, get rid of assign, correction for memory set to indefinite.
C> 1989-10-25 | Ralph Jones | Change to cray cft77 fortran
C> 1995-11-15 | Ralph Jones | Add save statement
C> 2002-10-15 | Boi Vuong | Replaced function ichar with mova2i
C>
C> @param[in] IDATE Packed binary date/time as follows:
C> Byte | Variable | Range
C> -----|----------|------
C> Byte 1 | is year of century | 00-99
C> Byte 2 | is month           | 01-12
C> Byte 3 | is day of month    | 01-31
C> Byte 4 | is hour            | 00-23
C> Subroutine takes advantage of fortran address passing, IDATE and NDATE may
C> be a character*1 array of four, the left 32 bits of 64 bit integer word.
C> An office note 85 label can be stored in 4 integer words. If integer the
C> 2nd word is used. Output is stored in left 32 bits. for a office note 84
C> label the 7th word is in the 4th cray 64 bit integer, the left 32 bits.
C> @param[in] JTAU Number of hours to update (if positive) or backdate (if negative)
C> @param[out] NDATE New date/time word returned in the same format as 'IDATE'.
C> 'NDATE' and 'IDATE' may be the same variable.
C>
C> @note This routine is valid only for the 20th century.
C>
C> @note The format of the date/time word is the same as the seventh word of
C> the packed data field label (see o.n. 84) and the third word of a binary
C> data set label (see o.n. 85).
C>
C> Exit states: An error found by out of range tests on the given date/time
C> information will be indicated by returning a binary zero word in 'NDATE'.
C>
C> @author Ralph Jones @date 1987-02-09
      SUBROUTINE W3FS15(IDATE,JTAU,NDATE)
C
      INTEGER     ITABYR(13)
      INTEGER     LPTB(13)
      INTEGER     NOLPTB(13)
C
      CHARACTER*1 IDATE(4)
      CHARACTER*1 NDATE(4)
C
      SAVE
C
      DATA  LPTB  /0000,0744,1440,2184,2904,3648,4368,5112,
     &             5856,6576,7320,8040,8784/
      DATA  NOLPTB/0000,0744,1416,2160,2880,3624,4344,5088,
     &             5832,6552,7296,8016,8760/
      DATA  ICENTY/1900/
C
C     ...WHERE ICENTY IS FOR THE 20TH CENTURY ASSUMED FOR THE GIVEN
C     ...                 YEAR WITHIN THE CENTURY
C
      IYR    = MOVA2I(IDATE(1))
      IMONTH = MOVA2I(IDATE(2))
      IDAY   = MOVA2I(IDATE(3))
      IHOUR  = MOVA2I(IDATE(4))
C
      IF (IYR    .GT. 99) GO TO 1600
      IF (IMONTH .LE.  0) GO TO 1600
      IF (IMONTH .GT. 12) GO TO 1600
      IF (IDAY   .LE.  0) GO TO 1600
      IF (IDAY   .GT. 31) GO TO 1600
      IF (IHOUR  .LT.  0) GO TO 1600
      IF (IHOUR  .GT. 24) GO TO 1600
      IF (JTAU   .NE.  0) GO TO 100
C
        NDATE(1) = IDATE(1)
        NDATE(2) = IDATE(2)
        NDATE(3) = IDATE(3)
        NDATE(4) = IDATE(4)
        RETURN
C
  100 CONTINUE
        JAHR  = IYR + ICENTY
        KABUL = 1
        GO TO 900
C
C     ...WHERE 900 IS SUBROUTINE TO INITIALIZE ITABYR
C     ...AND RETURN THRU KABUL
C
  200 CONTINUE
        IHRYR  = IHOUR + 24 * (IDAY - 1) + ITABYR(IMONTH)
        IHRYR2 = IHRYR + JTAU
C
C     ...TO TEST FOR BACKDATED INTO PREVIOUS YEAR...
C
  300 CONTINUE
        IF (IHRYR2 .LT. 0) GO TO 700
C
      DO  400  M = 2,13
        IF (IHRYR2 .LT. ITABYR(M)) GO TO 600
  400 CONTINUE
C
C     ...IF IT FALLS THRU LOOP TO HERE, IT IS INTO NEXT YEAR...
C
        JAHR   = JAHR   + 1
        IHRYR2 = IHRYR2 - ITABYR(13)
        KABUL  = 2
        GO TO 900
C
  600 CONTINUE
        MONAT  = M      - 1
        IHRMO  = IHRYR2 - ITABYR(MONAT)
        NODAYS = IHRMO  / 24
        ITAG   = NODAYS + 1
        IUHR   = IHRMO  - NODAYS * 24
        GO TO 1500
C
C     ...ALL FINISHED.  RETURN TO CALLING PROGRAM.......................
C     ...COMES TO 700 IF NEG TOTAL HRS. BACK UP INTO PREVIOUS YEAR
C
  700 CONTINUE
        JAHR  = JAHR - 1
        KABUL = 3
        GO TO 900
C
C     ...WHICH IS CALL TO INITIALIZE ITABYR AND RETURN THRU KABUL
C
  800 CONTINUE
        IHRYR2 = ITABYR(13) + IHRYR2
        GO TO 300
C
C     ...SUBROUTINE INITYR...
C     ...CALLED BY GO TO 900 AFTER ASSIGNING RETURN NO. TO KABUL...
C     ...ITABYR HAS MONTHLY ACCUMULATING TOTAL HRS REL TO BEGIN OF YR.
C     ...DEPENDS ON WHETHER JAHR IS LEAP YEAR OR NOT.
C
  900 CONTINUE
        IQUOT  = JAHR / 4
        IRMNDR = JAHR - 4 * IQUOT
        IF (IRMNDR .NE. 0) GO TO 1000
C
C     ...WAS MODULO 4, SO MOST LIKELY A LEAP YEAR,
C
        IQUOT  = JAHR / 100
        IRMNDR = JAHR - 100 * IQUOT
        IF (IRMNDR .NE. 0) GO TO 1200
C
C     ...COMES THIS WAY IF A CENTURY YEAR...
C
        IQUOT  = JAHR / 400
        IRMNDR = JAHR - 400 * IQUOT
        IF (IRMNDR .EQ. 0) GO TO 1200
C
C     ...COMES TO 1000 IF NOT A LEAP YEAR...
C
 1000 CONTINUE
      DO  1100  I = 1,13
        ITABYR(I) = NOLPTB(I)
 1100 CONTINUE
      GO TO 1400
C
C     ...COMES TO 1200 IF LEAP YEAR
C
 1200 CONTINUE
      DO  1300  I = 1,13
        ITABYR(I) = LPTB(I)
 1300 CONTINUE
C
 1400 CONTINUE
        GO TO (200,300,800) KABUL
C
 1500 CONTINUE
        JAHR     = MOD(JAHR,100)
        NDATE(1) = CHAR(JAHR)
        NDATE(2) = CHAR(MONAT)
        NDATE(3) = CHAR(ITAG)
        NDATE(4) = CHAR(IUHR)
        RETURN
C
 1600 CONTINUE
        NDATE(1) = CHAR(0)
        NDATE(2) = CHAR(0)
        NDATE(3) = CHAR(0)
        NDATE(4) = CHAR(0)
C
C     ...WHICH FLAGS AN ERROR CONDITION ...
C
      RETURN
      END
