C> @file
C> @brief Build 40 char communications prefix.
C> @author Bill Cavanaugh @date 1991-06-21

C> Using information from the user, build a 40 character
C> communications prefix and place in indicated location.
C>
C> Program history log:
C> - Bill Cavanaugh 1991-06-21
C> - Ralph Jones 1991-09-20 Changes for silicongraphics 3.3 fortran 77.
C> - Ralph Jones 1993-03-29 Add save statement.
C> - Ralph Jones 1994-04-28 Change for cray 64 bit word size and
C> for ASCII character set computers.
C> - Boi Vuong 2002-10-15 Replaced function ichar with mova2i.
C>
C> @param[in] ICAT Catalog number.
C> @param[in] AREG AFOS regional addressing flags (6 positions)
C> select any or all of the following. Selections
C> will automatically be left justified and blank
C> filled to 6 positions.
C> If bulletins and/or messages are not to be routed
C> to AFOS, then leave the field filled with blanks.
C> - E - Eastern region
C> - C - Central region
C> - W - Western region
C> - S - Southern region
C> - A - Atlantic region
C> - P - Pacific region
C> @param[in] IERR Error return.
C> @param[in] IBCKUP Backup indicator w/header key
C> - 0 = Not a backup.
C> - 1 = FD backup.
C> - 2 = DF backup.
C>  - Back up is only permitted for KU and KU bulletins.
C> @param[in] IDATYP Data type indicator.
C> - 0  = EBCIDIC data.
C> - 11 = Binary data.
C> - 12 = Psuedo-ASCII data.
C> - 3  = ASCII data.
C> @param[out] LOC Name of the array to receive the communications prefix.
C>
C> @note Error returns
C> IERR:
C> - = 0 Normal return.
C> - = 1 Incorrect backup flag.
C> - = 2 A regional addressing flag is non-blank and non-standard entry.
C> - = 3 Data type is non-standard entry.
C>
C> @author Bill Cavanaugh @date 1991-06-21
      SUBROUTINE W3FI61 (LOC,ICAT,AREG,IBCKUP,IDATYP,IERR)
      INTEGER        LOC(*)
      INTEGER        ICAT,IBCKUP,IDATYP
      INTEGER        IERR,IHOLD
C
      CHARACTER*6    AREG
      CHARACTER*8    AHOLD
      CHARACTER*6    ARGNL
      CHARACTER*1    BLANK
C
      LOGICAL        IBM370
C
      EQUIVALENCE    (IHOLD,AHOLD)
C
      SAVE
C
      DATA  ARGNL /'ECWSAP'/
C
C     BLANK WILL BE 40 HEX OR DECIMAL 64 ON AN IBM370 TYPE
C     COMPUTER, THIS IS THE EBCDIC CHARACTER SET.
C     BLANK WILL BE 20 HEX OR DECIMAL 32 ON A COMPUTER WITH THE
C     ASCII CHARACTER SET. THIS WILL BE USED TO TEST FOR CHARACTER
C     SETS TO FIND IBM370 TYPE COMPUTER.
C
      DATA  BLANK /' '/
      DATA  IBM370/.FALSE./
C
C  ----------------------------------------------------------------
C
C     TEST FOR CRAY 64 BIT COMPUTER, LW = 8
C
      CALL W3FI01(LW)
C
C     TEST FOR EBCDIC CHARACTER SET
C
      IF (MOVA2I(BLANK).EQ.64) THEN
        IBM370 = .TRUE.
      END IF
C
      IERR      = 0
      INOFST    = 0
C BYTE  1                    SOH -  START OF HEADER
      CALL SBYTE (LOC,125,INOFST,8)
      INOFST    = INOFST + 8
C BYTE  2                    TRANSMISSION PRIORITY
      CALL SBYTE (LOC,1,INOFST,8)
      INOFST    = INOFST + 8
C BYTE  3-7                  CATALOG NUMBER
      IF (ICAT.GT.0) THEN
        IF (LW.EQ.4) THEN
          KK        = ICAT / 10
          CALL W3AI15 (KK,IHOLD,1,4,'-')
          IF (.NOT.IBM370) CALL W3AI39(IHOLD,4)
          CALL SBYTE (LOC,IHOLD,INOFST,32)
          INOFST    = INOFST + 32
          KK        = MOD(ICAT,10)
          CALL W3AI15 (KK,IHOLD,1,4,'-')
          IF (.NOT.IBM370) CALL W3AI39(IHOLD,4)
          CALL SBYTE (LOC,IHOLD,INOFST,8)
          INOFST    = INOFST + 8
        ELSE
          CALL W3AI15 (ICAT,IHOLD,1,8,'-')
          IF (.NOT.IBM370) CALL W3AI39(IHOLD,8)
          CALL SBYTE (LOC,IHOLD,INOFST,40)
          INOFST    = INOFST + 40
        END IF
      ELSE
          CALL SBYTE (LOC,-252645136,INOFST,32)
          INOFST    = INOFST + 32
          CALL SBYTE (LOC,240,INOFST,8)
          INOFST    = INOFST + 8
      END IF
C BYTE  8-9-10              BACK-UP FLAG FOR FD OR DF BULLETINS
C                                    0 = NOT A BACKUP
C                                    1 = FD
C                                    2 = DF
      IF (IBCKUP.EQ.0) THEN
C                              NOT A BACKUP
          CALL SBYTE (LOC,4210752,INOFST,24)
          INOFST    = INOFST + 24
      ELSE IF (IBCKUP.EQ.1) THEN
C                              BACKUP FOR FD
          CALL SBYTE (LOC,12764868,INOFST,24)
          INOFST    = INOFST + 24
      ELSE IF (IBCKUP.EQ.2) THEN
C                              BACKUP FOR DF
          CALL SBYTE (LOC,12764358,INOFST,24)
          INOFST    = INOFST + 24
      END IF
C BYTE  11                   BLANK
      CALL SBYTE (LOC,64,INOFST,8)
      INOFST    = INOFST + 8
C BYTE  12                   DATA TYPE
      IF (IDATYP.EQ.0) THEN
      ELSE IF (IDATYP.EQ.11) THEN
      ELSE IF (IDATYP.EQ.12) THEN
      ELSE IF (IDATYP.EQ.3) THEN
      ELSE
          IERR  = 3
          RETURN
      END IF
      CALL SBYTE (LOC,IDATYP,INOFST,8)
      INOFST    = INOFST + 8
C BYTES 13-18                AFOS REGIONAL ADDRESSING FLAGS
      CALL SBYTE (LOC,1077952576,INOFST,32)
      INOFST    = INOFST + 32
      CALL SBYTE (LOC,1077952576,INOFST,16)
      KRESET    = INOFST + 16
      INOFST    = INOFST - 32
      DO 1000 J = 1, 6
          DO 900 K = 1, 6
              IF (AREG(J:J).EQ.ARGNL(K:K)) THEN
C                 PRINT *,AREG(J:J),ARGNL(K:K),' MATCH'
                  IHOLD    = 0
                  IF (LW.EQ.4) THEN
                    AHOLD(4:4) = AREG(J:J)
                    IF (.NOT.IBM370) CALL W3AI39(IHOLD,4)
                  ELSE
                    AHOLD(8:8) = AREG(J:J)
                    CALL W3AI39(IHOLD,8)
                  END IF
                  CALL SBYTE (LOC,IHOLD,INOFST,8)
                  INOFST   = INOFST + 8
                  GO TO 1000
              ELSE IF (AREG(J:J).EQ.' ') THEN
C                 PRINT *,'BLANK SOURCE '
                  GO TO 1000
              END IF
  900     CONTINUE
          IERR  = 2
          RETURN
 1000 CONTINUE
      INOFST   = KRESET
C BYTES 19-39                UNUSED (SET TO BLANK)
      DO 1938 I = 1, 20, 4
          CALL SBYTE (LOC,1077952576,INOFST,32)
          INOFST    = INOFST + 32
 1938 CONTINUE
C BYTE  39                   MUST BE A BLANK
      CALL SBYTE (LOC,64,INOFST,8)
          INOFST    = INOFST + 8
C BYTE  40                   MUST BE A BLANK
      CALL SBYTE (LOC,64,INOFST,8)
C  ----------------------------------------------------------------
      RETURN
      END
