C> @file
C> @brief Translates arg line from standard input
C> @author Dennis Keyser @date 2002-02-11

C> Reads argument lines from standard input and obtains subdirectory, bufr
C> tankname, characters to append for adding an orbit, and options for limiting
C> the time window.
C>
C> ### Program History Log:
C> Date | Programmer | Comment
C> -----|------------|--------
C> 1996-09-03 | B. KATZ   | Original author
C> 1998-11-27 | B. KATZ   | Changes for y2k and fortran 90 compliance
C> 2002-02-11 | D. KEYSER | If "tlflag" is not specified, it defaults to
C> "notimlim" rather than "timlim" and gross time limits will not be
C> calculated and returned in "iymdhb" and "iymdhe"
C>
C> @param[in] SUBDIR Name of sub-directory including bufr data type where
C> bufr data tank is located.
C> @param[in] LSUBDR Number of characters in 'subdir'.
C> @param[in] TANKID Name of file including bufr data sub-type containing
C> bufr data tank.
C> @param[in] LTNKID Number of characters in 'tankid'.
C> @param[in] APPCHR Characters to be appended to 'tankid' giving a
C> uniquely named file to contain the original tank
C> with one orbit appended to it.
C> @param[in] LAPCHR Number of characters in 'appchr'.
C> @param[in] TLFLAG 8 character flag indicating whether time acceptance
C> checks atre to be performed.
C> = 'timlim  ' : perform time acceptance checks.
C> = 'notimlim' : do not perform time acceptance checks.
C>                jdate and kdate are disregarded.
C> @param[in] IYMDHB Start of time acceptance window, in form yyyymmddhh.
C> @param[in] IYMDHE End of time acceptance window, in form yyyymmddhh.
C> @param IERR
C>
C> Input files :
C> unit 05  - standard input for passing in arguments. arguments
C> (for list-directed i/o) are as follows :
C> record 1 - (1) subdirectory. contains bufr data type
C>            (2) tankfile. contains bufr data sub-type
C>            (3) append characters. appended to tankfile
C>                to give unique output file name.
C>            (4) date in yyyymmddhh format.
C> next three records are optional :
C> record 2 - (1) time limit flag. may be either
C>                'timlim  ' or 'notimlim'.  see
C>                description of 'tlflag' above.
C>                (default is 'notimlim')
C> record 3 - (1) hours before current time.
C> record 4 - (1) hours after current time.
C> if 'timlim  ' is specified in record 2, the
C> quantities in records 3 and 4 are used to
C> compute the limits of the time acceptance window.
C> if records 3 and 4 are omitted, the values
C> default to -48 (48 hours before current time)
C> and +12 (12 hours after current time).
C> if 'notimlim  ' is specified in record 2, then
C> these quantities are not used regardless of whether
C> or not they were specified.
C>
C> @author Dennis Keyser @date 2002-02-11
      SUBROUTINE W3TRNARG(SUBDIR,LSUBDR,TANKID,LTNKID,APPCHR,LAPCHR,
     1                    TLFLAG,IYMDHB,IYMDHE,IERR)
      CHARACTER*(*) SUBDIR,TANKID,APPCHR,TLFLAG
      INTEGER IDATIN(8),IDTOUT(8)
      REAL TIMINC(5)
      READ(5,*,END=9999) SUBDIR,TANKID,APPCHR,IYMDH
      MSUBDR = LEN(SUBDIR)
      DO LSUBDR=0,MSUBDR-1
        IF(SUBDIR(LSUBDR+1:LSUBDR+1).EQ.' ') GO TO 10
      ENDDO
      LSUBDR = MSUBDR
   10 CONTINUE
      IF(LSUBDR.LT.4) THEN
        WRITE(6,'(1X,I2,'' CHARACTERS IN SUBDIRECTORY ARGUMENT'',
     1            '' AT LEAST 4 CHARACTERS ARE REQUIRED'')') LSUBDR
        IERR = 2
        RETURN
      ENDIF
      MTNKID = LEN(TANKID)
      DO LTNKID=0,MTNKID-1
        IF(TANKID(LTNKID+1:LTNKID+1).EQ.' ') GO TO 20
      ENDDO
      LTNKID = MTNKID
   20 CONTINUE
      IF(LTNKID.LT.4) THEN
        WRITE(6,'(1X,I2,'' CHARACTERS IN TANKFILE ARGUMENT'',
     1            '' AT LEAST 4 CHARACTERS ARE REQUIRED'')') LTNKID
        IERR = 2
        RETURN
      ENDIF
      MAPCHR = LEN(APPCHR)
      DO LAPCHR=0,MAPCHR-1
        IF(APPCHR(LAPCHR+1:LAPCHR+1).EQ.' ') GO TO 30
      ENDDO
      LAPCHR = MAPCHR
   30 CONTINUE
      TLFLAG = 'NOTIMLIM'  ! The default is to NOT perform time checks
      READ(5,*,END=40) TLFLAG
   40 CONTINUE
      IF(TLFLAG(1:6).NE.'TIMLIM') THEN
        TLFLAG = 'NOTIMLIM'
        PRINT 123, IYMDH,SUBDIR(1:LSUBDR),TANKID(1:LTNKID)
  123   FORMAT(/'RUN ON ',I10/'WRITE TO ',A,'/',A/'GROSS TIME LIMIT ',
     1   'CHECKS ARE NOT PERFORMED HERE - SUBSEQUENT PROGRAM ',
     1   'BUFR_TRANJB WILL TAKE CARE OF THIS'/)
        IYMDHB = 0000000000
        IYMDHE = 2100000000
        IERR = 0
        RETURN
      ENDIF
      TLFLAG(7:8) = '  '
      READ(5,*,END=60) IHRBEF
      GO TO 70
   60 CONTINUE
      IHRBEF = -48
      IHRAFT = 12
      GO TO 100
   70 CONTINUE
      READ(5,*,END=80) IHRAFT
      GO TO 90
   80 CONTINUE
      IHRAFT = 12
      GO TO 100
   90 CONTINUE
      IF(IHRBEF.GT.0 .AND. IHRAFT.LT.0) THEN
        ITEMP = IHRBEF
        IHRBEF = IHRAFT
        IHRAFT = ITEMP
      ELSE IF(IHRBEF.GT.0) THEN
        IHRBEF = -1 * IHRBEF
      ENDIF
  100 CONTINUE
      IDATIN(1) = IYMDH / 1000000
      IDATIN(2) = MOD(IYMDH,1000000) / 10000
      IDATIN(3) = MOD(IYMDH,10000) / 100
      IDATIN(4) = 0
      IDATIN(5) = MOD(IYMDH,100)
      IDATIN(6:8) = 0
      TIMINC(1) = 0.0
      TIMINC(2) = FLOAT(IHRBEF)
      TIMINC(3:5) = 0.0
      CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
      IYMDHB = ((IDTOUT(1) * 100 + IDTOUT(2)) * 100 + IDTOUT(3)) *
     1           100 + IDTOUT(5)
      TIMINC(2) = FLOAT(IHRAFT)
      CALL W3MOVDAT(TIMINC,IDATIN,IDTOUT)
      IYMDHE = ((IDTOUT(1) * 100 + IDTOUT(2)) * 100 + IDTOUT(3)) *
     1           100 + IDTOUT(5)
      PRINT 124, IYMDH,SUBDIR(1:LSUBDR),TANKID(1:LTNKID),IYMDHB,IYMDHE
  124 FORMAT(/'RUN ON ',I10/'WRITE TO ',A,'/',A/'ACCEPT BETWEEN ',I10,
     1 ' AND ',I10/)
      IERR = 0
      RETURN
 9999 CONTINUE
      WRITE(6,'('' INSUFFICIENT NO. OF ARGUMENTS TO BUFR '',
     1          ''TRANSLATION PROCEDURE - AT LEAST 4 ARE NEEDED'')')
      IERR = 1
      RETURN
      END
