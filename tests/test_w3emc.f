C> @file
C> @brief Tests writing an index file
C> @author Boi Vuong
C>
C> ABSTRACT: THIS IS A TEST PROGRAM FOR NCEPLIBS-W3EMC.
C>
C> PROGRAM HISTORY LOG:
C>   2021-01-14 BOI VUONG
C>
C> USAGE: testw3emc gribfile indexfile
C>
C> @param[in] gribfile - input file name
C>
C> @param[out] indexfile - output index file
C>
C> EXIT STATES:
C>   COND =   0 - SUCCESSFUL RUN
C>   COND =   1 - GRIB MESSAGE NOT FOUND
C>   COND =   2 - INCORRECT ARGUMENTS
C>   COND =   8 - ERROR ACCESSING FILE
C>
C>
      PROGRAM TESTW3MC
      PARAMETER (MXSIZE=2000000,MXSIZ3=MXSIZE*3)
      PARAMETER (MSK1=32000,MSK2=4000)
      PARAMETER (MBUF=320000*128)
      integer, PARAMETER :: LENHEAD=21
      CHARACTER CGB*256,CGI*256,CARG*300
      CHARACTER CIX1*256,CHEAD(2)*81,CBUF*(MBUF),
     &          CGRIB*40,CIS*8,CPDS*28,CTITLE*86
      CHARACTER LABEL*5,MPDS*120,NPDS*63,EPDS*53,KPDS*24
      INTEGER NARG,IARGC,IARG,CHKIRET,OUTPUT,TOTAL
      INTEGER DUM,JGDS(200),MPDST(200),JREW,MI,ITOT
      INTEGER,DIMENSION(8):: ITIME=(/0,0,0,-500,0,0,0,0/)
      INTEGER,DIMENSION(28) :: HEXPDS
      INTEGER IFLD(MXSIZE),IBDSFL(12),IBMAP(MXSIZE),IDAWIP(200)
      INTEGER KGDS(200),KGDSO(200),KPDST(25)
      INTEGER NPARM,NBUL,PUNUM,MAPNUM(20),NBITS(20)
      CHARACTER BULHED*6,CPARM*100,DESC*20,EOML*3
      CHARACTER * 1 GRIB(MXSIZ3),WMOHDR(LENHEAD)
      CHARACTER * 200 fileb,filei,fileo
      CHARACTER KWBX(4),TITLE(132)
      CHARACTER * 1   PDS(28),PDSL(28),CSEP(80),GDS(400)
      REAL            FLDI(MXSIZE)
C
      LOGICAL         IW3PDS
C
      HEXPDS=0
      DATA  OUTPUT/6/
2100   FORMAT (' INVENTORY FOR FILE = ',A,/)
2300   FORMAT(' PRODUCT DEFINITION SECTION (PDS) OF EACH RECORD.',/,
     &   ' SEE NMC OFFICE NOTE 388 ON GRIB (WMO CODE FM 92-IX EXT.)',/)
2400   FORMAT (/,' END OF INVENTORY FOR FILE = ',A,', ',
     &       I5,' GRIB RECORDS.',/,' FILE NUMBER ',
     &       I2,',',' IT HAS ',I15,' BYTES IN FILE.',/)
2600   FORMAT (' ERROR EXIT W3FP11 = ',I2,1X,',  RECORD  = ',A4,
     & ',  ',1X,A63,' , BYTES  = ',I6)
2800   FORMAT(1X,A5,1X,A,/,' PDS EXT. ',A,/,5X,A25,A18,A2,A12,1X,I9)
2801   FORMAT(1X,A5,1X,A,/,' PDS EXT. ',A18,/,5X,A25,A18,A2,A12,1X,I9)
2802   FORMAT(1X,A5,1X,A,/,6X,A25,A18,A2,A12,1X,I0)
C
      print *, ''
      print *,'*** Testing NCEPLIBS-w3emc. ***'
      print *, ''
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET ARGUMENTS
      IARG=1
      NARG=IARGC()
      IF(NARG.NE.2) THEN
        CALL ERRMSG('testw3emc:  Incorrect usage')
        CALL ERRMSG('Usage: testw3emc gribfile indexfile')
        CALL ERREXIT(2)
      ENDIF
      CALL GETARG(1,CGB)
      NCGB=LEN_TRIM(CGB)
      CALL BAOPENR(11,CGB(1:NCGB),IOS)
      CALL BASETO(1,1)
      IF(IOS.NE.0) THEN
        LCARG=LEN('testw3emc:  Error accessing file '//CGB(1:NCGB))
        CARG(1:LCARG)='testw3emc:  Error accessing file '//CGB(1:NCGB)
        CALL ERRMSG(CARG(1:LCARG))
        CALL ERREXIT(8)
      ENDIF
      CALL GETARG(2,CGI)
      NCGI=LEN_TRIM(CGI)
      CALL BAOPEN(31,CGI(1:NCGI),IOS)
      IF(IOS.NE.0) THEN
        LCARG=LEN('testw3emc:  Error accessing file '//CGI(1:NCGI))
        CARG(1:LCARG)='testw3emc:  Error accessing file '//CGI(1:NCGI)
        CALL ERRMSG(CARG(1:LCARG))
        CALL ERREXIT(8)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE INDEX FILE
      CHKIRET=0
      LUGB=11
      LUGI=0 
      MNUM=0
      CALL GETGIR(11,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRGI)
      CHKIRET=CHKIRET + IRGI
      IF(IRGI.GT.1.OR.NNUM.EQ.0) THEN
        CALL ERRMSG('testw3emc:  No GRIB messages detected in file '
     &              //CGB(1:NCGB))
c        CALL BACLOSE(11,IRET)
        CALL BACLOSE(31,IRET)
        CALL ERREXIT(1)
      ENDIF
      MNUM=MNUM+NNUM
      CALL WRGI1H(31,NLEN,MNUM,CGB(1:NCGB))
      IW=162
      CALL BAWRITE(31,IW,NLEN*NNUM,KW,CBUF)
      IW=IW+NLEN*NNUM
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  EXTEND INDEX FILE IF INDEX BUFFER LENGTH GREATER THAN MBUF
      IF(IRGI.EQ.1) THEN
        DOWHILE(IRGI.EQ.1.AND.NNUM.GT.0)
          CALL GETGIR(11,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRGI)
          CHKIRET=CHKIRET + IRGI
          IF(IRGI.LE.1.AND.NNUM.GT.0) THEN
            MNUM=MNUM+NNUM
            CALL BAWRITE(31,IW,NLEN*NNUM,KW,CBUF)
            IW=IW+NLEN*NNUM
          ENDIF
        ENDDO
        CALL WRGI1H(31,NLEN,MNUM,CGB(1:NCGB))
      ENDIF
      CALL BACLOSE(11,IRET)
      CALL BACLOSE(31,IRET)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      CALL BAOPENR(32,CGI(1:NCGI),IRET)
      IF (NNUM.GT.0) THEN
          NCGRIB=0
          DOWHILE(CGRIB(NCGRIB+1:NCGRIB+1).NE.' ')
            NCGRIB=NCGRIB+1
          ENDDO
          WRITE (OUTPUT,2100) CGI(1:NCGI)
          WRITE (OUTPUT,2300)
          NS    = 0
          TOTAL = 0
          DO N=1,NNUM
            WRITE (LABEL(2:5),FMT='(I4.4)') N
            LABEL(1:1) = CHAR(78)
            CALL GBYTEC(CBUF,LGRIB,8*NS+8*20,8*4)
            TOTAL = TOTAL + LGRIB
            CIS='GRIB'//CBUF(NS+22:NS+25)
            CPDS=CBUF(NS+26:NS+53)
            KPDS=CBUF(NS+113:NS+172)
            CALL W3FP11(CIS,CPDS,CTITLE,IRET)
            IF (IRET.NE.0) CTITLE=' UNKNOWN FIELD'
            WRITE (MPDS(1:64),FMT='(4(8Z2.2))') (CPDS(J:J),J=1,28)
            NPDS(1:8)   = MPDS(1:8)
            NPDS(9:9)   = ' '
            NPDS(10:17) = MPDS(9:16)
            NPDS(18:18) = ' '
            NPDS(19:26) = MPDS(17:24)
            NPDS(27:27) = ' '
            NPDS(28:35) = MPDS(25:32)
            NPDS(36:36) = ' '
            NPDS(37:44) = MPDS(33:40)
            NPDS(45:45) = ' '
            NPDS(46:53) = MPDS(41:48)
            NPDS(54:54) = ' '
            NPDS(55:62) = MPDS(49:56)
            NPDS(63:63) = ' '
C
            IF ( NPDS(57:58) .EQ. '02' ) THEN
               WRITE(MPDS(57:120),FMT='(3(8Z2.2))')(KPDS(J:J),J=1,24)
               EPDS(1:8)   = MPDS(57:64)
               EPDS(9:9)   = ' '
               EPDS(10:17) = MPDS(65:72)
               EPDS(18:18) = ' '
               IF ((NPDS(19:20).EQ.'BF'.OR.NPDS(19:20).EQ.'C0')
     &           .OR.(EPDS(3:4).EQ.'04'.OR.EPDS(3:4).EQ.'05')) THEN
                  EPDS(19:26) = MPDS(73:80)
                  EPDS(27:27) = ' '
                  EPDS(28:35) = MPDS(81:88)
                  EPDS(36:36) = ' '
                  EPDS(37:44) = MPDS(89:96)
                  EPDS(45:45) = ' '
                  EPDS(46:53) = MPDS(97:104)
                  WRITE(OUTPUT,2800)LABEL(1:5),NPDS,EPDS,CTITLE(2:26),
     &            CTITLE(28:45),CTITLE(46:47),CTITLE(51:62),LGRIB
               ELSE
                  WRITE(OUTPUT,2801)LABEL(1:5),NPDS,EPDS,CTITLE(2:26),
     &            CTITLE(28:45),CTITLE(46:47),CTITLE(51:62),LGRIB
               END IF
            ELSE
               WRITE(OUTPUT,2802)LABEL(1:5),NPDS,CTITLE(2:26),
     &         CTITLE(28:45),CTITLE(46:47),CTITLE(51:62),LGRIB
            ENDIF
            NS=NS+NLEN
          ENDDO
      ELSE
          PRINT *,' ... COULD NOT READ INDEX FILE = ',CGI(1:NCGI)
      ENDIF
      WRITE (OUTPUT,2400) CGI(1:NCGI), NNUM, IARG, TOTAL
      CALL BACLOSE(32,IRET)
C
      CALL W3UTCDAT (ITIME)
      JREW    = 0
      JGDS    = -1
      MPDST   = -1
      CPARM = 'KWBC'
      CALL W3AS00(NPARM,CPARM,IER)
      CPARM = 'KWBC'
c
      CALL GETGBP(LUGB,LUGI,MXSIZ3,JREW,MPDST,JGDS,
     &      itot,KREW,KPDST,KGDS,GRIB,IRET)
      print *, ' '
      IF (CHKIRET.EQ.0) THEN
          print *,'Testing NCEPLIBS-w3emc: SUCCESS !'
      ELSE
          print *,'Testing NCEPLIBS-w3emc: FAILED !'
      ENDIF
      print *, ' '
c
      END

C>
C> SUBPROGRAM: WRGI1H         WRITE INDEX HEADERS
C>   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 93-11-22
C>
C> @brief THIS SUBPROGRAM WRITES TWO INDEX HEADERS. CURRENTLY, THE LENGTH OF EACH INDEX RECORD IS 152.
C>
C>
C> USAGE:    CALL WRGI1H(LUGI,NLEN,NNUM,CGB)
C>   INPUT ARGUMENTS:
C>     LUGI         INTEGER LOGICAL UNIT OF OUTPUT INDEX FILE
C>     NLEN         INTEGER LENGTH OF INDEX RECORDS
C>     NNUM         INTEGER NUMBER OF INDEX RECORDS
C>     CGB          CHARACTER NAME OF GRIB FILE
C>
C> SUBPROGRAMS CALLED:
C>   NCBASE       GET BASENAME OF FILE
C>   HOSTNAM      GET SYSTEM NAME
C>   BAWRITE      BYTE-ADDRESSABLE WRITE
C>
C>
C>
      SUBROUTINE WRGI1H(LUGI,NLEN,NNUM,CGB)
      CHARACTER CGB*(*)
      CHARACTER CD8*8,CT10*10,HOSTNAM*15
      CHARACTER CHEAD(2)*81
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL FIRST 81-BYTE HEADER
      NCGB=LEN(CGB)
      NCGB1=NCBASE(CGB,NCGB)
      NCGB2=NCBASE(CGB,NCGB1-2)
      CALL DATE_AND_TIME(CD8,CT10)
      CHEAD(1)='!GFHDR!'
      CHEAD(1)(9:10)=' 1'
      CHEAD(1)(12:14)='  1'
      WRITE(CHEAD(1)(16:20),'(I5)') 162
      CHEAD(1)(22:31)=CD8(1:4)//'-'//CD8(5:6)//'-'//CD8(7:8)
      CHEAD(1)(33:40)=CT10(1:2)//':'//CT10(3:4)//':'//CT10(5:6)
      CHEAD(1)(42:47)='GB1IX1'
      CHEAD(1)(49:54)=CGB(NCGB2:NCGB1-2)
      CHEAD(1)(56:70)="hiiiiiiiiiiiiii"
      CHEAD(1)(72:81)='testw3emc '
      !CHEAD(1)(82:82)=CHAR(10)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL SECOND 83-BYTE HEADER
      CHEAD(2)='IX1FORM:'
      WRITE(CHEAD(2)(9:38),'(3I10)') 162,NLEN,NNUM
      CHEAD(2)(41:81)=CGB(NCGB1:NCGB)
      !CHEAD(2)(82:82)=CHAR(10)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  WRITE HEADERS AT BEGINNING OF INDEX FILE
      CALL BAWRITE(LUGI,0,162,KW,CHEAD)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END


C>
C> SUBPROGRAM: NCBASE         LOCATE BASENAME OF A FILE
C> @author Mark Iredell          ORG: W/NMC23     DATE: 93-11-22
C>
C> @brief THIS SUBPROGRAM LOCATES THE CHARACTER NUMBER AFTER THE LAST IN A CHARACTER STRING.
C> FOR UNIX FILENAMES, THE CHARACTER NUMBER
C> RETURNED MARKS THE BEGINNING OF THE BASENAME OF THE FILE.
C>
C> PROGRAM HISTORY LOG:
C>   93-11-22  IREDELL
C>
C> USAGE:     ...=NCBASE(C,N)
C>   INPUT ARGUMENTS:
C>     C            CHARACTER STRING TO SEARCH
C>     N            INTEGER LENGTH OF STRING
C>
C>
      FUNCTION NCBASE(C,N)
      CHARACTER C*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      K=N
      DOWHILE(K.GE.1.AND.C(K:K).NE.'/')
        K=K-1
      ENDDO
      NCBASE=K+1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
