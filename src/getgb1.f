C> @file
C> @brief Find and unpacks a grib message.
C> @author Mark Iredell @date 1994-04-01

C> Find and unpack a grib message.
C> Read an associated grib index file (unless it already was read).
C> Find in the index file a reference to the grib message requested.
C> The grib message request specifies the number of messages to skip
C> and the unpacked pds and gds parameters. (A requested parameter
C> of -1 means to allow any value of this parameter to be found.)
C> If the requested grib message is found, then it is read from the
C> grib file and unpacked. Its message number is returned along with
C> the unpacked pds and gds parameters, the unpacked bitmap (if any),
C> and the unpacked data. If the grib message is not found, then the
C> return code will be nonzero.
C>
C> Program history log:
C>  - Mark Iredell 1994-04-01
C>  - Ralph Jones 1995-05-10  Add one more parameter to getgb and
C> change name to getgb1.
C>
C> @param[in] lugb logical unit of the unblocked grib data file.
C> @param[in] lugi logical unit of the unblocked grib index file.
C> @param[in] jf integer maximum number of data points to unpack.
C> @param[in] j integer number of messages to skip (=0 to search from beginning)
C> (<0 to reopen index file and search from beginning).
C> @param[in] jpds integer (25) pds parameters for which to search
C> (=-1 for wildcard) look in doc block of w3fi63 for array kpds
C> for list of order of unpacked pds values.
C> In most cases you only need to set 4 or 5 values to pick up record.
C> @param[in] jgds integer (22) gds parameters for which to search
C> (only searched if jpds(3)=255) (=-1 for wildcard).
C> @param[out] grib Grib data array before it is unpacked.
C> @param[out] kf Integer number of data points unpacked.
C> @param[out] k Integer message number unpacked
C> (can be same as j in calling program
C> in order to facilitate multiple searches).
C> @param[out] kpds Integer (25) unpacked pds parameters.
C> @param[out] kgds Integer (22) unpacked gds parameters.
C> @param[out] lb Logical (kf) unpacked bitmap if present.
C> @param[out] f Real (kf) unpacked data.
C> @param[out] iret Integer return code.
C>  - 0 All ok.
C>  - 96 Error reading index file.
C>  - 97 Error reading grib file.
C>  - 98 Number of data points greater than jf.
C>  - 99 Request not found.
C>  - other w3fi63 grib unpacker return code.
C>
C> @author Mark Iredell @date 1994-04-01
      SUBROUTINE GETGB1(LUGB,LUGI,JF,J,JPDS,JGDS,
     &                       GRIB,KF,K,KPDS,KGDS,LB,F,IRET)
C
      PARAMETER (MBUF=8192*128)
      PARAMETER (LPDS=23,LGDS=22)
C
      INTEGER      JPDS(25),JGDS(*),KPDS(25),KGDS(*)
      INTEGER      IPDSP(LPDS),JPDSP(LPDS),IGDSP(LGDS)
      INTEGER      JGDSP(LGDS)
      INTEGER      KPTR(20)
C
      LOGICAL      LB(*)
C
      REAL         F(*)
C
      CHARACTER    CBUF(MBUF)
      CHARACTER*81 CHEAD(2)
      CHARACTER*1  CPDS(28)
      CHARACTER*1  CGDS(42)
      CHARACTER*1  GRIB(*)
C
C     SAVE LUX,NSKP,NLEN,NNUM,CBUF
      SAVE
C
      DATA LUX/0/
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ INDEX FILE
      IF(J.LT.0.OR.LUGI.NE.LUX) THEN
C        REWIND LUGI
C        READ(LUGI,fmt='(2A81)',IOSTAT=IOS) CHEAD
        CALL BAREAD(LUGI,0,162,ios,chead)
        IF(IOS.EQ.162.AND.CHEAD(1)(42:47).EQ.'GB1IX1') THEN
          LUX=0
          READ(CHEAD(2),'(8X,3I10,2X,A40)',IOSTAT=IOS) NSKP,NLEN,NNUM
          IF(IOS.EQ.0) THEN
            NBUF=NNUM*NLEN
            IF(NBUF.GT.MBUF) THEN
              PRINT *,'GETGB1: INCREASE BUFFER FROM ',MBUF,' TO ',NBUF
              NNUM=MBUF/NLEN
              NBUF=NNUM*NLEN
            ENDIF
            CALL BAREAD(LUGI,NSKP,NBUF,LBUF,CBUF)
            IF(LBUF.EQ.NBUF) THEN
              LUX=LUGI
              J=MAX(J,0)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH FOR REQUEST
      LGRIB=0
      KJ=J
      K=J
      KF=0
      IF(J.GE.0.AND.LUGI.EQ.LUX) THEN
        LPDSP=0
        DO I=1,LPDS
          IF(JPDS(I).NE.-1) THEN
            LPDSP=LPDSP+1
            IPDSP(LPDSP)=I
            JPDSP(LPDSP)=JPDS(I)
          ENDIF
        ENDDO
        LGDSP=0
        IF(JPDS(3).EQ.255) THEN
          DO I=1,LGDS
            IF(JGDS(I).NE.-1) THEN
              LGDSP=LGDSP+1
              IGDSP(LGDSP)=I
              JGDSP(LGDSP)=JGDS(I)
            ENDIF
          ENDDO
        ENDIF
        IRET=99
        DOWHILE(LGRIB.EQ.0.AND.KJ.LT.NNUM)
          KJ=KJ+1
          LT=0
          IF(LPDSP.GT.0) THEN
            CPDS=CBUF((KJ-1)*NLEN+26:(KJ-1)*NLEN+53)
            KPTR=0
            CALL GBYTE(CBUF,KPTR(3),(KJ-1)*NLEN*8+25*8,3*8)
            CALL FI632(CPDS,KPTR,KPDS,IRET)
            DO I=1,LPDSP
              IP=IPDSP(I)
              LT=LT+ABS(JPDS(IP)-KPDS(IP))
            ENDDO
          ENDIF
          IF(LT.EQ.0.AND.LGDSP.GT.0) THEN
            CGDS=CBUF((KJ-1)*NLEN+54:(KJ-1)*NLEN+95)
            KPTR=0
            CALL FI633(CGDS,KPTR,KGDS,IRET)
            DO I=1,LGDSP
              IP=IGDSP(I)
              LT=LT+ABS(JGDS(IP)-KGDS(IP))
            ENDDO
          ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ AND UNPACK GRIB DATA
          IF(LT.EQ.0) THEN
            CALL GBYTE(CBUF,LSKIP,(KJ-1)*NLEN*8,4*8)
            CALL GBYTE(CBUF,LGRIB,(KJ-1)*NLEN*8+20*8,4*8)
            CGDS=CBUF((KJ-1)*NLEN+54:(KJ-1)*NLEN+95)
            KPTR=0
            CALL FI633(CGDS,KPTR,KGDS,IRET)
C  BSM      IF(LGRIB.LE.200+17*JF/8.AND.KGDS(2)*KGDS(3).LE.JF) THEN
C  Change number of bits that can be handled to 25
            IF(LGRIB.LE.200+25*JF/8.AND.KGDS(2)*KGDS(3).LE.JF) THEN
              CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
              IF(LREAD.EQ.LGRIB) THEN
                CALL W3FI63(GRIB,KPDS,KGDS,LB,F,KPTR,IRET)
                IF(IRET.EQ.0) THEN
                  K=KJ
                  KF=KPTR(10)
                ENDIF
              ELSE
                IRET=97
              ENDIF
            ELSE
              IRET=98
            ENDIF
          ENDIF
        ENDDO
      ELSE
        IRET=96
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
