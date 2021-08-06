C> @file
C> @brief Find a grib message.
C> @author Mark Iredell @date 1995-10-31

C> Find a grib message.
C> Find in the index file a reference to the grib message requested.
C> The grib message request specifies the number of messages to skip
c> and the unpacked pds and gds parameters. (A requested parameter
c> of -1 means to allow any value of this parameter to be found.)
C>
C> Program history log:
C> - Mark Iredell 1995-10-31
C> - Mark Iredell 2001-06-05 Apply linux port by ebisuzaki.
C>
C> @param[in] CBUF Character*1 (nlen*nnum) buffer containing index data.
C> @param[in] NLEN Integer length of each index record in bytes.
C> @param[in] NNUM Integer number of index records.
C> @param[in] J Integer number of messages to skip
c> (=0 to search from beginning).
C> @param[in] JPDS Integer (200) pds parameters for which to search
c> (=-1 for wildcard).
C> @param[in] JGDS Integer (200) gds parameters for which to search
c> (only searched if jpds(3)=255) (=-1 for wildcard).
C> @param[in] JENS Integer (200) ensemble pds parms for which to search
c> (only searched if jpds(23)=2) (=-1 for wildcard).
C> @param[out] K Integer message number found
c> (can be same as j in calling program in order to facilitate multiple searches).
C> @param[out] KPDS Integer (200) unpacked pds parameters.
C> @param[out] KGDS Integer (200) unpacked gds parameters.
C> @param[out] KENS Integer (200) unpacked ensemble pds parms.
C> @param[out] LSKIP Integer number of bytes to skip.
C> @param[out] LGRIB Integer number of bytes to read.
C> @param[out] IRET Integer return code.
C> - 0 All ok.
C> - 1 Request not found.
C>
C> @note Subprogram can be called from a multiprocessing environment.
C> This subprogram is intended for private use by getgb routines only.
C>
C> @author Mark Iredell @date 1995-10-31
C-----------------------------------------------------------------------
      SUBROUTINE GETGB1S(CBUF,NLEN,NNUM,J,JPDS,JGDS,JENS,
     &                   K,KPDS,KGDS,KENS,LSKIP,LGRIB,IRET)
      CHARACTER CBUF(NLEN*NNUM)
      INTEGER JPDS(200),JGDS(200),JENS(200)
      INTEGER KPDS(200),KGDS(200),KENS(200)
      PARAMETER(LPDS=23,LGDS=22,LENS=5)     ! ACTUAL SEARCH RANGES
      CHARACTER CPDS(400)*1,CGDS(400)*1
      INTEGER KPTR(200)
      INTEGER IPDSP(LPDS),JPDSP(LPDS)
      INTEGER IGDSP(LGDS),JGDSP(LGDS)
      INTEGER IENSP(LENS),JENSP(LENS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPRESS REQUEST LISTS
      K=J
      LSKIP=0
      LGRIB=0
      IRET=1
C  COMPRESS PDS REQUEST
      LPDSP=0
      DO I=1,LPDS
        IF(JPDS(I).NE.-1) THEN
          LPDSP=LPDSP+1
          IPDSP(LPDSP)=I
          JPDSP(LPDSP)=JPDS(I)
        ENDIF
      ENDDO
C  COMPRESS GDS REQUEST
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
C  COMPRESS ENS REQUEST
      LENSP=0
      IF(JPDS(23).EQ.2) THEN
        DO I=1,LENS
          IF(JENS(I).NE.-1) THEN
            LENSP=LENSP+1
            IENSP(LENSP)=I
            JENSP(LENSP)=JENS(I)
          ENDIF
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH FOR REQUEST
      DOWHILE(IRET.NE.0.AND.K.LT.NNUM)
        K=K+1
        LT=0
C  SEARCH FOR PDS REQUEST
        IF(LPDSP.GT.0) THEN
          CPDS=CHAR(0)
          CPDS(1:28)=CBUF((K-1)*NLEN+26:(K-1)*NLEN+53)
          NLESS=MAX(184-NLEN,0)
          CPDS(29:40-NLESS)=CBUF((K-1)*NLEN+173:(K-1)*NLEN+184-NLESS)
          KPTR=0
          CALL GBYTEC(CBUF,KPTR(3),(K-1)*NLEN*8+25*8,3*8)
          KPDS(18)=1
          CALL GBYTEC(CPDS,KPDS(4),7*8,8)
          CALL FI632(CPDS,KPTR,KPDS,KRET)
          DO I=1,LPDSP
            IP=IPDSP(I)
            LT=LT+ABS(JPDS(IP)-KPDS(IP))
          ENDDO
        ENDIF
C  SEARCH FOR GDS REQUEST
        IF(LT.EQ.0.AND.LGDSP.GT.0) THEN
          CGDS=CHAR(0)
          CGDS(1:42)=CBUF((K-1)*NLEN+54:(K-1)*NLEN+95)
          NLESS=MAX(320-NLEN,0)
          CGDS(43:178-NLESS)=CBUF((K-1)*NLEN+185:(K-1)*NLEN+320-NLESS)
          KPTR=0
          CALL FI633(CGDS,KPTR,KGDS,KRET)
          DO I=1,LGDSP
            IP=IGDSP(I)
            LT=LT+ABS(JGDS(IP)-KGDS(IP))
          ENDDO
        ENDIF
C  SEARCH FOR ENS REQUEST
        IF(LT.EQ.0.AND.LENSP.GT.0) THEN
          NLESS=MAX(172-NLEN,0)
          CPDS(41:100-NLESS)=CBUF((K-1)*NLEN+113:(K-1)*NLEN+172-NLESS)
          CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,CPDS)
          DO I=1,LENSP
            IP=IENSP(I)
            LT=LT+ABS(JENS(IP)-KENS(IP))
          ENDDO
        ENDIF
C  RETURN IF REQUEST IS FOUND
        IF(LT.EQ.0) THEN
          CALL GBYTEC(CBUF,LSKIP,(K-1)*NLEN*8,4*8)
          CALL GBYTEC(CBUF,LGRIB,(K-1)*NLEN*8+20*8,4*8)
          IF(LPDSP.EQ.0) THEN
            CPDS=CHAR(0)
            CPDS(1:28)=CBUF((K-1)*NLEN+26:(K-1)*NLEN+53)
            NLESS=MAX(184-NLEN,0)
            CPDS(29:40-NLESS)=CBUF((K-1)*NLEN+173:(K-1)*NLEN+184-NLESS)
            KPTR=0
            CALL GBYTEC(CBUF,KPTR(3),(K-1)*NLEN*8+25*8,3*8)
            KPDS(18)=1
            CALL GBYTEC(CPDS,KPDS(4),7*8,8)
            CALL FI632(CPDS,KPTR,KPDS,KRET)
          ENDIF
          IF(LGDSP.EQ.0) THEN
            CGDS=CHAR(0)
            CGDS(1:42)=CBUF((K-1)*NLEN+54:(K-1)*NLEN+95)
            NLESS=MAX(320-NLEN,0)
            CGDS(43:178-NLESS)=CBUF((K-1)*NLEN+185:(K-1)*NLEN+320-NLESS)
            KPTR=0
            CALL FI633(CGDS,KPTR,KGDS,KRET)
          ENDIF
          IF(KPDS(23).EQ.2.AND.LENSP.EQ.0) THEN
            NLESS=MAX(172-NLEN,0)
            CPDS(41:100-NLESS)=CBUF((K-1)*NLEN+113:(K-1)*NLEN+172-NLESS)
            CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,45,CPDS)
          ENDIF
          IRET=0
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
