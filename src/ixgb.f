C> @file
C> @brief This subprogram makes one index record.
C> @author Mark iredell @date 1995-10-31

C> Byte 001-004: Bytes to skip in data file before grib message.
C> Byte 005-008: Bytes to skip in message before pds.
C> Byte 009-012: Bytes to skip in message before gds (0 if no gds).
C> Byte 013-016: Bytes to skip in message before bms (0 if no bms).
C> Byte 017-020: Bytes to skip in message before bds.
C> Byte 021-024: Bytes total in the message.
C> Byte 025-025: Grib version number.
C> Byte 026-053: Product definition section (pds).
C> Byte 054-095: Grid definition section (gds) (or nulls).
C> Byte 096-101: First part of the bit map section (bms) (or nulls).
C> Byte 102-112: First part of the binary data section (bds).
C> Byte 113-172: (optional) bytes 41-100 of the pds.
C> Byte 173-184: (optional) bytes 29-40 of the pds.
C> Byte 185-320: (optional) bytes 43-178 of the gds.
C>
C> Program history log:
C> - Mark iredell 1995-10-31
C> - Mark iredell 1996-10-31 Augmented optional definitions to byte 320.
C> - Mark iredell 2001-06-05 Apply linux port by ebisuzaki.
C>
C> @param[in] LUGB Integer logical unit of input grib file.
C> @param[in] LSKIP Integer number of bytes to skip before grib message.
C> @param[in] LGRIB Integer number of bytes in grib message.
C> @param[in] NLEN Integer length of each index record in bytes.
C> @param[in] NNUM Integer index record number to make.
C> @param[out] MLEN Integer actual valid length of index record.
C> @param[out] CBUF Character*1 (mbuf) buffer to receive index data.
C>
C> @author Mark iredell @date 1995-10-31
C-----------------------------------------------------------------------
      SUBROUTINE IXGB(LUGB,LSKIP,LGRIB,NLEN,NNUM,MLEN,CBUF)
      CHARACTER CBUF(*)
      PARAMETER(LINDEX=112,MINDEX=320)
      PARAMETER(IXSKP=0,IXSPD=4,IXSGD=8,IXSBM=12,IXSBD=16,IXLEN=20,
     &          IXVER=24,IXPDS=25,IXGDS=53,IXBMS=95,IXBDS=101,
     &          IXPDX=112,IXPDW=172,IXGDX=184)
      PARAMETER(MXSKP=4,MXSPD=4,MXSGD=4,MXSBM=4,MXSBD=4,MXLEN=4,
     &          MXVER=1,MXPDS=28,MXGDS=42,MXBMS=6,MXBDS=11,
     &          MXPDX=60,MXPDW=12,MXGDX=136)
      CHARACTER CBREAD(MINDEX),CINDEX(MINDEX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  INITIALIZE INDEX RECORD AND READ GRIB MESSAGE
      MLEN=LINDEX
      CINDEX=CHAR(0)
      CALL SBYTEC(CINDEX,LSKIP,8*IXSKP,8*MXSKP)
      CALL SBYTEC(CINDEX,LGRIB,8*IXLEN,8*MXLEN)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PUT PDS IN INDEX RECORD
      ISKPDS=8
      IBSKIP=LSKIP
      IBREAD=ISKPDS+MXPDS
      CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
      IF(LBREAD.NE.IBREAD) RETURN
      CINDEX(IXVER+1)=CBREAD(8)
      CALL SBYTEC(CINDEX,ISKPDS,8*IXSPD,8*MXSPD)
      CALL GBYTEC(CBREAD,LENPDS,8*ISKPDS,8*3)
      CALL GBYTEC(CBREAD,INCGDS,8*ISKPDS+8*7+0,1)
      CALL GBYTEC(CBREAD,INCBMS,8*ISKPDS+8*7+1,1)
      ILNPDS=MIN(LENPDS,MXPDS)
      CINDEX(IXPDS+1:IXPDS+ILNPDS)=CBREAD(ISKPDS+1:ISKPDS+ILNPDS)
      ISKTOT=ISKPDS+LENPDS
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PUT PDS EXTENSION IN INDEX RECORD
      IF(LENPDS.GT.MXPDS) THEN
        ISKPDW=ISKPDS+MXPDS
        ILNPDW=MIN(LENPDS-MXPDS,MXPDW)
        IBSKIP=LSKIP+ISKPDW
        IBREAD=ILNPDW
        CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
        IF(LBREAD.NE.IBREAD) RETURN
        CINDEX(IXPDW+1:IXPDW+ILNPDW)=CBREAD(1:ILNPDW)
        ISKPDX=ISKPDS+(MXPDS+MXPDW)
        ILNPDX=MIN(LENPDS-(MXPDS+MXPDW),MXPDX)
        IBSKIP=LSKIP+ISKPDX
        IBREAD=ILNPDX
        CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
        IF(LBREAD.NE.IBREAD) RETURN
        CINDEX(IXPDX+1:IXPDX+ILNPDX)=CBREAD(1:ILNPDX)
        MLEN=MAX(MLEN,IXPDW+ILNPDW,IXPDX+ILNPDX)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PUT GDS IN INDEX RECORD
      IF(INCGDS.NE.0) THEN
        ISKGDS=ISKTOT
        IBSKIP=LSKIP+ISKGDS
        IBREAD=MXGDS
        CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
        IF(LBREAD.NE.IBREAD) RETURN
        CALL SBYTEC(CINDEX,ISKGDS,8*IXSGD,8*MXSGD)
        CALL GBYTEC(CBREAD,LENGDS,0,8*3)
        ILNGDS=MIN(LENGDS,MXGDS)
        CINDEX(IXGDS+1:IXGDS+ILNGDS)=CBREAD(1:ILNGDS)
        ISKTOT=ISKGDS+LENGDS
        IF(LENGDS.GT.MXGDS) THEN
          ISKGDX=ISKGDS+MXGDS
          ILNGDX=MIN(LENGDS-MXGDS,MXGDX)
          IBSKIP=LSKIP+ISKGDX
          IBREAD=ILNGDX
          CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
          IF(LBREAD.NE.IBREAD) RETURN
          CINDEX(IXGDX+1:IXGDX+ILNGDX)=CBREAD(1:ILNGDX)
          MLEN=MAX(MLEN,IXGDX+ILNGDX)
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PUT BMS IN INDEX RECORD
      IF(INCBMS.NE.0) THEN
        ISKBMS=ISKTOT
        IBSKIP=LSKIP+ISKBMS
        IBREAD=MXBMS
        CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
        IF(LBREAD.NE.IBREAD) RETURN
        CALL SBYTEC(CINDEX,ISKBMS,8*IXSBM,8*MXSBM)
        CALL GBYTEC(CBREAD,LENBMS,0,8*3)
        ILNBMS=MIN(LENBMS,MXBMS)
        CINDEX(IXBMS+1:IXBMS+ILNBMS)=CBREAD(1:ILNBMS)
        ISKTOT=ISKBMS+LENBMS
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  PUT BDS IN INDEX RECORD
      ISKBDS=ISKTOT
      IBSKIP=LSKIP+ISKBDS
      IBREAD=MXBDS
      CALL BAREAD(LUGB,IBSKIP,IBREAD,LBREAD,CBREAD)
      IF(LBREAD.NE.IBREAD) RETURN
      CALL SBYTEC(CINDEX,ISKBDS,8*IXSBD,8*MXSBD)
      CALL GBYTEC(CBREAD,LENBDS,0,8*3)
      ILNBDS=MIN(LENBDS,MXBDS)
      CINDEX(IXBDS+1:IXBDS+ILNBDS)=CBREAD(1:ILNBDS)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  STORE INDEX RECORD
      MLEN=MIN(MLEN,NLEN)
      NSKIP=NLEN*(NNUM-1)
      CBUF(NSKIP+1:NSKIP+MLEN)=CINDEX(1:MLEN)
      CBUF(NSKIP+MLEN+1:NSKIP+NLEN)=CHAR(0)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
