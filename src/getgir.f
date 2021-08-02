C> @file
C> @brief Read a grib index file and return its index contents.
C> @author Mark Iredell @date 1995-10-31

C> Read a grib file and return its index contents.
C> The index buffer returned contains index records with the internal format:
C> - byte 001-004: bytes to skip in data file before grib message.
C> - byte 005-008: bytes to skip in message before pds.
C> - byte 009-012: bytes to skip in message before gds (0 if no gds).
C> - byte 013-016: bytes to skip in message before bms (0 if no bms).
C> - byte 017-020: bytes to skip in message before bds.
C> - byte 021-024: bytes total in the message.
C> - byte 025-025: grib version number.
C> - byte 026-053: product definition section (pds).
C> - byte 054-095: grid definition section (gds) (or nulls).
C> - byte 096-101: first part of the bit map section (bms) (or nulls).
C> - byte 102-112: first part of the binary data section (bds).
C> - byte 113-172: (optional) bytes 41-100 of the pds.
C> - byte 173-184: (optional) bytes 29-40 of the pds.
C> - byte 185-320: (optional) bytes 43-178 of the gds.
C>
C> Program history log:
C> - Mark Iredell 1995-10-31
C> - Mark Iredell 1996-10-31 Augmented optional definitions to byte 320.
C>
C> @param[in] lugb integer unit of the unblocked grib file.
C> @param[in] msk1 integer number of bytes to search for first message.
C> @param[in] msk2 integer number of bytes to search for other messages.
C> @param[in] mnum integer number of index records to skip (usually 0).
C> @param[in] mbuf integer length of cbuf in bytes.
C> @param[out] cbuf character*1 (mbuf) buffer to receive index data.
C> @param[out] nlen integer length of each index record in bytes.
C> @param[out] nnum integer number of index records
C> (=0 if no grib messages are found).
C> @param[out] iret integer return code.
C> - 0: all ok.
C> - 1: cbuf too small to hold index data.
C>
C> @note Subprogram can be called from a multiprocessing environment.
C> Do not engage the same logical unit from more than one processor.
C>
C> @author Mark Iredell @date 1995-10-31
C-----------------------------------------------------------------------
      SUBROUTINE GETGIR(LUGB,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRET)
      CHARACTER CBUF(MBUF)
      PARAMETER(MINDEX=320)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SEARCH FOR FIRST GRIB MESSAGE
      ISEEK=0
      CALL SKGB(LUGB,ISEEK,MSK1,LSKIP,LGRIB)
      IF(LGRIB.GT.0.AND.MINDEX.LE.MBUF) THEN
        CALL IXGB(LUGB,LSKIP,LGRIB,MINDEX,1,NLEN,CBUF)
      ELSE
        NLEN=MINDEX
      ENDIF
      DO M=1,MNUM
        IF(LGRIB.GT.0) THEN
          ISEEK=LSKIP+LGRIB
          CALL SKGB(LUGB,ISEEK,MSK2,LSKIP,LGRIB)
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  MAKE AN INDEX RECORD FOR EVERY GRIB RECORD FOUND
      NNUM=0
      IRET=0
      DOWHILE(IRET.EQ.0.AND.LGRIB.GT.0)
        IF(NLEN*(NNUM+1).LE.MBUF) THEN
          NNUM=NNUM+1
          CALL IXGB(LUGB,LSKIP,LGRIB,NLEN,NNUM,MLEN,CBUF)
          ISEEK=LSKIP+LGRIB
          CALL SKGB(LUGB,ISEEK,MSK2,LSKIP,LGRIB)
        ELSE
          IRET=1
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
