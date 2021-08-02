C> @file
C> @brief Read a grib index file and return its contents.
C> @author Mark Iredell @date 1995-10-31

C> Read a grib index file and return its contents.
C> Version 1 of the index file has the following format:
C> 81-byte s.lord header with 'gb1ix1' in columns 42-47 followed by
C> 81-byte header with number of bytes to skip before index records,
C> number of bytes in each index record, number of index records,
C> and grib file basename written in format ('ix1form:',3i10,2x,a40).
C> Each following index record corresponds to a grib message
C> and has the internal format:
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
C> - Mark Iredell  1995-10-31
C> - Mark Iredell  1996-10-31 Augmented optional definitions to byte 320.
C>
C> @param[in] lugi integer unit of the unblocked grib index file.
C> @param[in] mnum integer number of index records to skip (usually 0).
C> @param[in] mbuf integer length of cbuf in bytes.
C> @param[out] cbuf character*1 (mbuf) buffer to receive index data.
C> @param[out] nlen integer length of each index record in bytes.
C> @param[out] nnum integer number of index records.
C> @param[out] iret integer return code.
C> - 0: all ok.
C> - 1: cbuf too small to hold index buffer.
C> - 2: error reading index file buffer.
C> - 3: error reading index file header.
C>
C> @note Subprogram can be called from a multiprocessing environment.
C> Do not engage the same logical unit from more than one processor.
C>
C> @author Mark Iredell @date 1995-10-31
C-----------------------------------------------------------------------
      SUBROUTINE GETGI(LUGI,MNUM,MBUF,CBUF,NLEN,NNUM,IRET)
      CHARACTER CBUF(MBUF)
      CHARACTER CHEAD*162
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      NLEN=0
      NNUM=0
      IRET=3
      CALL BAREAD(LUGI,0,162,LHEAD,CHEAD)
      IF(LHEAD.EQ.162.AND.CHEAD(42:47).EQ.'GB1IX1') THEN
        READ(CHEAD(82:162),'(8X,3I10,2X,A40)',IOSTAT=IOS) NSKP,NLEN,NNUM
        IF(IOS.EQ.0) THEN
          NSKP=NSKP+MNUM*NLEN
          NNUM=NNUM-MNUM
          NBUF=NNUM*NLEN
          IRET=0
          IF(NBUF.GT.MBUF) THEN
            NNUM=MBUF/NLEN
            NBUF=NNUM*NLEN
            IRET=1
          ENDIF
          IF(NBUF.GT.0) THEN
            CALL BAREAD(LUGI,NSKP,NBUF,LBUF,CBUF)
            IF(LBUF.NE.NBUF) IRET=2
          ENDIF
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
