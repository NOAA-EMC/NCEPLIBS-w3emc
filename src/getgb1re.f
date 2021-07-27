C> @file
C> @brief Reads and unpacks a grib message.
C> @author Mark Iredell @date 1995-10-31

C> Reads and unpacks a grib message.
C>
C> PROGRAM HISTORY LOG:
C>   Mark Iredell 1995-10-31
C>   Y. Zhu 1997-02-11 Included probability and cluster arguments.
C>
C> @param[in] LUGB Integer unit of the unblocked grib data file.
C> @param[in] LSKIP Integer number of bytes to skip.
C> @param[in] LGRIB Integer number of bytes to read.
C> @param[out] KF Integer number of data points unpacked.
C> @param[out] KPDS Integer (200) unpacked pds parameters.
C> @param[out] KGDS Integer (200) unpacked gds parameters.
C> @param[out] KENS Integer (200) unpacked ensemble pds parms.
C> @param[out] KPROB Integer (2) probability ensemble parms.
C> @param[out] XPROB Real (2) probability ensemble parms.
C> @param[out] KCLUST Integer (16) cluster ensemble parms.
C> @param[out] KMEMBR Integer (8) cluster ensemble parms.
C> @param[out] LB Logical*1 (kf) unpacked bitmap if present.
C> @param[out] F Real (kf) unpacked data.
C> @param[out] IRET Integer return code.
C> - 0 All ok.
C> - 97 Error reading grib file.
C> - other w3fi63 grib unpacker return code.
C>
C> @note There is no protection against unpacking too much data.
C> Subprogram can be called from a multiprocessing environment.
C> Do not engage the same logical unit from more than one processor.
C> This subprogram is intended for private use by getgb routines only.
C>
C> @author Mark Iredell @date 1995-10-31
C-----------------------------------------------------------------------
      SUBROUTINE GETGB1RE(LUGB,LSKIP,LGRIB,KF,KPDS,KGDS,KENS,
     &                    KPROB,XPROB,KCLUST,KMEMBR,LB,F,IRET)
      INTEGER KPDS(200),KGDS(200),KENS(200)
      INTEGER KPROB(2),KCLUST(16),KMEMBR(80)
      REAL XPROB(2)
      LOGICAL*1 LB(*)
      REAL F(*)
      INTEGER KPTR(200)
      CHARACTER GRIB(LGRIB)*1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  READ GRIB RECORD
      CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  UNPACK GRIB RECORD
      IF(LREAD.EQ.LGRIB) THEN
        CALL W3FI63(GRIB,KPDS,KGDS,LB,F,KPTR,IRET)
        IF(IRET.EQ.0.AND.KPDS(23).EQ.2) THEN
          CALL PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,86,GRIB(9))
        ENDIF
      ELSE
        IRET=97
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RETURN NUMBER OF POINTS
      IF(IRET.EQ.0) THEN
        KF=KPTR(10)
      ELSE
        KF=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
