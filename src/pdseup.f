C> @file
C> @brief Unpacks grib pds extension 41- for ensemble.
C> @author Zoltan Toth and Mark Iredell @date DATE: 1995-03-14

C> Unpacks grib pds extension starting on byte 41 for ensemble
C> forecast products. for format of pds extension, see nmc office note 38
C>
C> Program history log:
C> - Zoltan Toth and Mark Iredell 1995-03-14
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C> - Richard Wobus 1998-09-28 Corrected member extraction.
C> - Mark Iredell 2001-06-05 Apply linux port by ebisuzaki.
C>
C> @param[out] KENS (5) Bytes 41-45 (general section, always present.).
C> @param[out] KPROB (2) Bytes 46-47 (probability section, present only if neede.
C> @param[out] XPROB (2) Bytes 48-51&52-55 (probability section, if needed.).
C> @param[out] KCLUST (16) Bytes 61-76 (clustering section, if needed.).
C> @param[out] KMEMBR (80) Bytes 77-86 (cluster membership section, if needed.).
C> @param[in] ILAST Last byte to be unpacked (if greater/equal to first byte
C> in any of four sections below, whole section is packed).
C> @param[in] MSGA Full pds section, including new ensemble extension.
C>
C> @note Use pdsens() for packing pds ensemble extension.
C> Subprogram can be called from a multiprocessing environment.
C>
      SUBROUTINE PDSEUP(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,MSGA)
      INTEGER KENS(5),KPROB(2),KCLUST(16),KMEMBR(80)
      DIMENSION XPROB(2)
      CHARACTER*1 MSGA(100)
C    CHECKING TOTAL NUMBER OF BYTES IN PDS (IBYTES)
      CALL GBYTEC(MSGA, IBYTES, 0,24)
      IF(ILAST.GT.IBYTES) THEN
C      ILAST=IBYTES
      GO TO 333
      ENDIF
      IF(ILAST.LT.41) THEN
      GO TO 333
      ENDIF
C    UNPACKING FIRST SECTION (GENERAL INFORMATION)
      CALL GBYTESC(MSGA,KENS,40*8,8,0,5)
C    UNPACKING 2ND SECTION (PROBABILITY SECTION)
      IF(ILAST.GE.46) THEN
      CALL GBYTESC(MSGA,KPROB,45*8,8,0,2)
C
      CALL GBYTEC (MSGA,JSGN,47*8,1)
      CALL GBYTEC (MSGA,JEXP,47*8+1,7)
      CALL GBYTEC (MSGA,IFR,47*8+8,24)
      XPROB(1)=(-1)**JSGN*IFR*16.**(JEXP-70)
C
      CALL GBYTEC (MSGA,JSGN,51*8,1)
      CALL GBYTEC (MSGA,JEXP,51*8+1,7)
      CALL GBYTEC (MSGA,IFR,51*8+8,24)
      XPROB(2)=(-1)**JSGN*IFR*16.**(JEXP-70)
      ENDIF
C
C    UNPACKING 3RD SECTION (CLUSTERING INFORMATION)
      IF(ILAST.GE.61) CALL GBYTESC(MSGA,KCLUST,60*8,8,0,16)
C  UNPACKING 4TH SECTION (CLUSTERMEMBERSHIP INFORMATION)
      IF(ILAST.GE.77) CALL GBYTESC(MSGA,KMEMBR,76*8,1,0,80)
C
 333  CONTINUE
      RETURN
      END
