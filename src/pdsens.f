C> @file
C> @brief Packs grib pds extension 41- for ensemble.
C> @author Zoltan Toth & Mark Iredell @date 1995-03-14

C> Packs brib pds extension starting on byte 41 for ensemble
c> forecast products. For format of pds extension, see nmc office note 38.
C>
C> Program history log:
C> - Zoltan Toth and Mark Iredell 1995-03-14
C> - Mark Iredell 1995-10-31 Removed saves and prints.
C> - Richard Wobus 1998-09-28 Corrected member entry, blank all unused fields.
C> - Mark Iredell 2001-06-05 Apply linux port by Ebisuzaki.
C>
C> @param[in] KENS (5) Bytes 41-45 (general section, always present.)
C> @param[in] KPROB (2) Bytes 46-47 (probability section, present only if needed).
C> @param[in] XPROB (2) Bytes 48-51&52-55 (probability section, if needed.).
C> @param[in] KCLUST (16) Bytes 61-76 (clustering section, if needed.).
C> @param[in] KMEMBR (80) Bytes 77-86 (cluster membership section, if needed.).
C> @param[in] ILAST Last byte to be packed (if greater or equal to first byte
C> in any of four sections above, whole section is packed).
C> @param[out] MSGA - Full pds section, including new ensemble extension.
C>
C> @note Use pdseup() for unpacking pds ensemble extension.
c> subprogram can be called from a multiprocessing environment.
C>
C> @author Zoltan Toth & Mark Iredell @date 1995-03-14
      SUBROUTINE PDSENS(KENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,MSGA)
      INTEGER KENS(5),KPROB(2),KCLUST(16),KMEMBR(80)
      DIMENSION XPROB(2)
      CHARACTER*1 MSGA(100)
      IF(ILAST.LT.41) THEN
      GO TO 333
      ENDIF
C    PACKING IS DONE IN FOUR SECTIONS ENDING AT BYTE IL
      IF(ILAST.GE.41) IL=45
      IF(ILAST.GE.46) IL=55
      IF(ILAST.GE.61) IL=76
      IF(ILAST.GE.77) IL=86
          do i=42,il
            CALL SBYTEC(MSGA, 0, i*8, 8)
          enddo
C    CHANGING THE NUMBER OF BYTES (FIRST THREE BYTES IN PDS)
      CALL SBYTEC(MSGA, IL, 0,24)
C    PACKING FIRST SECTION (GENERAL INTORMATION SECTION)
      IF(IL.GE.45) CALL SBYTESC(MSGA,KENS,40*8,8,0,5)
C    PACKING 2ND SECTION (PROBABILITY SECTION)
      IF(IL.GE.55) THEN
          CALL SBYTESC(MSGA,KPROB,45*8,8,0,2)
      CALL W3FI01(LW)
      CALL W3FI76(XPROB(1),IEXP,IMANT,8*LW)
      CALL SBYTEC(MSGA,IEXP,47*8,8)
      CALL SBYTEC(MSGA,IMANT,48*8,24)
      CALL W3FI76(XPROB(2),IEXP,IMANT,8*LW)
      CALL SBYTEC(MSGA,IEXP,51*8,8)
      CALL SBYTEC(MSGA,IMANT,52*8,24)
      ENDIF
C    PACKING 3RD SECTION (CLUSTERING INFORMATION)
      IF(IL.GE.76) CALL SBYTESC(MSGA,KCLUST,60*8,8,0,16)
C    PACKING 4TH SECTION (CLUSTER MEMBERSHIP)
      IF(IL.GE.86) CALL SBYTESC(MSGA,KMEMBR,76*8,1,0,80)
C
 333  CONTINUE
      RETURN
      END
