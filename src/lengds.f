C> @file
C> @brief GIven a grid description section (in w3fi63 format),
C> return its size in terms of number of data points.
C> @author Mark Iredell @date 1996-07-19

C> Program history log:
C> - Mark Iredell 1996-07-19
C>
C> @param[in] KGDS Integer (200) gds parameters in w3fi63 format.
C> @return LENGDS Integer size of grid.
C>
C> @author Mark Iredell @date 1996-07-19
C-----------------------------------------------------------------------
      FUNCTION LENGDS(KGDS)
      INTEGER KGDS(200)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SPECIAL CASE OF STAGGERED ETA
      IF(KGDS(1).EQ.201) THEN
        LENGDS=KGDS(7)*KGDS(8)-KGDS(8)/2
C  SPECIAL CASE OF FILLED ETA
      ELSEIF(KGDS(1).EQ.202) THEN
        LENGDS=KGDS(7)*KGDS(8)
C  SPECIAL CASE OF THINNED WAFS
      ELSEIF(KGDS(19).EQ.0.AND.KGDS(20).NE.255) THEN
        LENGDS=KGDS(21)
C  GENERAL CASE
      ELSE
        LENGDS=KGDS(2)*KGDS(3)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
