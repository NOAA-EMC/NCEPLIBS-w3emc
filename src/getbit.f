C> @file
C> @brief Compute number of bits and round field.
C> @author Mark Iredell @date 1992-10-31
C>
C> The number of bits required to pack a given field
C> the field is rounded off to the decimal scaling for packing.
C> The minimum and maximum rounded field values are also returned.
C> For particular binary and decimal scalings is computed.
C> Grib bitmap masking for valid data is optionally used.
C>
C> Program history log:
C> - Mark Iredell 1996-09-16
C>
C> @param[in] IBM Integer bitmap flag (=0 for no bitmap).
C> @param[in] IBS Integer binary scaling (e.g. ibs=3 to round field
C> to nearest eighth value).
C> @param[in] IDS Integer decimal scaling (e.g. ids=3 to round field
C> to nearest milli-value) (note that ids and ibs can both be nonzero,
C> e.g. ids=1 and ibs=1 rounds to the nearest twentieth).
C> @param[in] LEN Integer length of the field and bitmap.
C> @param[in] MG Integer (LEN) bitmap if ibm=1 (0 to skip, 1 to keep).
C> @param[in] G Real (LEN) field.
C> @param[out] GROUND Real (LEN) field rounded to decimal and binary scaling
C> (set to zero where bitmap is 0 if ibm=1).
C> @param[out] GMIN Real minimum valid rounded field value.
C> @param[out] GMAX Real maximum valid rounded field value.
C> @param[out] NBIT Integer number of bits to pack.
C>
C> @note CRAY FORTRAN
C>
C> @author Mark Iredell @date 1992-10-31
      SUBROUTINE GETBIT(IBM,IBS,IDS,LEN,MG,G,GROUND,GMIN,GMAX,NBIT)
      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      S=2.**IBS*10.**IDS
      IF(IBM.EQ.0) THEN
        GROUND(1)=NINT(G(1)*S)/S
        GMAX=GROUND(1)
        GMIN=GROUND(1)
        DO I=2,LEN
          GROUND(I)=NINT(G(I)*S)/S
          GMAX=MAX(GMAX,GROUND(I))
          GMIN=MIN(GMIN,GROUND(I))
        ENDDO
      ELSE
        I1=1
        DOWHILE(I1.LE.LEN.AND.MG(I1).EQ.0)
          I1=I1+1
        ENDDO
        IF(I1.LE.LEN) THEN
          DO I=1,I1-1
            GROUND(I)=0.
          ENDDO
          GROUND(I1)=NINT(G(I1)*S)/S
          GMAX=GROUND(I1)
          GMIN=GROUND(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GROUND(I)=NINT(G(I)*S)/S
              GMAX=MAX(GMAX,GROUND(I))
              GMIN=MIN(GMIN,GROUND(I))
            ELSE
              GROUND(I)=0.
            ENDIF
          ENDDO
        ELSE
          DO I=1,LEN
            GROUND(I)=0.
          ENDDO
          GMAX=0.
          GMIN=0.
        ENDIF
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF BITS
      NBIT=LOG((GMAX-GMIN)*S+0.9)/LOG(2.)+1.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
