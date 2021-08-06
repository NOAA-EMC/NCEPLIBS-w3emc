C> @file
C> @brief The number of bits required to pack a given field.
C> @author Mark Iredell @date 1992-10-31

C> The number of bits required to pack a given field
c> at a particular decimal scaling is computed using the field range.
C> The field is rounded off to the decimal scaling for packing.
C> The minimum and maximum rounded field values are also returned.
C> Grib bitmap masking for valid data is optionally used.
C>
C> Program history log:
C> - Mark Iredell 1992-10-31
C>
C> @param[in] ibm integer bitmap flag (=0 for no bitmap).
c> @param[in] ids integer decimal scaling
c> (e.g. ids=3 to round field to nearest milli-value).
c> @param[in] len integer length of the field and bitmap.
c> @param[in] mg integer (len) bitmap if ibm=1 (0 to skip, 1 to keep).
c> @param[in] g real (len) field.
c> @param[out] ground real (len) field rounded to decimal scaling
c> (set to zero where bitmap is 0 if ibm=1).
c> @param[out] gmin real minimum valid rounded field value.
c> @param[out] gmax real maximum valid rounded field value.
c> @param[out] nbit integer number of bits to pack.
C>
C> @author Mark Iredell @date 1992-10-31
      SUBROUTINE GTBITS(IBM,IDS,LEN,MG,G,GROUND,GMIN,GMAX,NBIT)
      DIMENSION MG(LEN),G(LEN),GROUND(LEN)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      DS=10.**IDS
      IF(IBM.EQ.0) THEN
        GROUND(1)=NINT(G(1)*DS)/DS
        GMAX=GROUND(1)
        GMIN=GROUND(1)
        DO I=2,LEN
          GROUND(I)=NINT(G(I)*DS)/DS
          GMAX=MAX(GMAX,GROUND(I))
          GMIN=MIN(GMIN,GROUND(I))
        ENDDO
      ELSE
        I1=ISRCHNE(LEN,MG,1,0)
        IF(I1.GT.0.AND.I1.LE.LEN) THEN
          DO I=1,I1-1
            GROUND(I)=0.
          ENDDO
          GROUND(I1)=NINT(G(I1)*DS)/DS
          GMAX=GROUND(I1)
          GMIN=GROUND(I1)
          DO I=I1+1,LEN
            IF(MG(I).NE.0) THEN
              GROUND(I)=NINT(G(I)*DS)/DS
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
      NBIT=LOG((GMAX-GMIN)*DS+0.9)/LOG(2.)+1.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
