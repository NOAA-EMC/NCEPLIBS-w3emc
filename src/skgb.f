C> @file
C> @brief Search for next grib message.
C> @author Mark Iredell @date 1993-11-22

C> This subprogram searches a file for the next grib 1 message.
C> A grib 1 message is identified by its indicator section, i.e.
C> an 8-byte sequence with 'grib' in bytes 1-4 and 1 in byte 8.
C> If found, the length of the message is decoded from bytes 5-7.
C> The search is done over a given section of the file.
C> The search is terminated if an eof or i/o error is encountered.
C>
C> Program history log:
C> - Mark Iredell 1993-11-22
C> - Mark Iredell 1995-10-31 Add call to baread.
C> - Mark Iredell 1997-03-14 Check for '7777'.
C> - Stephen Gilbert 2001-12-05 Modified to also look for grib2 messages.
C>
C> @param[in] LUGB Integer logical unit of input grib file.
C> @param[in] ISEEK Integer number of bytes to skip before search.
C> @param[in] MSEEK Integer maximum number of bytes to search.
C> @param[out] LSKIP Integer number of bytes to skip before message.
C> @param[out] LGRIB Integer number of bytes in message (0 if not found).
C>
C> @author Mark Iredell @date 1993-11-22
C-----------------------------------------------------------------------
      SUBROUTINE SKGB(LUGB,ISEEK,MSEEK,LSKIP,LGRIB)
      PARAMETER(LSEEK=128)
      CHARACTER Z(LSEEK)
      CHARACTER Z4(4)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LGRIB=0
      KS=ISEEK
      KN=MIN(LSEEK,MSEEK)
      KZ=LSEEK
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOOP UNTIL GRIB MESSAGE IS FOUND
      DOWHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
C  READ PARTIAL SECTION
        CALL BAREAD(LUGB,KS,KN,KZ,Z)
        KM=KZ-8+1
        K=0
C  LOOK FOR 'GRIB...1' IN PARTIAL SECTION
        DOWHILE(LGRIB.EQ.0.AND.K.LT.KM)
          CALL GBYTEC(Z,I4,(K+0)*8,4*8)
          CALL GBYTEC(Z,I1,(K+7)*8,1*8)
          IF(I4.EQ.1196575042.AND.(I1.EQ.1.OR.I1.EQ.2)) THEN
C  LOOK FOR '7777' AT END OF GRIB MESSAGE
            IF (I1.EQ.1) CALL GBYTEC(Z,KG,(K+4)*8,3*8)
            IF (I1.EQ.2) CALL GBYTEC(Z,KG,(K+12)*8,4*8)
            CALL BAREAD(LUGB,KS+K+KG-4,4,K4,Z4)
            IF(K4.EQ.4) THEN
              CALL GBYTEC(Z4,I4,0,4*8)
              IF(I4.EQ.926365495) THEN
C  GRIB MESSAGE FOUND
                LSKIP=KS+K
                LGRIB=KG
              ENDIF
            ENDIF
          ENDIF
          K=K+1
        ENDDO
        KS=KS+KM
        KN=MIN(LSEEK,ISEEK+MSEEK-KS)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
