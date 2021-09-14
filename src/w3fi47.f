C> @file
C> @brief Convert label to off. no. 85 format (cray)
C> @author Ralph Jones @date 1985-07-31

C> Converts a office note 85 label in IBM370 format
C> to office note 85 cray format. All EBCDIC characters are
C> converted to ASCII. Converts binary or coded label.
C>
C> Program history log:
C> - Ralph Jones 1985-07-31
C> - Ralph Jones 1989-10-24 Convert to cray cft77 fortran
C> - Boi Vuong 2002-10-15 Replaced function ichar with mova2i
C>
C> @param[in] ILABEL 4 words (32 bytes) characters are in EBCDIc or
C> binary.
C> @param[out] NLABEL 4 words (32 bytes), characters are in ASCII or
C> binary.
C>
C> @author Ralph Jones @date 1985-07-31
      SUBROUTINE W3FI47(ILABEL,NLABEL)
C
       CHARACTER*1    ILABEL(32)
       CHARACTER*1    NLABEL(32)
C
C          TEST FOR CODED LABEL, IF SO, CONVERT ALL CHARACTERS
C          TEST FOR EBCDIC C, 195 IN DECIMAL
C
           IF (MOVA2I(ILABEL(7)).EQ.195) THEN
C
             CALL AEA(NLABEL(1),ILABEL(1),32)
C
           ELSE
C
C          BINARY LABEL, CONVERT BYTES 1-8, 21-30 TO ASCII
C
             CALL AEA(NLABEL(1),ILABEL(1),8)
C
C          MOVE BYTES 9 TO 20
C
             DO 10 I = 9,20
               NLABEL(I) = ILABEL(I)
 10          CONTINUE
C
C          CONVERT WASHINGTON TO ASCII
C
             CALL AEA(NLABEL(21),ILABEL(21),10)
C
C          TEST BYTES 31 AND 32 FOR BINARY ZERO, IF NOT ZERO
C          CONVERT TO ASCII
C
             IF (MOVA2I(ILABEL(31)).EQ.0) THEN
               NLABEL(31) = CHAR(0)
             ELSE
               CALL AEA(NLABEL(31),ILABEL(31),1)
             ENDIF
C
             IF (MOVA2I(ILABEL(32)).EQ.0) THEN
               NLABEL(32) = CHAR(0)
             ELSE
               CALL AEA(NLABEL(32),ILABEL(32),1)
             ENDIF
C
           ENDIF
C
         RETURN
       END
